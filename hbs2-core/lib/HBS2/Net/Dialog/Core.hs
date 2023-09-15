{-# Language AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE ConstraintKinds       #-}
-- {-# LANGUAGE OverloadedLists #-}

-- {-# LANGUAGE CPP                   #-}
-- {-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE RankNTypes            #-}
-- {-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE TupleSections         #-}
-- {-# LANGUAGE TypeApplications      #-}
-- {-# LANGUAGE TypeFamilies          #-}


module HBS2.Net.Dialog.Core where

import Codec.Serialise
import Control.Arrow
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Except (Except, ExceptT(..), runExcept, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.State.Class as State
import Control.Monad.State.Strict as StateStrict (evalState, evalStateT, runStateT, StateT(..))
import Control.Monad.Trans.Class
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import Data.Bits
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Constraint (Dict(..))
import Data.Foldable as F
import Data.Function
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Product.Fields ()
import Data.Generics.Sum.Constructors
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict as Map
import Data.Maybe
import Data.Proxy
import Data.String.Conversions as X (cs)
import Data.Text (Text)
import Data.Typeable
import Data.Word
import GHC.Generics ((:*:) (..), Generic (..), K1 (..), M1 (..))
import GHC.Generics qualified as Generics
import GHC.TypeLits
import Lens.Micro.Platform
import Streaming
import System.Random.MWC
import UnliftIO.STM

import HBS2.Net.Dialog.Helpers.List

type Frames = Frames' ByteString
newtype Frames' a = Frames { unFrames :: [a] }
  deriving stock (Generic,Eq)
  deriving newtype (Functor, Foldable, Semigroup, Monoid
      -- , IsList
      )


instance Show Frames where
  show (Frames xs) = "Frames " <> show (BS.length <$> xs)
      -- <> " " <> show (fmap B16.encode xs)
      <> " " <> (show . fmap (limitSize 42)) xs

    where
      limitSize n as = bool as (BS.take (n-3) as <> "...") (BS.length as > n)

framesBodyPart :: Traversal' Frames [ByteString]
framesBodyPart = #unFrames . tailAfterP (== "")

tailAfterP :: forall a . (a -> Bool) -> Traversal' [a] [a]
tailAfterP p focus = fix \go -> \case
    x:xs -> (x :) <$> bool go focus (p x) xs
    xs -> pure xs

---

encodeFrames :: Frames -> ByteString
-- encodeFrames :: Foldable t => t ByteString -> ByteString
encodeFrames = F.toList >>> BSL.toStrict . runPut . \case

    []   -> pure ()

    xss -> flip fix xss \go -> \case
        [] -> pure ()
        bs:xs -> do
            let (flip shiftR 1 -> n1, ns) = unfoldSizeBytes @Word64 . flip shiftL 1 . fromIntegral . BS.length $ bs

            putWord8 $ n1
                & bool (sbit 7) id (List.null xs)
                & bool (sbit 6) id (List.null ns)

            forM_ (markMore ns) \(n, isMoreBytesInSize) -> do
                putWord8 $ n & bool (zbit 7) (sbit 7) isMoreBytesInSize

            putByteString bs

            go xs

  where

    markMore as = zip as ((True <$ List.drop 1 as) <> [False])

    unfoldSizeBytes :: (Bits n, Integral n) => n -> (Word8, [Word8])
    unfoldSizeBytes = (\(a NE.:| as) -> (a, as)) . NE.unfoldr \w ->
        ( (flip shiftR 1 . flip shiftL 1 . fromIntegral) w
        , let w' = shiftR w 7
           in bool Nothing (Just w') (w' > 0)
        )

decodeFrames :: MonadError String m => ByteString -> m Frames
decodeFrames = \case
    "" -> pure mempty

    bs' -> (bs' &) $ BSL.fromStrict >>> either (throwError . view _3) (pure . Frames . view _3)
        <$> runGetOrFail do

          fix \go -> do

              j <- getWord8

              bsize <-
                  flip fix (6, j) \fu (b, j') -> do
                      let n = (fromIntegral . clearLeftBits (8-b)) j'
                      if tbit b j'
                            then (n +) . flip shiftL b <$> (fu . (7, ) =<< getWord8)
                            else pure n

              bs <- getByteString bsize

              let moreFrames = tbit 7 j

              if moreFrames
                then (bs :) <$> go
                else pure [bs]

  where
    clearLeftBits n = flip shiftR n . flip shiftL n
    tbit = flip testBit


devDialogCore :: IO ()
devDialogCore = do
    display (Frames [])
    display (Frames [""])
    display (Frames [BS.replicate 32 0x55])
    display (Frames [BS.replicate 32 0x55, ""])
    display (Frames [BS.replicate 32 0x55, "\3\3"])
    display (Frames [BS.replicate 33 0x55, "\3\3"])
    display (Frames [BS.replicate 63 0x55])
    display (Frames [BS.replicate 64 0x55])
    -- display (Frames [BS.replicate 65 0x55])
    display (Frames ["\8\8\8","\4\4\4"])
    display (Frames ["","\1"])
  where
    display a = do
        putStrLn . cs . show . B16.encode . encodeFrames $ a
        putStrLn ""




sbit :: (Bits n) => Int -> n -> n
sbit = flip setBit

zbit :: (Bits n) => Int -> n -> n
zbit = flip clearBit

---

decodeFramesFail :: (MonadFail m) => ByteString -> m Frames
decodeFramesFail = errorToFail . decodeFrames

---

errorToFailT :: (MonadFail m) => ExceptT String m a -> m a
errorToFailT = either fail pure <=< runExceptT

errorToFail :: MonadFail m => Except String a -> m a
errorToFail = either fail pure . runExcept

errorShowToFail :: (MonadFail m, Show s) => Except s a -> m a
errorShowToFail = either (fail . show) pure . runExcept

--

data CallerID = CallerID
  { unCallerIDV :: Word8
  , unCallerID :: Word32
  }
  deriving stock (Generic, Eq, Ord)

instance Serialise CallerID

newCallerID :: forall m. MonadIO m => m CallerID
newCallerID = liftIO $ withSystemRandomST \g ->
    CallerID <$> uniformM g <*> uniformM g

---

newtype CallerHandler m = CallerHandler
  { unCallerHandler :: Frames -> m ()
  }

newtype CallerEnv m = CallerEnv
  { unCallerEnv :: TVar (Map CallerID (CallerHandler m)) }

newCallerEnv :: MonadIO m => m (CallerEnv m')
newCallerEnv = CallerEnv <$> newTVarIO mempty

---

newtype RequestResponseHandler m = RequestResponseHandler
  { unRequestResponseHandler :: Frames -> m ()
  }

newtype RequestResponseEnv m = RequestResponseEnv
  { unRequestResponseEnv :: TVar (Map RequestID (RequestResponseHandler m))
  }

newRequestResponseEnv :: MonadIO m => m (RequestResponseEnv m')
newRequestResponseEnv =
    RequestResponseEnv <$> newTVarIO mempty

---

data DClient m p i = DClient
  { clientCallerEnv :: CallerEnv m
  , clientSendProtoRequest :: p -> Frames -> m ()
  , clientGetKnownPeers :: m [(p, i)]
  }

---

newtype RequestID = RequestID { unRequestID :: Word32 }
  deriving stock (Generic, Eq, Ord)
  deriving newtype (Serialise, Num, Enum)
  -- deriving  via TODO_GenericVLQ Put Get

data RequestResult
  = RequestDone
  -- | RequestSuccessIncomplete
  | RequestTimeout
  | RequestFailure ResponseStatus Frames
  | RequestErrorBadResponse BadResponse Frames
  deriving stock (Generic, Eq, Show)

data BadResponse
  = ResponseErrorNoResponseHeader
  | ResponseInsufficientFrames
  | ResponseParseError DeserialiseFailure
  deriving stock (Generic, Eq, Show)

---

setupCallerEnv :: MonadIO m => CallerEnv m' -> CallerID -> CallerHandler m' -> m ()
setupCallerEnv env callerID repHandleEnv =
    (atomically . modifyTVar' (unCallerEnv env))
        (at callerID ?~ repHandleEnv)

clearCallerEnv :: MonadIO m => CallerEnv m' -> CallerID -> m ()
clearCallerEnv env callerID =
    (atomically . modifyTVar' (unCallerEnv env))
        (at callerID .~ Nothing)

findCallerHandler :: MonadIO m => CallerEnv m' -> CallerID -> m (Maybe (CallerHandler m'))
findCallerHandler CallerEnv{..} callerID =
    readTVarIO unCallerEnv <&> preview (ix callerID)

---

setupRepHandler :: MonadIO m => RequestResponseEnv m' -> RequestID -> RequestResponseHandler m' -> m ()
setupRepHandler RequestResponseEnv{..} requestID useResponse =
    (atomically . modifyTVar' unRequestResponseEnv)
        (at requestID ?~ useResponse)

clearRepHandler :: MonadIO m => RequestResponseEnv m' -> RequestID -> m ()
clearRepHandler RequestResponseEnv{..} requestID =
    (atomically . modifyTVar' unRequestResponseEnv)
        (at requestID .~ Nothing)

findRepHandler :: MonadIO m => RequestResponseEnv m' -> RequestID -> m (Maybe (RequestResponseHandler m'))
findRepHandler RequestResponseEnv{..} requestID =
    readTVarIO unRequestResponseEnv <&> preview (ix requestID)

---

data DialogRequestEnv m p pd = DialogRequestEnv
    { dreqEnvPeer :: p
    , dreqEnvGetPeerData :: m pd
    }

-- data DialogRequestError
--   = DialogRequestFailure String
--   deriving stock (Show)
-- instance Exception DialogRequestError

---

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
type DApp m = Frames -> (Frames -> m ()) -> m ()

mkDApp ::
    forall spec ctx m io.
    ( Monad m
    , Monad io
    , HasHandler m (NamedSpec spec) ctx
    , HasHandler io (NamedSpec spec) ctx
    )
    => Proxy (NamedSpec spec)
    -> Ctx ctx
    -> (forall x. m x -> DialHandlerT io x)
    -> spec (ModeServerT m)
    -> DApp io
mkDApp p ctx ntToDialHandlerTn hd = routeDialogRequest rr
  where
    rr :: DialogRequestRouter io
    rr = route p ctx
        $ hoistDialogWithContext p (Proxy @ctx) ntToDialHandlerTn
          hd

type DialogReplyHandler m = (Frames -> m ()) -> m ()

type DialogRequestRouter (m :: Type -> Type) =
     DialogRequestRoutes (DialogReplyHandler m)

data DialogRequestRoutes (h :: Type)
  = DialogRequestPaths (Map ByteString (DialogRequestRoutes h))
  | DialogRequestPreparse (Frames -> Either Text (DialogRequestRoutes h, Frames))
  | DialogRequestEndpoint h
  deriving (Generic, Functor)

instance Semigroup (DialogRequestRoutes h) where
  (<>) a b = case (a, b) of
      (DialogRequestPaths p1, DialogRequestPaths p2) ->
          DialogRequestPaths (p1 <> p2)
      _ -> b

-- instance Monoid (DialogRequestRoutes h) where
--   mempty = DialogRequestPaths mempty

dialogRequestRoutes
    :: ListBuilder
          ([ByteString], Frames -> Either Text ((Frames -> m ()) -> m (), Frames))
    -> DialogRequestRouter m
dialogRequestRoutes = List.foldl1' (<>)
      . fmap toPaths
      . over (traverse . _2) (DialogRequestPreparse . (fmap . fmap) (over _1 DialogRequestEndpoint))
      . buildList
  where
    toPaths :: ([ByteString], DialogRequestRoutes ((Frames -> m ()) -> m ()))
       -> DialogRequestRoutes (DialogReplyHandler m)
    toPaths = fix \go (ps, rr) -> case ps of
        [] -> rr
        [p] -> DialogRequestPaths (Map.singleton p rr)
        p:px' -> DialogRequestPaths (Map.singleton p (go (px', rr)))

hand :: Monad m => a -> b -> ListBuilderT m (a, b)
hand = curry li

handconv :: (Monad m, Monad m', Serialise req, Serialise resp)
    => a
    -> Text
    -> (req -> ExceptT ResponseStatus m resp)
    -> ListBuilderT m' (a, Frames -> Either Text ((Frames -> m ()) -> m (), Frames))
handconv path msg h =
    hand path $ processReply msg h

---

processReply :: forall m m' req resp .
    ( Monad m
    , Serialise req
    , Serialise resp
    , m' ~ ExceptT ResponseStatus m
    )
    => Text
    -> (req -> m' resp)
    -> Frames
    -> Either Text ((Frames -> m ()) -> m (), Frames)
processReply msg h = runExcept . runStateT do
    flip runReply . h <$> cutFrameDecode msg

runReply ::
    ( Monad m
    , Serialise a
    )
    => (Frames -> m r)
    -> ExceptT ResponseStatus m a
    -> m r
runReply reply =
        either
            (\e -> reply (Frames [serialiseS (ResponseHeader e 0)]))
            (\a -> reply (Frames [serialiseS (ResponseHeader (ResponseStatus Success200 "") 0)
                                  , serialiseS a
                                  ])
            )
    <=< runExceptT

---

dpath :: Text -> [ByteString] -> Frames
dpath path = Frames . (cs path :)

---

addEnvelope :: Monoid a => [a] -> Frames' a -> Frames' a
addEnvelope en = over #unFrames ((en <> [mempty]) <>)

splitEnvelope :: (Monoid a, Eq a) => Frames' a -> ([a], Frames' a)
splitEnvelope = fmap (Frames . List.drop 1) . List.break (== mempty) . unFrames

data ResponseHeader = ResponseHeader
  { respStatus :: ResponseStatus
  , respSeqNumber :: Int
  }
  deriving (Generic, Show, Eq)

instance Serialise ResponseHeader

data ResponseStatus = ResponseStatus
  { responseStatusCode :: ResponseStatusCode
  , responseStatusMessage :: Text
  }
  deriving (Generic, Show, Eq)

instance Serialise ResponseStatus

data ResponseStatusCode
  = Success200
  | SuccessNoContent204
  | SuccessMore
  | BadRequest400
  | Forbidden403
  | NotFound404
  deriving (Generic, Show, Eq)

instance Serialise ResponseStatusCode

routerSignature :: Word8
routerSignature = 1

routeDialogRequest :: forall m .
       Monad m
    => DialogRequestRouter m
    -> Frames
    -> (Frames -> m ())
    -> m ()
routeDialogRequest router frames rawReplyToPeer = do
    -- error $ show router
    erun <- pure $ runExcept $ flip evalStateT req do

        signature <- cutFrameDecode
            (ResponseStatus BadRequest400 "No signature in request")

        when (signature /= routerSignature) $ throwError
            (ResponseStatus BadRequest400 "Wrong signature in request")

        path <- cutFrameOr
            (ResponseStatus BadRequest400 "No path in request")

        lift . ExceptT . pure
              -- Если не может разобрать параметры запроса,
              -- то самим ответить этому пиру '404'
              -- . left (ResponseStatus BadRequest400)
              . travel (BS8.split '/' path) router
              -- передать оставшуюся часть запроса в хэндлер
            =<< get

    case erun of
        Left rs -> replyToPeer (Frames [serialiseS (ResponseHeader rs 0)])
        Right go ->
            -- передать хэндлеру продолжение чтобы ответить этому пиру
            go replyToPeer

  where
    (backPath, req) = splitEnvelope frames

    replyToPeer :: Frames -> m ()
    replyToPeer = rawReplyToPeer . over #unFrames (backPath <>)

travel :: ()
    => [ByteString]
    -> DialogRequestRouter m
    -> Frames
    -> Either ResponseStatus ((Frames -> m ()) -> m ())
travel path'' router'' = evalStateT $ flipfix2 path'' router''
    \go path -> \case
        DialogRequestPaths kv -> case path of
            step:path' ->
                maybe
                    (throwError (ResponseStatus BadRequest400 "Path not found"))
                    (go path')
                    (Map.lookup step kv)
            _ -> throwError (ResponseStatus BadRequest400 "Path not found (too long)")
        DialogRequestPreparse hfx ->
            go path =<< StateT (left (ResponseStatus BadRequest400) . hfx)
        DialogRequestEndpoint ep -> pure ep

flipfix2 :: a -> b -> ((a -> b -> c) -> (a -> b -> c)) -> c
flipfix2 a b f = fix f a b

cutFrameDecode :: (Serialise b, MonadState Frames m, MonadError e m) => e -> m b
cutFrameDecode e =
    State.gets unFrames >>= \case
        x:xs ->
            (either (const (throwError e)) pure . deserialiseOrFailS) x
                <* State.put (Frames xs)
        _ -> throwError e

cutFrameDecode'
    :: (Serialise b, MonadState Frames m, MonadError (Maybe DeserialiseFailure) m)
    => m b
cutFrameDecode' =
    State.gets unFrames >>= \case
        x:xs ->
            (either (throwError . Just) pure . deserialiseOrFailS) x
                <* State.put (Frames xs)
        _ -> throwError Nothing

cutFrameOr :: (MonadState (Frames' b) m, MonadError e m) => e -> m b
cutFrameOr e =
    State.gets unFrames >>= \case
        x:xs -> x <$ State.put (Frames xs)
        _ -> throwError e

serialiseS :: Serialise a => a -> ByteString
serialiseS = BSL.toStrict . serialise

deserialiseOrFailS :: Serialise a => ByteString -> Either DeserialiseFailure a
deserialiseOrFailS = deserialiseOrFail . BSL.fromStrict

fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM ma = maybe ma pure

fromJustThrowError :: MonadError e m => e -> Maybe a -> m a
fromJustThrowError = fromMaybeM . throwError



------------------------------------------
--- Type-level specification -------------
------------------------------------------


data ReqCBOR (a :: Type)
data SingleAck
data SingleRespCBOR (a :: Type)
data StreamingRespCBOR (a :: Type)

data NamedSpec (spec :: Type -> Type)

class DialMode mode where
    type mode &- spec :: Type
infixl 0 &-

data (path :: k) &/ (a :: Type)
    deriving (Typeable)
infixr 4 &/

type path &// a = path &/ NamedSpec a
infixr 4 &//

---

data ModePlain
instance DialMode ModePlain where
    type ModePlain &- spec = spec

---

data ModeServerT (m :: Type -> Type)

instance DialMode (ModeServerT m) where
    type ModeServerT m &- spec = HandlerD spec m

class HasHandler m spec ctx where
  type HandlerD spec (m' :: Type -> Type) :: Type

  route ::
       Proxy spec
    -> Ctx ctx
    -> HandlerD spec (DialHandlerT m)
    -> DialogRequestRouter m

  hoistDialogWithContext
      :: Proxy spec
      -> Proxy ctx
      -> (forall x. m x -> n x)
      -> HandlerD spec m
      -> HandlerD spec n

data EmptyCX -- '[]
data Ctx ctx where
    EmptyCtx :: Ctx EmptyCX
    -- (:&.) :: x -> Ctx xs -> Ctx (x ': xs)
-- infixr 5 :&.

-- hoistTRouter :: forall t m n .
--       (MonadTrans t, Monad m, Monad n, m ~ t n)
--       => (forall a . m a -> n a)
--       -> DialogRequestRouter m
--       -> DialogRequestRouter n
-- hoistTRouter nt = fmap nt'
--   where
--     nt' :: ((x -> m y) -> m y)
--         -> ((x -> n y) -> n y)
--     nt' xtmy_tmy =  nt . xtmy_tmy . fmap lift

hoistTRouter :: forall m n .
      (Monad m, Monad n)
      => (forall a . m a -> n a)
      -> (forall a . n a -> m a)
      -> DialogRequestRouter m
      -> DialogRequestRouter n
hoistTRouter ntf ntb = fmap nt'
  where
    nt' :: ((x -> m y) -> m y)
        -> ((x -> n y) -> n y)
    nt' xtmy_tmy =  ntf . xtmy_tmy . fmap ntb


type DialHandlerIO a = DialHandlerT IO a
newtype DialHandlerT m a = DialHandlerT { runDialHandlerT :: ExceptT ResponseStatus m a }
  deriving
    ( Generic, Functor, Applicative, Monad
    , MonadIO
    , MonadTrans
    , MonadError ResponseStatus
    -- , MonadUnliftIO
    -- , MonadThrow, MonadCatch, MonadMask
    )

---

instance (KnownSymbol path, HasHandler m spec ctx) => HasHandler m (path &/ spec) ctx where
    type HandlerD (path &/ spec) m = HandlerD spec m

    route _ ctx h = DialogRequestPaths $
        Map.singleton (cs (symbolVal (Proxy @path))) (route (Proxy @spec) ctx h)

    hoistDialogWithContext _ = hoistDialogWithContext (Proxy @spec)

---

instance
  ( Serialise a
  , Typeable a
  , HasHandler m spec ctx
  ) =>
  HasHandler m (ReqCBOR a &/ spec) ctx where
    type HandlerD (ReqCBOR a &/ spec) m = a -> HandlerD spec m

    route _ ctx (ha :: a -> HandlerD spec (DialHandlerT m)) =
        DialogRequestPreparse \fx -> do
            (a, fx')
                <- runExcept
                  $ flip runStateT fx
                  $ cutFrameDecode ((cs . show . typeRep) (Proxy @a))
            pure (route (Proxy @spec) ctx (ha a), fx')

    hoistDialogWithContext _ pc nt s = hoistDialogWithContext (Proxy @spec) pc nt . s

---

instance
  ( Applicative m
  ) =>
  HasHandler m SingleAck ctx where
    type HandlerD SingleAck m = m ()

    route _ _ctx _mx =
        DialogRequestEndpoint \reply -> do
            reply (Frames [serialiseS (ResponseHeader (ResponseStatus SuccessNoContent204 "") 0)])

    hoistDialogWithContext _ _ nt hdlM = nt hdlM

---

instance
  ( Monad m
  , Serialise a
  ) =>
  HasHandler m (SingleRespCBOR a) ctx where
    type HandlerD (SingleRespCBOR a) m = m a

    route _ _ctx ma =
        DialogRequestEndpoint \reply -> do

            ea <- runExceptT $ runDialHandlerT ma

            case ea of
                Left e -> reply $ Frames [ serialiseS e ]
                Right a -> reply $ Frames
                    [ serialiseS (ResponseHeader (ResponseStatus Success200 "") 0)
                    , serialiseS (a :: a)
                    ]

    hoistDialogWithContext _ _ nt hdlM = nt hdlM

---

instance
  ( Serialise a
  ) =>
  HasHandler m (StreamingRespCBOR a) ctx where
    type HandlerD (StreamingRespCBOR a) m = Stream (Of a) m ()

    route = undefined

    -- hoistDialogWithContext = undefined

---

type GServerConstraints spec m =
  ( GToProduct (Rep (spec (ModeServerT m))) ~ HandlerD (GToProduct (Rep (spec ModePlain))) m
  , GProduct (Rep (spec (ModeServerT m)))
  )

class GServer (spec :: Type -> Type) (m :: Type -> Type) where
  gServerProof :: Dict (GServerConstraints spec m)

instance
  ( GToProduct (Rep (spec (ModeServerT m))) ~ HandlerD (GToProduct (Rep (spec ModePlain))) m
  , GProduct (Rep (spec (ModeServerT m)))
  ) => GServer spec m where
  gServerProof = Dict


instance
  ( HasHandler m (GToProduct (Rep (spec ModePlain))) ctx
  -- , HasHandler m (GToProduct (Rep (spec (ModeServerT m)))) ctx
  -- , GProduct (Rep (spec ModePlain))
  , forall q . Generic (spec (ModeServerT q))
  , forall q . GServer spec q
  ) =>
  HasHandler m (NamedSpec spec) ctx where
    type HandlerD (NamedSpec spec) m = spec (ModeServerT m)

    route ::
           Proxy (NamedSpec spec)
        -> Ctx ctx
        -> spec (ModeServerT (DialHandlerT m))
        -> DialogRequestRouter m
    route _ ctx spec =
      case gServerProof @spec @(DialHandlerT m) of
          Dict -> route (Proxy @(GToProduct (Rep (spec ModePlain)))) ctx (toProduct spec)

    hoistDialogWithContext
      :: forall n. Proxy (NamedSpec spec)
      -> Proxy ctx
      -> (forall x. m x -> n x)
      -> spec (ModeServerT m)
      -> spec (ModeServerT n)
    hoistDialogWithContext _ pctx nat dl =
      case (gServerProof @spec @m, gServerProof @spec @n) of
        (Dict, Dict) ->
            fromProduct dlN
          where
            dlM :: HandlerD (GToProduct (Rep (spec ModePlain))) m =
              toProduct dl
            dlN :: HandlerD (GToProduct (Rep (spec ModePlain))) n =
              hoistDialogWithContext (Proxy @(GToProduct (Rep (spec ModePlain)))) pctx nat dlM


toProduct :: (Generic (spec mode), GProduct (Rep (spec mode)))
    => spec mode -> GToProduct (Rep (spec mode))
toProduct = gtoProduct . Generics.from

fromProduct
    :: (Generic (spec mode), GProduct (Rep (spec mode)))
    => GToProduct (Rep (spec mode)) -> spec mode
fromProduct = Generics.to . gfromProduct

instance
  ( HasHandler m speca ctx
  , HasHandler m specb ctx
  ) =>
  HasHandler m (GP speca specb) ctx where
    type HandlerD (GP speca specb) m = GP (HandlerD speca m) (HandlerD specb m)
    route _ ctx (GP speca specb) =
            route (Proxy @speca) ctx speca
        <>  route (Proxy @specb) ctx specb

    hoistDialogWithContext _ pc nt (GP speca specb) =
      GP
        (hoistDialogWithContext (Proxy @speca) pc nt speca)
        (hoistDialogWithContext (Proxy @specb) pc nt specb)

data GP a b = GP a b

class GProduct f where
    type GToProduct (f :: Type -> Type)
    gtoProduct   :: f p -> GToProduct f
    gfromProduct :: GToProduct f -> f p

instance (GProduct l, GProduct r) => GProduct (l :*: r) where
    type GToProduct (l :*: r) = GP (GToProduct l) (GToProduct r)
    gtoProduct   (l :*: r)  = GP (gtoProduct l) (gtoProduct r)
    gfromProduct (GP l r) = gfromProduct l :*: gfromProduct r

instance GProduct f => GProduct (M1 i c f) where
    type GToProduct (M1 i c f) = GToProduct f
    gtoProduct   = gtoProduct . unM1
    gfromProduct = M1 . gfromProduct

instance GProduct (K1 i c) where
    type GToProduct (K1 i c) = c
    gtoProduct = unK1
    gfromProduct = K1
