{-# LANGUAGE StrictData #-}
-- {-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE UndecidableInstances #-}
module HBS2.Net.Dialog.Core where

-- import Data.ByteString.Builder as Builder
-- import Data.ByteString.Builder.Internal as Builder
-- import GHC.IsList
import Codec.Serialise
import Control.Arrow
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Except (Except(..), ExceptT(..), runExcept, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.State.Class as State
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans.Class
import Control.Monad.Writer qualified as W
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import Data.Bits
import Data.Bool
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Sized as BAS
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable
import Data.Foldable as F
import Data.Function
import Data.Generics.Labels
import Data.Generics.Product.Fields
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict as Map
import Data.Maybe
import Data.String.Conversions as X (cs)
import Data.Text (Text)
import Data.Word
import GHC.Exts
import GHC.Generics (Generic)
import Lens.Micro.Platform
import Numeric.Natural
import System.Random.MWC
import UnliftIO.Exception
import UnliftIO.STM

-- import Prettyprinter
-- import HBS2.Base58
import Data.ByteString.Base16 qualified as B16

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
                & (bool (sbit 7) id (List.null xs))
                & (bool (sbit 6) id (List.null ns))

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

              size <-
                  flip fix (6, j) \fu (b, j') -> do
                      let n = (fromIntegral . clearLeftBits (8-b)) j'
                      if (tbit b j')
                            then (n +) . flip shiftL b <$> (fu . (7, ) =<< getWord8)
                            else pure n

              bs <- getByteString size

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
    CallerID <$> (uniformM g) <*> (uniformM g)

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
  | RequestFailure ResponseStatusCode Frames
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
        (at callerID .~ Just repHandleEnv)

clearCallerEnv :: MonadIO m => CallerEnv m' -> CallerID -> m ()
clearCallerEnv env callerID =
    (atomically . modifyTVar' (unCallerEnv env))
        (at callerID .~ Nothing)

findCallerHandler :: MonadIO m => CallerEnv m' -> CallerID -> m (Maybe (CallerHandler m'))
findCallerHandler CallerEnv{..} callerID =
    (atomically (readTVar unCallerEnv)) <&> (preview (ix callerID))

---

setupRepHandler :: MonadIO m => RequestResponseEnv m' -> RequestID -> RequestResponseHandler m' -> m ()
setupRepHandler RequestResponseEnv{..} requestID useResponse =
    (atomically . modifyTVar' unRequestResponseEnv)
        (at requestID .~ Just useResponse)

clearRepHandler :: MonadIO m => RequestResponseEnv m' -> RequestID -> m ()
clearRepHandler RequestResponseEnv{..} requestID =
    (atomically . modifyTVar' unRequestResponseEnv)
        (at requestID .~ Nothing)

findRepHandler :: MonadIO m => RequestResponseEnv m' -> RequestID -> m (Maybe (RequestResponseHandler m'))
findRepHandler RequestResponseEnv{..} requestID =
    (atomically (readTVar unRequestResponseEnv)) <&> (preview (ix requestID))

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

newtype DialogRequestRouter m = DialogRequestRouter
  { unDialogRequestRouter ::
        Map [ByteString]  -- path

        -- handler :: Input -> m (Either ErrorMessage (HowToReply -> ResponseContinuation))
            (Frames -> Either Text ((Frames -> m ()) -> m ()))
  }

  deriving (Semigroup, Monoid)

dialogRequestRoutes
    :: ListBuilder
          ([ByteString], Frames -> Either Text ((Frames -> m ()) -> m ()))
    -> DialogRequestRouter m
dialogRequestRoutes = DialogRequestRouter . Map.fromList . buildList

hand :: Monad m => a -> b -> ListBuilderT m (a, b)
hand = curry li

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
  | SuccessMore
  | BadRequest400
  | Forbidden403
  | NotFound404
  deriving (Generic, Show, Eq)

instance Serialise ResponseStatusCode

routerSignature :: Word8
routerSignature = 1

routeDialogRequest :: forall m p pd .
       Monad m
    => DialogRequestRouter m
    -> DialogRequestEnv m p pd
    -> (Frames -> m ())
    -> Frames
    -> m ()
routeDialogRequest router drenv rawReplyToPeer frames = do
    erun <- pure $ runExcept $ flip evalStateT req do

        signature <- cutFrameDecode
            (ResponseStatus BadRequest400 "No signature in request")

        when (signature /= routerSignature) $ throwError
            (ResponseStatus BadRequest400 "Wrong signature in request")

        route <- cutFrameOr
            (ResponseStatus BadRequest400 "No route in request")

        h <- fromJustThrowError
            (ResponseStatus NotFound404 "Route not found")
            (unDialogRequestRouter router ^? ix (BS8.split '/' route))

        lift . ExceptT . pure
            -- Если не может разобрать параметры запроса,
            -- то самим ответить этому пиру '404'
            . left (ResponseStatus BadRequest400)
            . h
            -- передать оставшуюся часть запроса в хэндлер
          =<< get

    case erun of
        Left rs -> replyToPeer (Frames [serialiseS (ResponseHeader rs 0)])
        Right run ->
            -- передать хэндлеру продолжение чтобы ответить этому пиру
            run replyToPeer

  where
    (backPath, req) = splitEnvelope frames

    replyToPeer :: Frames -> m ()
    replyToPeer = rawReplyToPeer . over #unFrames (backPath <>)

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

