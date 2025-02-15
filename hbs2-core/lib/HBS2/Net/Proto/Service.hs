{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module HBS2.Net.Proto.Service
  ( module HBS2.Net.Proto.Service
  , module HBS2.Net.Proto.Types
  ) where

import HBS2.Actors.Peer
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Types
import HBS2.Prelude.Plated

import Codec.Serialise
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Data.Kind
import Data.List qualified as List
import GHC.TypeLits
-- import Lens.Micro.Platform
import UnliftIO
import UnliftIO.Async
import UnliftIO qualified as UIO
import UnliftIO (TVar,TQueue,atomically)
import System.Random (randomIO)
import Data.Hashable
import Data.Word
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
-- import Control.Exception (bracket_)
import Control.Monad.Trans.Cont
import System.IO.Unsafe (unsafePerformIO)

type family Input a  :: Type
type family Output a :: Type

-- FIXME: wrap-those-instances
type instance Input () = ()
type instance Output () = ()

class (Monad m, Serialise (Output a), Serialise (Input a)) => HandleMethod m a where
  handleMethod :: Input a -> m (Output a)

type family AllHandlers m (xs :: [Type]) :: Constraint where
    AllHandlers m '[]       = ()
    AllHandlers m (x ': xs) = (HandleMethod m x, AllHandlers m xs)

data SomeHandler m = forall a . HandleMethod m a => SomeHandler ( Input a -> m (Output a) )

class Monad m => EnumAll (xs :: [Type]) t m where
    enumMethods :: [t]

instance (Monad m, HandleMethod m ()) => EnumAll '[] (Int, SomeHandler m) m where
  enumMethods = [(0, SomeHandler @m @() (\_ -> pure ()))]

instance (Monad m, EnumAll xs (Int, SomeHandler m) m, HandleMethod m x) => EnumAll (x ': xs) (Int, SomeHandler m) m where
  enumMethods = (0, wtf) : shift (enumMethods @xs @(Int, SomeHandler m) @m)
    where
      wtf = SomeHandler @m @x (handleMethod @m @x)
      shift = map (\(i, h) -> (i + 1, h))

instance Monad m => HandleMethod m () where
  handleMethod _ = pure ()

data ServiceError =
    ErrorMethodNotFound
  | ErrorInvalidRequest
  | ErrorInvalidResponse
  deriving stock (Eq,Ord,Generic,Show,Typeable)

instance Serialise ServiceError

data ServiceProto api e =
    ServiceRequest   { reqNum :: Word64, reqData :: ByteString }
  | ServiceResponse  { reqNum :: Word64, reqResp :: Either ServiceError ByteString }
  deriving stock (Generic,Show)

instance Serialise (ServiceProto api e)

dispatch :: forall api e m . ( EnumAll api (Int, SomeHandler m) m
                             , MonadIO m
                             , HasProtocol e (ServiceProto api e)
                             )
         => ServiceProto api e
         -> m (ServiceProto api e)

dispatch (ServiceResponse n _) = do
  pure $ ServiceResponse n (Left ErrorInvalidRequest)

dispatch (ServiceRequest rn lbs) = do
  let ha = enumMethods @api @(Int, SomeHandler m) @m
  let (n, bss) = deserialise @(Int, ByteString) lbs
  maybe1 (List.lookup n ha) methodNotFound $ \(SomeHandler fn) -> do
    case deserialiseOrFail bss of
      Left{} ->  pure $ ServiceResponse rn (Left ErrorInvalidRequest)
      Right v -> ServiceResponse rn . Right . serialise <$> fn v
  where
    methodNotFound = pure (ServiceResponse rn (Left ErrorMethodNotFound))

type family FindMethodIndex (n :: Nat) (x :: Type) (xs :: [Type]) :: Maybe Nat where
  FindMethodIndex _ x '[] = 'Nothing
  FindMethodIndex n x (x ': xs) = 'Just n
  FindMethodIndex n x (y ': xs) = FindMethodIndex (n + 1) x xs

type family FromJust (a :: Maybe k) :: k where
  FromJust ('Just a) = a
  FromJust 'Nothing = TypeError ('Text "Element not found")

findMethodIndex :: forall x xs. KnownNat (FromJust (FindMethodIndex 0 x xs)) => Integer
findMethodIndex = natVal (Proxy :: Proxy (FromJust (FindMethodIndex 0 x xs)))


makeRequest :: forall api method e . ( KnownNat (FromJust (FindMethodIndex 0 method api))
                                     , Serialise (Input method)
                                     )
            => Word64 -> Input method -> ServiceProto api e
makeRequest rnum input = ServiceRequest rnum (serialise (fromIntegral idx :: Int, serialise input))
  where
    idx = findMethodIndex @method @api

rnumnum :: TVar Word64
rnumnum = unsafePerformIO (newTVarIO 1)
{-# NOINLINE rnumnum #-}

makeRequestR :: forall api method e m . ( KnownNat (FromJust (FindMethodIndex 0 method api))
                                       , Serialise (Input method)
                                       , MonadIO m
                                       )
            => Input method -> m (ServiceProto api e)
makeRequestR input = do

  t <- getTimeCoarse <&> round @_ @Word64  . realToFrac

  rnum  <- atomically do
              n <- readTVar rnumnum
              modifyTVar' rnumnum succ
              pure (fromIntegral $ hash (n+t))

  pure $ ServiceRequest rnum (serialise (fromIntegral idx :: Int, serialise input))
  where
    idx = findMethodIndex @method @api


runWithContext :: r -> ReaderT r m a -> m a
runWithContext co m = runReaderT m co

makeServer :: forall api e m proto . ( MonadIO m
                                     , EnumAll api (Int, SomeHandler m) m
                                     , Response e (ServiceProto api e) m
                                     , HasProtocol e proto
                                     , HasDeferred proto e m
                                     , Pretty (Peer e)
                                     , proto ~ ServiceProto api e
                                     )
                => ServiceProto api e
                -> m ()

makeServer msg = do
  deferred @proto $ dispatch @api @e msg >>= response

data ServiceCaller api e =
  ServiceCaller
  { callPeer    :: Peer e
  , callInQ     :: TQueue (ServiceProto api e)
  , callWaiters :: TVar (HashMap Word64 (TQueue (ServiceProto api e)))
  }

makeServiceCaller :: forall api e m . MonadIO m => Peer e -> m (ServiceCaller api e)
makeServiceCaller p = ServiceCaller p <$> UIO.newTQueueIO
                                      <*> UIO.newTVarIO mempty

runServiceClient :: forall api e m . ( MonadIO m
                                     , MonadUnliftIO m
                                     , HasProtocol e (ServiceProto api e)
                                     -- FIXME: remove-this-debug-shit
                                     , Show (Peer e)
                                     , Pretty (Peer e)
                                     , PeerMessaging e
                                     , HasOwnPeer e m
                                     , HasFabriq e m
                                     , HasTimeLimits e (ServiceProto api e) m
                                     )
                  => ServiceCaller api e
                  -> m ()

runServiceClient caller = do
  flip runContT pure do
    p <- ContT $ withAsync (runProto @e [ makeResponse (makeClient @api caller) ])
    link p
    forever $ lift do
      req <- getRequest caller
      request @e (callPeer caller) req

data Endpoint e m = forall (api :: [Type]) . ( HasProtocol e (ServiceProto api e)
                                             , HasTimeLimits e (ServiceProto api e) m
                                             , PeerMessaging e
                                             , Pretty (Peer e)
                                             )
  => Endpoint (ServiceCaller api e)

runServiceClientMulti :: forall e m . ( MonadIO m
                                      , MonadUnliftIO m
                                      -- FIXME: remove-this-debug-shit
                                      , Show (Peer e)
                                      , Pretty (Peer e)
                                      , PeerMessaging e
                                      , HasOwnPeer e m
                                      , HasFabriq e m
                                      )
                  => [ Endpoint e m ]
                  -> m ()

runServiceClientMulti endpoints = do
  proto <- async $ runProto @e [ makeResponse @e (makeClient x) | (Endpoint x) <- endpoints ]

  waiters <- forM endpoints $ \(Endpoint caller) -> async $ forever do
                req <- getRequest caller
                request @e (callPeer caller) req


  r <- UIO.waitAnyCatchCancel $ proto : waiters

  either UIO.throwIO (const $ pure ()) (snd r)

notifyServiceCaller :: forall api e m . MonadIO m
             => ServiceCaller api e
             -> ServiceProto api e
             -> m ()

notifyServiceCaller caller msg = do
  waiter <- UIO.readTVarIO (callWaiters caller) <&> HashMap.lookup (reqNum msg)
  maybe1 waiter none $ \q -> atomically $ UIO.writeTQueue q msg


getRequest :: forall api e m . MonadIO m
             => ServiceCaller api e
             -> m (ServiceProto api e)

getRequest caller = atomically $ UIO.readTQueue (callInQ caller)

callService :: forall method api e m . ( MonadIO m
                                       , HasProtocol e (ServiceProto api e)
                                       , KnownNat (FromJust (FindMethodIndex 0 method api))
                                       , Serialise (Input method)
                                       , Serialise (Output method)
                                       )
            => ServiceCaller api e
            -> Input method
            -> m (Either ServiceError (Output method))

callService caller input = do
  req <- makeRequestR @api @method @e @m input

  resp <- UIO.newTQueueIO

  let addWaiter = atomically $ do
        UIO.modifyTVar (callWaiters caller) (HashMap.insert (reqNum req) resp)
        UIO.writeTQueue (callInQ caller) req

  let removeWaiter = atomically $
        UIO.modifyTVar (callWaiters caller) (HashMap.delete (reqNum req))

  liftIO $ bracket_ addWaiter removeWaiter $ do
    msg <- atomically $ UIO.readTQueue resp

    case msg of
      ServiceResponse _ (Right bs) ->
        case deserialiseOrFail @(Output method) bs of
          Left _  -> pure (Left ErrorInvalidResponse)
          Right x -> pure (Right x)

      ServiceResponse _ (Left wtf) -> pure (Left wtf)
      _ -> pure (Left ErrorInvalidResponse)


callRpcWaitMay :: forall method (api :: [Type]) m e proto t . ( MonadUnliftIO m
                                                              , KnownNat (FromJust (FindMethodIndex 0 method api))
                                                              , HasProtocol e (ServiceProto api e)
                                                              , Serialise (Input method)
                                                              , Serialise (Output method)
                                                              , IsTimeout t
                                                              , proto ~ ServiceProto api e
                                                              )
           => Timeout t
           -> ServiceCaller api e
           -> Input method
           -> m (Maybe (Output method))

callRpcWaitMay t caller args = do
  flip fix 0 $ \next i -> do
    race (pause t) (callService @method @api @e @m caller args)
      >>= \case
        Right (Right x) -> pure (Just x)
        -- _  | i < 1      -> next (succ i)
        _               -> pure Nothing



callRpcWaitRetry :: forall method (api :: [Type]) m e proto t . ( MonadUnliftIO m
                                                              , KnownNat (FromJust (FindMethodIndex 0 method api))
                                                              , HasProtocol e (ServiceProto api e)
                                                              , Serialise (Input method)
                                                              , Serialise (Output method)
                                                              , IsTimeout t
                                                              , proto ~ ServiceProto api e
                                                              )
           => Timeout t
           -> Int
           -> ServiceCaller api e
           -> Input method
           -> m (Maybe (Output method))

callRpcWaitRetry t attempts caller args = do
  flip fix 0 $ \next i -> do
    race (pause t) (callService @method @api @e @m caller args)
      >>= \case
        Right (Right x)   -> pure (Just x)
        _  | i < attempts -> next (succ i)
        _                 -> pure Nothing


makeClient :: forall api e m  . ( MonadIO m
                                , HasProtocol e (ServiceProto api e)
                                , Pretty (Peer e)
                                )
                => ServiceCaller api e
                -> ServiceProto api e
                -> m ()

makeClient = notifyServiceCaller

instance (HasProtocol e (ServiceProto api e)) => HasTimeLimits e (ServiceProto api e) IO where
  tryLockForPeriod _ _ = pure True

