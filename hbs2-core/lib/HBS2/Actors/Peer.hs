{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Actors.Peer
  ( module HBS2.Actors.Peer
  , module HBS2.Actors.Peer.Types
  , HasStorage(..), AnyStorage(..)
  ) where

import HBS2.Actors
import HBS2.Actors.Peer.Types
import HBS2.Clock
import HBS2.Defaults
import HBS2.Events
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging
import HBS2.Net.PeerLocator
import HBS2.Net.PeerLocator.Static
import HBS2.Net.Proto
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.System.Logger.Simple

import Data.Config.Suckless.KeyValue (HasConf(..))

import Control.Monad.Trans.Maybe
import Control.Concurrent.Async
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Dynamic
import Data.Foldable hiding (find)
import Data.Map qualified as Map
import Data.Maybe
import GHC.TypeLits
import Lens.Micro.Platform as Lens
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad.IO.Unlift

import Codec.Serialise (serialise, deserialiseOrFail)



class ( Eq (SessionKey e a)
      , Hashable (SessionKey e a)
      , Typeable (SessionData e a)
      , Typeable (SessionKey e a)
      , Expires (SessionKey e a)
      ) => PeerSessionKey e a

instance ( Eq (SessionKey e a)
         , Hashable (SessionKey e a)
         , Typeable (SessionData e a)
         , Typeable (SessionKey e a)
         , Expires (SessionKey e a)
         )
  => PeerSessionKey e a

instance (HasPeer e, Encoded e ~ ByteString) => Messaging (Fabriq e) e (AnyMessage ByteString e) where
  sendTo (Fabriq bus) t f (AnyMessage n bs) = sendTo bus t f (serialise (n, bs))

  receive (Fabriq bus) t = do
    recv <- receive @_ @e @ByteString bus t
    r <- forM recv $ \(f, msg) ->
          case deserialiseOrFail msg of
            Right (n,bs) -> pure $ Just (f, AnyMessage n bs)
            Left _       -> pure Nothing -- FIXME what to do with undecoded messages?

    pure $ catMaybes r

data AnyProtocol e m = forall p  . ( HasProtocol e p
                                   , Response e p m
                                   , Messaging (Fabriq e) e (AnyMessage (Encoded e) e)
                                   ) =>
  AnyProtocol
  { myProtoId   :: Integer
  , protoDecode :: Encoded e -> Maybe p
  , protoEncode :: p -> Encoded e
  , handle      :: p -> m ()
  }

makeResponse :: forall e p  m . ( MonadIO m
                                , Response e p m
                                , HasProtocol e p
                                , Messaging (Fabriq e) e (AnyMessage (Encoded e) e)
                                )
            => (p ->  m ()) -> AnyProtocol e m

makeResponse h = AnyProtocol { myProtoId  = natVal (Proxy @(ProtocolId p))
                             , protoDecode = decode
                             , protoEncode = encode
                             , handle = h
                             }

-- makeResponse' :: forall e p  m . ( MonadIO m
--                                 , Response e p m
--                                 , HasProtocol e p
--                                 , Messaging (Fabriq e) e (AnyMessage (Encoded e) e)
--                                 )
--   => (Encoded e -> Maybe p)
--   -> (p -> Encoded e)
--   -> (p ->  m ())
--   -> AnyProtocol e m

-- makeResponse' dec enc h = AnyProtocol { myProtoId  = natVal (Proxy @(ProtocolId p))
--                              , protoDecode = dec
--                              , protoEncode = enc
--                              , handle = h
--                              }

data PeerEnv e =
  PeerEnv
  { _envSelf          :: Peer e
  , _envPeerNonce     :: PeerNonce
  , _envFab           :: Fabriq e
  , _envStorage       :: AnyStorage
  , _envPeerLocator   :: AnyPeerLocator e
  , _envDeferred      :: Pipeline IO ()
  , _envSessions      :: Cache SKey Dynamic
  , _envEvents        :: TVar (HashMap SKey [Dynamic])
  , _envExpireTimes   :: Cache SKey ()
  , _envSweepers      :: TVar (HashMap SKey [PeerM e IO ()])
  , _envReqMsgLimit   :: Cache (Peer e, Integer, Encoded e) ()
  , _envReqProtoLimit :: Cache (Peer e, Integer) ()
  }

newtype PeerM e m a = PeerM { fromPeerM :: ReaderT (PeerEnv e) m a }
                      deriving newtype ( Functor
                                       , Applicative
                                       , Monad
                                       , MonadReader (PeerEnv e)
                                       , MonadIO
                                       , MonadUnliftIO
                                       , MonadTrans
                                       )


newtype  ResponseM e m a = ResponseM { fromResponse :: ReaderT (ResponseEnv e) m a }
                           deriving newtype ( Functor
                                            , Applicative
                                            , Monad
                                            , MonadReader (ResponseEnv e)
                                            , MonadIO
                                            , MonadUnliftIO
                                            , MonadTrans
                                            )

newtype ResponseEnv e =
  ResponseEnv
  {  _answTo :: Peer e
  }

makeLenses 'PeerEnv

makeLenses 'ResponseEnv


runResponseM :: forall e m a . (Monad m)
             => Peer e
             -> ResponseM e m a
             -> m a

runResponseM peer f = runReaderT (fromResponse f) (ResponseEnv peer)

instance HasConf m => HasConf (ResponseM e m) where
  getConf = lift getConf

instance Monad m => HasOwnPeer e (PeerM e m) where
  ownPeer = asks (view envSelf)

instance Monad m => HasPeerLocator e (PeerM e m) where
  getPeerLocator = asks (view envPeerLocator)

instance Monad m => HasFabriq e (PeerM e m) where
  getFabriq = asks (view envFab)

instance (Monad m) => HasStorage (PeerM e m) where
  getStorage = asks (view envStorage)

instance Monad m => HasPeerNonce e (PeerM e m) where
  peerNonce = asks (view envPeerNonce)

instance ( MonadIO m
         -- , HasProtocol e p
         , Eq (SessionKey e p)
         , Typeable (SessionKey e p)
         , Typeable (SessionData e p)
         , Hashable (SessionKey e p)
         , Expires  (SessionKey e p)
         ) => Sessions e p (PeerM e m) where


  find k f = do
    se <- asks (view envSessions)
    let sk = newSKey @(SessionKey e p) k
    r <- liftIO $ Cache.lookup se sk
    case fromDynamic @(SessionData e p) <$> r of
      Just v  -> pure $ f <$> v
      Nothing -> pure Nothing

  fetch upd de k fn  = do
    se <- asks (view envSessions)
    let sk = newSKey @(SessionKey e p) k
    let ddef = toDyn de

    r <- liftIO $ Cache.lookup se sk

    let ts = expiresIn (Proxy @(SessionKey e p)) <&> toTimeSpec

    case r of
     Just v  -> pure $ fn $ fromMaybe de (fromDynamic @(SessionData e p) v )
     Nothing -> do
       when upd $ liftIO $ Cache.insert' se ts sk ddef
       pure (fn de)

  update de k f = do
    se <- asks (view envSessions)
    val <- fetch @e @p True de k id
    let ts = expiresIn (Proxy @(SessionKey e p)) <&> toTimeSpec
    liftIO $ Cache.insert' se ts (newSKey @(SessionKey e p) k) (toDyn (f val))

  expire k = do
    se <- asks (view envSessions)
    liftIO $ Cache.delete se (newSKey @(SessionKey e p) k)

instance (MonadIO m, HasProtocol e p, Hashable (Encoded e))
  => HasTimeLimits e p (PeerM e m) where
  tryLockForPeriod peer msg = case requestPeriodLim @e @p of
    NoLimit -> pure True

    ReqLimPerMessage lim -> do
      let proto = protoId @e @p (Proxy @p)
      ex <- asks (view envReqMsgLimit)
      let bin = encode @e msg
      let key = (peer, proto, bin)
      here <- liftIO $ Cache.lookup ex key  <&> isJust
      unless here $ do
        liftIO $ Cache.insert' ex (Just (toTimeSpec lim)) key ()
      pure (not here)

    ReqLimPerProto lim -> do
      let proto = protoId @e @p (Proxy @p)
      ex <- asks (view envReqProtoLimit)
      let key = (peer, proto)
      here <- liftIO $ Cache.lookup ex key  <&> isJust
      unless here $ do
        liftIO $ Cache.insert' ex (Just (toTimeSpec lim)) key ()
      pure (not here)

instance ( MonadIO m
         , HasProtocol e msg
         , HasFabriq e m -- (PeerM e m)
         , HasOwnPeer e m
         , PeerMessaging e
         , HasTimeLimits e msg m
         , Show (Peer e)
         ) => Request e msg m where
  request peer_e msg = do
    let proto = protoId @e @msg (Proxy @msg)
    pipe <- getFabriq @e
    me <- ownPeer @e

    -- TODO: check if a request were sent to peer and timeout is here
    --       if not here - than send and create a new timeout
    --
    -- TODO: where to store the timeout?
    -- TODO: where the timeout come from?
    -- withTimeLimit @e @msg peer_e msg $ do
      -- liftIO $ print "request!"
    allowed <- tryLockForPeriod peer_e msg

    unless allowed do
      trace $ "REQUEST: not allowed to send for proto" <+> viaShow proto

    when allowed do
      sendTo pipe (To peer_e) (From me) (AnyMessage @(Encoded e) @e proto (encode msg))
      -- trace $ "REQUEST: after sendTo" <+> viaShow peer_e


instance ( Typeable (EventHandler e p (PeerM e IO))
         , Typeable (EventKey e p)
         , Typeable (Event e p)
         , Hashable (EventKey e p)
         , Expires  (EventKey e p)
         , Eq (EventKey e p)
         ) => EventListener e p (PeerM e IO) where

  subscribe k h = do
    ev <- asks (view envEvents)
    let sk = newSKey @(EventKey e p) k
    let dyn = toDyn h
    liftIO $ atomically $ modifyTVar' ev (HashMap.insertWith (<>) sk [dyn])
    -- FIXME: add a sweeping routine or else everything  will be fucked!
    addSweeper (expiresIn (Proxy @(EventKey e p))) sk $ do
      -- liftIO $ print $ "sweep smth with key" <+> pretty (hash sk)
      liftIO $ atomically $ modifyTVar' ev (HashMap.delete sk)

addSweeper :: forall e . Maybe (Timeout 'Seconds) -> SKey -> PeerM e IO () -> PeerM e IO ()
addSweeper t k sweeper = do
  -- liftIO $ print $ "adding sweeper for key" <+> pretty (hash k)
  ex <- asks (view envExpireTimes)
  sw <- asks (view envSweepers)
  liftIO $ Cache.insert' ex (toTimeSpec <$> t) k ()
  liftIO $ atomically $ modifyTVar' sw (HashMap.insertWith (<>) k [sweeper])

sweep :: PeerM e IO ()
sweep = do
  ex <- asks (view envExpireTimes)
  sw <- asks (view envSweepers)

  liftIO $ Cache.purgeExpired ex
  toSweep <- HashMap.toList <$> liftIO (readTVarIO sw)

  alive <- forM toSweep $ \(s, actions) -> do
             here <- liftIO $ Cache.lookup' ex s <&> isJust

             if here then
                pure [(s, actions)]
             else do
                sequence_ actions
                pure []

  liftIO $ atomically $ modifyTVar' sw (<> HashMap.fromList (mconcat alive))

instance ( Typeable (EventKey e p)
         , Typeable (Event e p)
         , Hashable (EventKey e p)
         , Eq (EventKey e p)
         , Typeable (EventHandler e p (PeerM e IO))
         , EventType (Event e p)
         , Pretty (Peer e)
         ) => EventEmitter e p (PeerM e IO) where

  emit k d = do
    pip <- asks (view envDeferred)
    env <- ask
    liftIO $ addJob pip $ withPeerM env $ do

      se <- asks (view envEvents)
      let sk = newSKey @(EventKey e p) k

      void $ runMaybeT $ do
        subs <- MaybeT $ liftIO $ atomically $ readTVar se <&> HashMap.lookup sk
        void $ liftIO $ atomically $ modifyTVar' se (HashMap.delete sk)
        pers <- forM subs $ \r -> do
                  ev <- MaybeT $ pure $ fromDynamic @(EventHandler e p (PeerM e IO)) r
                  liftIO $ withPeerM env $  ev d
                  if isPersistent @(Event e p) then
                    pure [r]
                  else
                    pure []

        void $ liftIO $ atomically $ modifyTVar' se (HashMap.insert sk (mconcat pers))


newPeerEnv :: forall e m . ( MonadIO m
                           , HasPeer e
                           , Ord (Peer e)
                           , Pretty (Peer e)
                           , HasNonces () m
                           , PeerMessaging e
                           , Asymm (Encryption e)
                           , Hashable (PubKey 'Sign (Encryption e))
                           , Hashable PeerNonce
                           )
          => AnyPeerLocator e
          -> AnyStorage
          -> Fabriq e
          -> Peer e
          -> m (PeerEnv e)

newPeerEnv pl s bus p = do
  let _envSelf      = p
  _envPeerNonce     <- newNonce @()
  let _envFab       = bus
  let _envStorage   = s
  let _envPeerLocator = pl
  _envDeferred      <- newPipeline defProtoPipelineSize
  _envSessions      <- liftIO (Cache.newCache (Just defCookieTimeout))
  _envEvents        <- liftIO (newTVarIO mempty)
  _envExpireTimes   <- liftIO (Cache.newCache (Just defCookieTimeout))
  _envSweepers      <- liftIO (newTVarIO mempty)
  _envReqMsgLimit   <- liftIO (Cache.newCache (Just defRequestLimit))
  _envReqProtoLimit <- liftIO (Cache.newCache (Just defRequestLimit))
  pure PeerEnv {..}

runPeerM :: forall e m . ( MonadIO m
                         , HasPeer e
                         , Ord (Peer e)
                         , Pretty (Peer e)
                         , HasNonces () m
                         )
         => PeerEnv e
         -> PeerM e m ()
         -> m ()

runPeerM env f  = do

  let de = view envDeferred env
  as <- liftIO $ replicateM 8 $ async $ runPipeline de

  sw <- liftIO $ async $ forever $ withPeerM env $ do
          pause defSweepTimeout
          se <- asks (view envSessions)
          liftIO $ Cache.purgeExpired se
          sweep

  void $ runReaderT (fromPeerM f) env
  void $ liftIO $ stopPipeline de
  liftIO $ mapM_ cancel (as <> [sw])

withPeerM :: MonadIO m => PeerEnv e -> PeerM e m a -> m a
withPeerM env action = runReaderT (fromPeerM action) env

runProto :: forall e m  . ( MonadIO m
                          , HasOwnPeer e m
                          , HasFabriq e m
                          , HasPeer e
                          , PeerMessaging e
                          )
        => [AnyProtocol e (ResponseM e m)]
        -> m ()

runProto hh = do
  me       <- ownPeer @e @m
  pipe     <- getFabriq @e

  let resp = [ (pid, a) | a@AnyProtocol { myProtoId = pid } <- hh ]

  let disp = Map.fromList resp

  forever $ do

    messages <- receive @_ @e pipe (To me)

    for_ messages $ \(From pip, AnyMessage n msg :: AnyMessage (Encoded e) e) -> do

      case Map.lookup n disp of
        Nothing -> do
          -- err $ "PROTO not found" <+> pretty n <+> pretty (fmap fst resp)
          pure () -- FIXME: error counting! and statistics counting feature

        Just (AnyProtocol { protoDecode = decoder
                          , handle = h
                          }) -> maybe (pure ()) (runResponseM pip . h) (decoder msg)


instance (Monad m, HasProtocol e p) => HasThatPeer p e (ResponseM e m) where
  thatPeer = asks (view answTo)

instance HasProtocol e p => HasDeferred p e (ResponseM e (PeerM e IO)) where
  deferred action = do
    who <- asks (view answTo)
    pip <- lift $ asks (view envDeferred)
    env <- lift ask
    liftIO $ addJob pip $ withPeerM env (runResponseM who action)
    -- void $ liftIO $ async $ withPeerM env (runResponseM who action)

instance ( HasProtocol e p
         , MonadTrans (ResponseM e)
         , HasThatPeer p e (ResponseM e m)
         , HasStorage (PeerM e IO)
         , Pretty (Peer e)
         , PeerMessaging e
         , HasOwnPeer e m
         , HasFabriq e m
         , MonadIO m
         ) => Response e p (ResponseM e m) where

  response msg = do
    let proto = protoId @e @p (Proxy @p)
    who <-  thatPeer @p
    self <- lift $ ownPeer @e
    fab  <- lift $ getFabriq @e
    sendTo fab (To who) (From self) (AnyMessage @(Encoded e) @e proto (encode msg))

instance ( MonadIO m
         -- , HasProtocol e p
         , Sessions e p m
         , Eq (SessionKey e p)
         , Typeable (SessionKey e p)
         , Typeable (SessionData e p)
         , Hashable (SessionKey e p)
         ) => Sessions e p (ResponseM e m) where

  find k f = lift (find k f)

  fetch i d k f = lift (fetch i d k f)

  update d k f = lift (update d k f)

  expire k = lift (expire  k)


instance ( MonadIO m
         , Hashable (EventKey e p)
         , EventEmitter e p m
         ) => EventEmitter e p (ResponseM e  m) where

  emit k d = lift $ emit k d

instance (Monad m, HasOwnPeer e m) => HasOwnPeer e (ResponseM e m) where
  ownPeer = lift ownPeer

instance (Monad m, HasFabriq e m) => HasFabriq e (ResponseM e m) where
  getFabriq = lift getFabriq

instance (Monad m, HasPeerNonce e m) => HasPeerNonce e (ResponseM e m) where
  peerNonce = lift $ peerNonce @e

instance (Monad m, HasPeerLocator e m) => HasPeerLocator e (ResponseM e m) where
  getPeerLocator = lift getPeerLocator

instance (Monad m, HasStorage m) => HasStorage (ResponseM e m) where
  getStorage = lift getStorage


