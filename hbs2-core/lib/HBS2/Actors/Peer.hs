{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
-- {-# Language AllowAmbiguousTypes #-}
module HBS2.Actors.Peer where

import HBS2.Actors
import HBS2.Clock
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Net.Messaging
import HBS2.Net.PeerLocator
import HBS2.Net.PeerLocator.Static
import HBS2.Net.Proto
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage

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
import Lens.Micro.Platform
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Hashable (hash)

import Codec.Serialise (serialise, deserialiseOrFail)

import Prettyprinter hiding (pipe)

data AnyStorage = forall zu . (Block ByteString ~ ByteString, Storage zu HbSync ByteString IO) => AnyStorage zu

instance (IsKey HbSync, Key HbSync ~ Hash HbSync, Block ByteString ~ ByteString) => Storage AnyStorage HbSync ByteString IO where

  putBlock (AnyStorage s) = putBlock s
  enqueueBlock (AnyStorage s) = enqueueBlock s
  getBlock (AnyStorage s) = getBlock s
  getChunk (AnyStorage s) = getChunk s
  hasBlock (AnyStorage s) = hasBlock s

data AnyMessage enc e = AnyMessage !Integer !(Encoded e)
                       deriving stock (Generic)

class Monad m => HasOwnPeer e m where
  ownPeer :: m (Peer e)

class Monad m => HasPeerLocator e m where
  getPeerLocator ::  m (AnyPeerLocator e)

class HasStorage m where
  getStorage :: m AnyStorage

data Fabriq e = forall bus . (Messaging bus e (Encoded e)) => Fabriq bus

class HasFabriq e m where
  getFabriq :: m (Fabriq e)


class Messaging (Fabriq e) e (AnyMessage (Encoded e) e) => PeerMessaging e

instance Messaging (Fabriq e) e (AnyMessage (Encoded e) e) => PeerMessaging e


instance (HasPeer e, Encoded e ~ ByteString) => Messaging (Fabriq e) e (AnyMessage ByteString e) where
  sendTo (Fabriq bus) t f (AnyMessage n bs) = sendTo bus t f (serialise (n, bs))

  receive (Fabriq bus) t = do
    recv <- receive @_ @e @ByteString bus t
    r <- forM recv $ \(f, msg) ->
          case deserialiseOrFail msg of
            Right (n,bs) -> pure $ Just (f, AnyMessage n bs)
            Left _       -> liftIO (print "FUCK!") >> pure Nothing -- FIXME what to do with undecoded messages?

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

data PeerEnv e =
  PeerEnv
  { _envSelf         :: Peer e
  , _envFab          :: Fabriq e
  , _envStorage      :: AnyStorage
  , _envPeerLocator  :: AnyPeerLocator e
  , _envDeferred     :: Pipeline IO ()
  , _envSessions     :: Cache SKey Dynamic
  , _envEvents       :: TVar (HashMap SKey [Dynamic])
  , _envExpireTimes  :: Cache SKey ()
  , _envSweepers     :: TVar (HashMap SKey [PeerM e IO ()])
  }

newtype PeerM e m a = PeerM { fromPeerM :: ReaderT (PeerEnv e) m a }
                      deriving newtype ( Functor
                                       , Applicative
                                       , Monad
                                       , MonadReader (PeerEnv e)
                                       , MonadIO
                                       )


newtype  ResponseM e m a = ResponseM { fromResponse :: ReaderT (ResponseEnv e) m a }
                           deriving newtype ( Functor
                                            , Applicative
                                            , Monad
                                            , MonadReader (ResponseEnv e)
                                            , MonadIO
                                            , MonadTrans
                                            )

newtype ResponseEnv e =
  ResponseEnv
  {  _answTo :: Peer e
  }

makeLenses 'PeerEnv

makeLenses 'ResponseEnv


runResponseM :: forall e m . (Monad m)
             => Peer e
             -> ResponseM e m ()
             -> m ()

runResponseM peer f = runReaderT (fromResponse f) (ResponseEnv peer)

instance Monad m => HasOwnPeer e (PeerM e m) where
  ownPeer = asks (view envSelf)

instance Monad m => HasPeerLocator e (PeerM e m) where
  getPeerLocator = asks (view envPeerLocator)

instance Monad m => HasFabriq e (PeerM e m) where
  getFabriq = asks (view envFab)

instance Monad m => HasStorage (PeerM e m) where
  getStorage = asks (view envStorage)


instance ( MonadIO m
         , HasProtocol e p
         , Eq (SessionKey e p)
         , Typeable (SessionKey e p)
         , Typeable (SessionData e p)
         , Hashable (SessionKey e p)
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

    case r of
     Just v  -> pure $ fn $ fromMaybe de (fromDynamic @(SessionData e p) v )
     Nothing -> do
       when upd $ liftIO $ Cache.insert se sk ddef
       pure (fn de)

  update de k f = do
    se <- asks (view envSessions)
    val <- fetch @e @p True de k id
    liftIO $ Cache.insert se (newSKey @(SessionKey e p) k) (toDyn (f val))

  expire k = do
    se <- asks (view envSessions)
    liftIO $ Cache.delete se (newSKey @(SessionKey e p) k)


instance ( MonadIO m
         , HasProtocol e p
         , HasFabriq e (PeerM e m)
         , Messaging (Fabriq e) e (AnyMessage (Encoded e) e)
         ) => Request e p (PeerM e m) where
  request p msg = do
    let proto = protoId @e @p (Proxy @p)
    pipe <- getFabriq @e
    me <- ownPeer @e
    sendTo pipe (To p) (From me) (AnyMessage @(Encoded e) @e proto (encode msg))


instance ( HasProtocol e p
         , Typeable (EventHandler e p (PeerM e IO))
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

instance ( HasProtocol e p
         , Typeable (EventKey e p)
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

runPeerM :: forall e m . (MonadIO m, HasPeer e, Ord (Peer e), Pretty (Peer e))
         => AnyStorage
         -> Fabriq e
         -> Peer e
         -> PeerM e m ()
         -> m ()

runPeerM s bus p f  = do

  pl <- AnyPeerLocator <$> newStaticPeerLocator @e mempty

  env <- PeerEnv p bus s pl <$> newPipeline defProtoPipelineSize
                            <*> liftIO (Cache.newCache (Just defCookieTimeout))
                            <*> liftIO (newTVarIO mempty)
                            <*> liftIO (Cache.newCache (Just defCookieTimeout))
                            <*> liftIO (newTVarIO mempty)

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

withPeerM :: MonadIO m => PeerEnv e -> PeerM e m a -> m ()
withPeerM env action = void $ runReaderT (fromPeerM action) env

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

    messages <- receive pipe (To me)

    for_ messages $ \(From pip, AnyMessage n msg :: AnyMessage (Encoded e) e) -> do

      case Map.lookup n disp of
        Nothing -> pure ()

        Just (AnyProtocol { protoDecode = decoder
                          , handle = h
                          }) -> maybe (pure ()) (runResponseM pip . h) (decoder msg)

instance ( HasProtocol e p
         , MonadTrans (ResponseM e)
         , HasStorage (PeerM e IO)
         , Pretty (Peer e)
         , PeerMessaging e
         ) => Response e p (ResponseM e (PeerM e IO)) where

  thatPeer _ = asks (view answTo)

  deferred _ action = do
    who <- asks (view answTo)
    pip <- lift $ asks (view envDeferred)
    env <- lift ask
    liftIO $ addJob pip $ withPeerM env (runResponseM who action)

  response msg = do
    let proto = protoId @e @p (Proxy @p)
    who <- asks (view answTo)
    self <- lift $ ownPeer @e
    fab  <- lift $ getFabriq @e
    sendTo fab (To who) (From self) (AnyMessage @(Encoded e) @e proto (encode msg))

instance ( MonadIO m
         , HasProtocol e p
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

