{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module RPC where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Messaging.UDP
import HBS2.Hash
import HBS2.Actors.Peer
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Definition()

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise (serialise,deserialiseOrFail)
import Lens.Micro.Platform

data SetLogging =
    DebugOn Bool
  | TraceOn Bool
  deriving (Generic,Eq,Show)

instance Serialise SetLogging

data RPC e =
    RPCDie
  | RPCPoke
  | RPCPing (PeerAddr e)
  | RPCPong (PeerAddr e)
  | RPCPokeAnswer (PubKey 'Sign (Encryption e))
  | RPCPokeAnswerFull Text
  | RPCAnnounce (Hash HbSync)
  | RPCFetch (Hash HbSync)
  | RPCPeers
  | RPCPeersAnswer (PeerAddr e) (PubKey 'Sign (Encryption e))
  | RPCPexInfo
  | RPCPexInfoAnswer [PeerAddr L4Proto]
  | RPCLogLevel SetLogging
  | RPCRefLogUpdate ByteString
  | RPCRefLogFetch (PubKey 'Sign (Encryption e))
  | RPCRefLogGet (PubKey 'Sign (Encryption e))
  | RPCRefLogGetAnswer (Maybe (Hash HbSync))
  deriving stock (Generic)

deriving instance
  ( Show (PubKey 'Sign (Encryption e))
  , Show (PeerAddr e)
  ) => Show (RPC e)

instance (Serialise (PeerAddr e), Serialise (PubKey 'Sign (Encryption e))) => Serialise (RPC e)

instance HasProtocol L4Proto (RPC L4Proto) where
  type instance ProtocolId (RPC L4Proto) = 0xFFFFFFE0
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


data RPCEnv =
  RPCEnv
  { _rpcSelf :: Peer L4Proto
  , _rpcFab  :: Fabriq L4Proto
  }

makeLenses 'RPCEnv

data RpcAdapter e m =
  RpcAdapter
  { rpcOnPoke          :: RPC e -> m ()
  , rpcOnDie           :: RPC e -> m ()
  , rpcOnPokeAnswer    :: PubKey 'Sign (Encryption e) -> m ()
  , rpcOnPokeAnswerFull :: Text -> m ()
  , rpcOnAnnounce      :: Hash HbSync -> m ()
  , rpcOnPing          :: PeerAddr e -> m ()
  , rpcOnPong          :: PeerAddr e -> m ()
  , rpcOnFetch         :: Hash HbSync -> m ()
  , rpcOnPeers         :: RPC e -> m ()
  , rpcOnPeersAnswer   :: (PeerAddr e, PubKey 'Sign (Encryption e)) -> m ()
  , rpcOnPexInfo       :: RPC e -> m ()
  , rpcOnPexInfoAnswer :: [PeerAddr L4Proto] -> m ()
  , rpcOnLogLevel      :: SetLogging -> m ()
  , rpcOnRefLogUpdate  :: ByteString -> m ()
  , rpcOnRefLogFetch   :: PubKey 'Sign (Encryption e) -> m ()
  , rpcOnRefLogGet     :: PubKey 'Sign (Encryption e) -> m ()
  , rpcOnRefLogGetAnsw :: Maybe (Hash HbSync) -> m ()
  }

newtype RpcM m a = RpcM { fromRpcM :: ReaderT RPCEnv m a }
                        deriving newtype ( Functor
                                         , Applicative
                                         , Monad
                                         , MonadIO
                                         , MonadReader RPCEnv
                                         , MonadTrans
                                         )

runRPC :: ( MonadIO m
          , PeerMessaging L4Proto
          )
       => MessagingUDP -> RpcM m a -> m a

runRPC udp m = runReaderT (fromRpcM m) (RPCEnv pip (Fabriq udp))
  where
    pip = getOwnPeer udp

continueWithRPC :: RPCEnv -> RpcM m a -> m a
continueWithRPC e m = runReaderT (fromRpcM m) e

instance Monad m => HasFabriq L4Proto (RpcM m) where
  getFabriq = asks (view rpcFab)

instance Monad m => HasOwnPeer L4Proto (RpcM m) where
  ownPeer = asks (view rpcSelf)

instance (Monad m, HasProtocol L4Proto p) => HasTimeLimits L4Proto p (RpcM m) where
  tryLockForPeriod _ _ = pure True

rpcHandler :: forall e m  . ( MonadIO m
                            , Response e (RPC e) m
                            , HasProtocol e (RPC e)
                            , IsPeerAddr e m
                            )
           => RpcAdapter e m -> RPC e -> m ()

rpcHandler adapter = \case
    p@RPCDie{}         -> rpcOnDie adapter p
    p@RPCPoke{}        -> rpcOnPoke adapter p
    (RPCPokeAnswer k)  -> rpcOnPokeAnswer adapter k
    (RPCPokeAnswerFull k)  -> rpcOnPokeAnswerFull adapter k
    (RPCAnnounce h)    -> rpcOnAnnounce adapter h
    (RPCPing pa)       -> rpcOnPing adapter pa
    (RPCPong pa)       -> rpcOnPong adapter pa
    (RPCFetch h)       -> rpcOnFetch adapter h
    p@RPCPeers{}       -> rpcOnPeers adapter p
    (RPCPeersAnswer pa k) -> rpcOnPeersAnswer adapter (pa,k)
    p@RPCPexInfo{}     -> rpcOnPexInfo adapter p
    (RPCPexInfoAnswer pa) -> rpcOnPexInfoAnswer adapter pa
    (RPCLogLevel l)    -> rpcOnLogLevel adapter l
    (RPCRefLogUpdate bs)  -> rpcOnRefLogUpdate adapter bs
    (RPCRefLogFetch e) -> rpcOnRefLogFetch adapter e
    (RPCRefLogGet e)   -> rpcOnRefLogGet adapter e
    (RPCRefLogGetAnswer s)   -> rpcOnRefLogGetAnsw adapter s

