{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module RPC where

import HBS2.Data.Types.Refs
import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Hash
import HBS2.Net.Messaging.UDP
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
    RPCPoke
  | RPCPing (PeerAddr e)
  | RPCPong (PeerAddr e)
  | RPCPokeAnswer (PubKey 'Sign e)
  | RPCAnnounce (Hash HbSync)
  | RPCFetch (Hash HbSync)
  | RPCPeers
  | RPCPeersAnswer (PeerAddr e) (PubKey 'Sign e)
  | RPCLogLevel SetLogging
  | RPCLRefAnn (Hash HbSync)
  | RPCLRefNew (PubKey 'Sign e) Text
  | RPCLRefNewAnswer (Hash HbSync)
  | RPCLRefGet (Hash HbSync)
  | RPCLRefGetAnswer (Maybe (Signed 'SignaturePresent (MutableRef e 'LinearRef)))
  deriving stock (Generic)


instance (
    Serialise (PeerAddr e)
  , Serialise (Signature e)
  ) => Serialise (RPC e)

instance HasProtocol UDP (RPC UDP) where
  type instance ProtocolId (RPC UDP) = 0xFFFFFFE0
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


data RPCEnv =
  RPCEnv
  { _rpcSelf :: Peer UDP
  , _rpcFab  :: Fabriq UDP
  }

makeLenses 'RPCEnv

data RpcAdapter e m =
  RpcAdapter
  { rpcOnPoke        :: RPC e -> m ()
  , rpcOnPokeAnswer  :: PubKey 'Sign e -> m ()
  , rpcOnAnnounce    :: Hash HbSync -> m ()
  , rpcOnPing        :: PeerAddr e -> m ()
  , rpcOnPong        :: PeerAddr e -> m ()
  , rpcOnFetch       :: Hash HbSync -> m ()
  , rpcOnPeers       :: RPC e -> m ()
  , rpcOnPeersAnswer :: (PeerAddr e, PubKey 'Sign e) -> m ()
  , rpcOnLogLevel    :: SetLogging -> m ()
  , rpcOnLRefAnn     :: Hash HbSync -> m ()
  , rpcOnLRefNew     :: (PubKey 'Sign e, Text) -> m ()
  , rpcOnLRefNewAnswer :: Hash HbSync -> m ()
  , rpcOnLRefGet     :: Hash HbSync -> m ()
  , rpcOnLRefGetAnswer :: Maybe (Signed 'SignaturePresent (MutableRef e 'LinearRef)) -> m ()
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
          , PeerMessaging UDP
          )
       => MessagingUDP -> RpcM m a -> m a

runRPC udp m = runReaderT (fromRpcM m) (RPCEnv pip (Fabriq udp))
  where
    pip = getOwnPeer udp

continueWithRPC :: RPCEnv -> RpcM m a -> m a
continueWithRPC e m = runReaderT (fromRpcM m) e

instance Monad m => HasFabriq UDP (RpcM m) where
  getFabriq = asks (view rpcFab)

instance Monad m => HasOwnPeer UDP (RpcM m) where
  ownPeer = asks (view rpcSelf)

instance (Monad m, HasProtocol UDP p) => HasTimeLimits UDP p (RpcM m) where
  tryLockForPeriod _ _ = pure True

rpcHandler :: forall e m  . ( MonadIO m
                            , Response e (RPC e) m
                            , HasProtocol e (RPC e)
                            , IsPeerAddr e m
                            )
           => RpcAdapter e m -> RPC e -> m ()

rpcHandler adapter = \case
    p@RPCPoke{}        -> rpcOnPoke adapter p
    (RPCPokeAnswer k)  -> rpcOnPokeAnswer adapter k
    (RPCAnnounce h)    -> rpcOnAnnounce adapter h
    (RPCPing pa)       -> rpcOnPing adapter pa
    (RPCPong pa)       -> rpcOnPong adapter pa
    (RPCFetch h)       -> rpcOnFetch adapter h
    p@RPCPeers{}       -> rpcOnPeers adapter p
    (RPCPeersAnswer pa k) -> rpcOnPeersAnswer adapter (pa,k)
    (RPCLogLevel l)    -> rpcOnLogLevel adapter l
    (RPCLRefAnn h)     -> rpcOnLRefAnn adapter h
    (RPCLRefNew pk t)     -> rpcOnLRefNew adapter (pk, t)
    (RPCLRefNewAnswer h) -> rpcOnLRefNewAnswer adapter h
    (RPCLRefGet h)     -> rpcOnLRefGet adapter h
    (RPCLRefGetAnswer hval) -> rpcOnLRefGetAnswer adapter hval

