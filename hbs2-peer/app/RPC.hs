{-# Language TemplateHaskell #-}
module RPC where


import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Hash
import HBS2.Net.Messaging
import HBS2.Net.Messaging.UDP
import HBS2.Actors.Peer
import HBS2.Defaults

import Logger

import Control.Concurrent.Async
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise (serialise, deserialiseOrFail,Serialise)
import Lens.Micro.Platform

import Prettyprinter

data RPC e =
    RPCPing
  | RPCPong
  | RPCAnnounce (Hash HbSync)
  deriving stock (Eq,Generic,Show)


instance Serialise (RPC e)

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
  { rpcOnPing     :: RPC e -> m ()
  , rpcOnPong     :: RPC e -> m ()
  , rpcOnAnnounce :: Hash HbSync -> m ()
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

rpcHandler :: forall e m  . ( MonadIO m
                            , Response e (RPC e) m
                            , HasProtocol e (RPC e)
                            )
           => RpcAdapter e m -> RPC e -> m ()

rpcHandler adapter = \case
    p@RPCPing{}     -> rpcOnPing adapter p >> response (RPCPong @e)
    p@RPCPong{}     -> rpcOnPong adapter p
    (RPCAnnounce h) -> rpcOnAnnounce adapter h


