module RPC2.Peer.API where

import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service
import HBS2.Actors.Peer

import HBS2.Peer.RPC.Internal.Types

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise

data RpcPoke
data RpcAnnounce
data RpcPing
data RpcPexInfo
data RpcPeers
data RpcFetch
data RpcLogLevel
data RpcDie

type PeerAPI = '[ RpcPoke
                , RpcPing
                , RpcAnnounce
                , RpcFetch
                , RpcPeers
                , RpcPexInfo
                , RpcLogLevel
                , RpcDie
                ]

instance HasProtocol UNIX  (ServiceProto PeerAPI UNIX) where
  type instance ProtocolId (ServiceProto PeerAPI UNIX) = 0xDA2374610000
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance (Monad m)
  => HasRpcContext PeerAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  -- type instance RpcContext PeerAPI = RPC2Context
  getRpcContext = lift ask

