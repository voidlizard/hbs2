{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.API.Peer where

import HBS2.Prelude.Plated
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service
import HBS2.Data.Types.Refs (HashRef(..))
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

data RpcPollList
data RpcPollAdd
data RpcPollDel

type PeerAPI = '[ RpcPoke
                , RpcPing
                , RpcAnnounce
                , RpcFetch
                , RpcPeers
                , RpcPexInfo
                , RpcLogLevel
                , RpcDie
                , RpcPollList
                , RpcPollAdd
                , RpcPollDel
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

type instance Input RpcDie = ()
type instance Output RpcDie = ()

type instance Input RpcPoke = ()
type instance Output RpcPoke = String

type instance Input RpcPing = PeerAddr L4Proto
type instance Output RpcPing = Bool

type instance Input RpcAnnounce = HashRef
type instance Output RpcAnnounce = ()

type instance Input RpcPexInfo = ()
type instance Output RpcPexInfo = [PeerAddr L4Proto]

type instance Input RpcPeers = ()
type instance Output RpcPeers = [(PubKey 'Sign HBS2Basic, PeerAddr L4Proto)]

type instance Input RpcFetch = HashRef
type instance Output RpcFetch = ()

type instance Input RpcPollList= ()
type instance Output RpcPollList = [(PubKey 'Sign HBS2Basic, String, Int)]

type instance Input RpcPollAdd = (PubKey 'Sign HBS2Basic, String, Int)
type instance Output RpcPollAdd = ()

type instance Input  RpcPollDel = PubKey 'Sign HBS2Basic
type instance Output RpcPollDel = ()

type instance Input RpcLogLevel = SetLogging
type instance Output RpcLogLevel = ()

data SetLogging =
    DebugOn Bool
  | TraceOn Bool
  deriving (Generic,Eq,Show)

instance Serialise SetLogging

