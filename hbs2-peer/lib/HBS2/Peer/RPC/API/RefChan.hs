{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.API.RefChan where

import HBS2.Peer.Prelude
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix (UNIX)
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox

import Data.ByteString.Lazy ( ByteString )
import Data.ByteString qualified as BS
import Codec.Serialise

-- NOTE: refchan-head-endpoints
data RpcRefChanHeadGet
data RpcRefChanHeadFetch
data RpcRefChanHeadPost

-- NOTE: refchan-endpoints
data RpcRefChanFetch
data RpcRefChanGet
data RpcRefChanPropose

data RpcRefChanNotify

type RefChanAPI = '[ RpcRefChanHeadGet
                   , RpcRefChanHeadFetch
                   , RpcRefChanHeadPost
                   , RpcRefChanGet
                   , RpcRefChanFetch
                   , RpcRefChanPropose
                   , RpcRefChanNotify
                   ]


type RefChanAPIProto =  0xDA2374630001

-- FIXME: hbs2-peer-protocols-to-
instance HasProtocol UNIX  (ServiceProto RefChanAPI UNIX) where
  type instance ProtocolId (ServiceProto RefChanAPI UNIX) = RefChanAPIProto
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


type instance Input RpcRefChanHeadGet = PubKey 'Sign HBS2Basic
type instance Output RpcRefChanHeadGet = Maybe HashRef

type instance Input RpcRefChanHeadFetch = PubKey 'Sign HBS2Basic
type instance Output RpcRefChanHeadFetch = ()

type instance Input RpcRefChanFetch = PubKey 'Sign HBS2Basic
type instance Output RpcRefChanFetch = ()

type instance Input RpcRefChanGet = PubKey 'Sign HBS2Basic
type instance Output RpcRefChanGet = Maybe HashRef

type instance Input RpcRefChanPropose = (PubKey 'Sign HBS2Basic, SignedBox BS.ByteString L4Proto)
type instance Output RpcRefChanPropose = ()

type instance Input RpcRefChanNotify = (PubKey 'Sign HBS2Basic, SignedBox BS.ByteString L4Proto)
type instance Output RpcRefChanNotify = ()

type instance Input RpcRefChanHeadPost = HashRef
type instance Output RpcRefChanHeadPost = ()

