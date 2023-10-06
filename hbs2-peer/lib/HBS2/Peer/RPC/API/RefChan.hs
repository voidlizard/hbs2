module HBS2.Peer.RPC.API.RefChan where

import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix (UNIX)

import Data.ByteString.Lazy ( ByteString )
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


