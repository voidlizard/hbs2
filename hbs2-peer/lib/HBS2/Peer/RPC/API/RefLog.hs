module HBS2.Peer.RPC.API.RefLog where

import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service

import Data.ByteString.Lazy (ByteString)
import Codec.Serialise

data RpcRefLogGet
data RpcRefLogFetch
data RpcRefLogPost

type RefLogAPI = '[ RpcRefLogGet
                  , RpcRefLogFetch
                  , RpcRefLogPost
                  ]



instance HasProtocol UNIX  (ServiceProto RefLogAPI UNIX) where
  type instance ProtocolId (ServiceProto RefLogAPI UNIX) = 0xDA2371620001
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

