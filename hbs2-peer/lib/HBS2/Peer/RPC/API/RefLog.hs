{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.API.RefLog where

import HBS2.Peer.Prelude
import HBS2.Net.Messaging.Unix
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Service
import HBS2.Peer.Proto.RefLog (RefLogUpdate)

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

type instance Input RpcRefLogGet = PubKey 'Sign HBS2Basic
type instance Output RpcRefLogGet = Maybe HashRef

type instance Input RpcRefLogFetch = PubKey  'Sign HBS2Basic
type instance Output RpcRefLogFetch = ()

type instance Input RpcRefLogPost = RefLogUpdate L4Proto
type instance Output RpcRefLogPost = ()
