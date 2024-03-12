module HBS2.Peer.RPC.API.LWWRef where

import HBS2.Peer.Prelude
import HBS2.Peer.Proto.LWWRef
import HBS2.Data.Types.SignedBox
import HBS2.Net.Messaging.Unix
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Service
import HBS2.Peer.Proto.RefLog (RefLogUpdate)

import Data.ByteString.Lazy (ByteString)
import Codec.Serialise

data RpcLWWRefGet
data RpcLWWRefUpdate
data RpcLWWRefFetch

type LWWRefAPI = '[ RpcLWWRefGet    -- may be done via storage
                  , RpcLWWRefUpdate --
                  , RpcLWWRefFetch  --
                  ]

instance HasProtocol UNIX  (ServiceProto LWWRefAPI UNIX) where
  type instance ProtocolId (ServiceProto LWWRefAPI UNIX) = 16267229472009458342
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

type instance Input RpcLWWRefGet   = LWWRefKey HBS2Basic
type instance Output RpcLWWRefGet  = Maybe (LWWRef L4Proto)

type instance Input RpcLWWRefFetch   = LWWRefKey HBS2Basic
type instance Output RpcLWWRefFetch  = ()

type instance Input RpcLWWRefUpdate = SignedBox (LWWRef L4Proto) L4Proto
type instance Output RpcLWWRefUpdate = ()




