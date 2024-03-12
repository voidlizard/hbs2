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

data LWWRefGet
data LWWRefUpdate

type LWWRefAPI = '[ LWWRefGet    -- may be done via storage
                  , LWWRefUpdate --
                  ]


instance HasProtocol UNIX  (ServiceProto LWWRefAPI UNIX) where
  type instance ProtocolId (ServiceProto LWWRefAPI UNIX) = 16267229472009458342
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

type instance Input LWWRefGet   = LWWRefKey HBS2Basic
type instance Output LWWRefGet  = Maybe (LWWRef L4Proto)

type instance Input LWWRefUpdate = SignedBox (LWWRef L4Proto) L4Proto
type instance Output LWWRefUpdate = ()



