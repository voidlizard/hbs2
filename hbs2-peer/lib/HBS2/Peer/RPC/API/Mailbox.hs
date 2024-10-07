{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.API.Mailbox where

import HBS2.Peer.Prelude
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix (UNIX)
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox

import Data.ByteString.Lazy ( ByteString )
import Data.ByteString qualified as BS
import Codec.Serialise

data RpcMailboxPoke

type MailboxAPI = '[ RpcMailboxPoke
                   ]

type MailboxAPIProto =  0x056091510d3b2ec9


instance HasProtocol UNIX  (ServiceProto MailboxAPI UNIX) where
  type instance ProtocolId (ServiceProto MailboxAPI UNIX) = MailboxAPIProto
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

type instance Input RpcMailboxPoke   = ()
type instance Output RpcMailboxPoke  = ()





