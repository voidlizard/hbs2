{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.API.Mailbox where

import HBS2.Peer.Prelude
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix (UNIX)
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Data.Types.SignedBox

import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox

import Data.ByteString.Lazy ( ByteString )
import Data.ByteString qualified as BS
import Codec.Serialise

data RpcMailboxPoke
data RpcMailboxCreate
data RpcMailboxDelete
data RpcMailboxGetStatus
data RpcMailboxList
data RpcMailboxSend
data RpcMailboxGet

type MailboxAPI = '[ RpcMailboxPoke
                   , RpcMailboxCreate
                   , RpcMailboxDelete
                   , RpcMailboxGetStatus
                   , RpcMailboxList
                   , RpcMailboxSend
                   , RpcMailboxGet
                   ]

type MailboxAPIProto =  0x056091510d3b2ec9


instance HasProtocol UNIX  (ServiceProto MailboxAPI UNIX) where
  type instance ProtocolId (ServiceProto MailboxAPI UNIX) = MailboxAPIProto
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

type instance Input RpcMailboxPoke   = ()
type instance Output RpcMailboxPoke  = ()

type instance Input RpcMailboxCreate   = (PubKey 'Sign HBS2Basic, MailboxType)
type instance Output RpcMailboxCreate  = ()

type instance Input RpcMailboxDelete   = (PubKey 'Sign HBS2Basic)
type instance Output RpcMailboxDelete  = ()


type instance Input RpcMailboxGetStatus  = (PubKey 'Sign HBS2Basic)
type instance Output RpcMailboxGetStatus = Either MailboxServiceError (Maybe (MailBoxStatusPayload 'HBS2Basic))

type instance Input RpcMailboxList   = ()
type instance Output RpcMailboxList  = [(MailboxRefKey 'HBS2Basic, MailboxType)]

type instance Input RpcMailboxSend  = (Message HBS2Basic)
type instance Output RpcMailboxSend = ()

type instance Input RpcMailboxGet = (PubKey 'Sign HBS2Basic)
type instance Output RpcMailboxGet = (Maybe HashRef)

