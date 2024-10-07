{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox
  ( module HBS2.Peer.Proto.Mailbox
  , module HBS2.Peer.Proto.Mailbox.Message
  ) where

import HBS2.Prelude.Plated

import HBS2.Data.Types.SignedBox

import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Message

import Codec.Serialise

data MailBoxStatusPayload s =
  MailBoxStatusPayload
  { mbsMailboxKey   :: MailboxKey s
  , mbsMailboxHash  :: HashRef
  }
  deriving stock (Generic)

data SetPolicyPayload s =
  SetPolicyPayload
  { sppMailboxKey    :: MailboxKey s
  , sppPolicyVersion :: PolicyVersion
  , sppPolicyRef     :: HashRef
  }
  deriving stock (Generic)

data GetPolicyPayload s =
  GetPolicyPayload
  { gppMailboxKey    :: MailboxKey s
  , gppPolicyVersion :: PolicyVersion
  , gppPolicyRef     :: HashRef
  }
  deriving stock (Generic)

data DeleteMessagesPayload s =
  DeleteMessagesPayload
  { dmpMailboxKey    :: MailboxKey s
  , dmpPredicate     :: MailboxMessagePredicate
  }
  deriving stock (Generic)

data MailBoxProtoMessage e s =
    SendMessage     (Message s) -- already has signed box
  | CheckMailbox    (SignedBox (MailboxKey s) s)
  | MailboxStatus   (SignedBox (MailBoxStatusPayload s) s)
  | SetPolicy       (SignedBox (SetPolicyPayload s) s)
  | CurrentPolicy   (GetPolicyPayload s)
  | DeleteMessages  (SignedBox (DeleteMessagesPayload s) s)
  deriving stock (Generic)

data MailBoxProto e s =
  MailBoxProtoV1 (MailBoxProtoMessage e s)

instance ForMailbox s => Serialise (MailBoxStatusPayload s)
instance ForMailbox s => Serialise (SetPolicyPayload s)
instance ForMailbox s => Serialise (GetPolicyPayload s)
instance ForMailbox s => Serialise (DeleteMessagesPayload s)
instance ForMailbox s => Serialise (MailBoxProtoMessage e s)





