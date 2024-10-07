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

data MailBoxProtoMessage s e =
    SendMessage     (Message s) -- already has signed box
  | CheckMailbox    (SignedBox (MailboxKey s) s)
  | MailboxStatus   (SignedBox (MailBoxStatusPayload s) s)
  | SetPolicy       (SignedBox (SetPolicyPayload s) s)
  | CurrentPolicy   (GetPolicyPayload s)
  | DeleteMessages  (SignedBox (DeleteMessagesPayload s) s)
  deriving stock (Generic)

data MailBoxProto s e =
  MailBoxProtoV1 { mailBoxProtoPayload :: MailBoxProtoMessage s e }
  deriving stock (Generic)

instance ForMailbox s => Serialise (MailBoxStatusPayload s)
instance ForMailbox s => Serialise (SetPolicyPayload s)
instance ForMailbox s => Serialise (GetPolicyPayload s)
instance ForMailbox s => Serialise (DeleteMessagesPayload s)
instance ForMailbox s => Serialise (MailBoxProtoMessage s e)
instance ForMailbox s => Serialise (MailBoxProto s e)

mailboxProto :: forall e m p . ( MonadIO m
                               , Response e p m
                               , HasDeferred p e m
                               , p ~ MailBoxProto (Encryption e) e
                               )
             => MailBoxProto (Encryption e) e
             ->  m ()

mailboxProto mess = do
  -- common stuff

  case mailBoxProtoPayload mess of
    SendMessage{} -> none
    _ -> none

  pure ()


