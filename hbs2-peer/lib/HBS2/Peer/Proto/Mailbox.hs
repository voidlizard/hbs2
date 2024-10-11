{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox
  ( module HBS2.Peer.Proto.Mailbox
  , module HBS2.Peer.Proto.Mailbox.Message
  , module HBS2.Peer.Proto.Mailbox.Types
  , module HBS2.Peer.Proto.Mailbox.Ref
  ) where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Storage
import HBS2.Actors.Peer.Types

import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Message
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Peer.Proto.Mailbox.Ref

import Data.Maybe
import Control.Monad.Trans.Cont
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
  | GetPolicy       (SignedBox (GetPolicyPayload s) s)
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

class IsMailboxProtoAdapter s a where

  mailboxGetStorage     :: forall m . MonadIO m => a -> m AnyStorage

  mailboxAcceptMessage  :: forall m . (ForMailbox s, MonadIO m)
                        => a
                        -> Message s
                        -> MessageContent s
                        -> m ()

data MailboxServiceError =
  MailboxCreateFailed String
  deriving stock (Typeable,Show)


class ForMailbox s => IsMailboxService s a where

  mailboxCreate :: forall m . MonadIO m
                => a
                -> MailboxType
                -> Recipient s
                -> m (Either MailboxServiceError ())

  mailboxSendMessage :: forall m . MonadIO m
                     => a
                     -> Message s
                     -> m (Either MailboxServiceError ())

data AnyMailboxService s =
  forall a  . (IsMailboxService s a) => AnyMailboxService { mailboxService :: a }

data AnyMailboxAdapter s =
  forall a . (IsMailboxProtoAdapter s a) => AnyMailboxAdapter { mailboxAdapter :: a}

instance ForMailbox s => IsMailboxService s (AnyMailboxService s) where
  mailboxCreate (AnyMailboxService a) = mailboxCreate @s a
  mailboxSendMessage (AnyMailboxService a) = mailboxSendMessage @s a

instance IsMailboxProtoAdapter s (AnyMailboxAdapter s) where
  mailboxGetStorage (AnyMailboxAdapter a) = mailboxGetStorage @s a
  mailboxAcceptMessage (AnyMailboxAdapter a) = mailboxAcceptMessage @s a

mailboxProto :: forall e s m p a . ( MonadIO m
                                   , Response e p m
                                   , HasDeferred p e m
                                   , HasGossip e p m
                                   , IsMailboxProtoAdapter s a
                                   , p ~ MailBoxProto s e
                                   , s ~ Encryption e
                                   , ForMailbox s
                                   )
             => Bool -- ^ inner, i.e from own peer
             -> a
             -> MailBoxProto (Encryption e) e
             ->  m ()

mailboxProto inner adapter mess = do
  -- common stuff

  sto <- mailboxGetStorage @s adapter
  now <- liftIO $ getPOSIXTime <&> round

  case mailBoxProtoPayload mess of
    SendMessage msg -> deferred @p do
      -- TODO: implement-SendMessage
      --   [ ] check-if-mailbox-exists
      --   [ ] check-message-signature
      --   [ ] if-already-processed-then-skip
      --   [ ] store-message-hash-block-with-ttl
      --   [ ] if-message-to-this-mailbox-then store-message
      --   [ ] gossip-message

      -- проверяем, что еще не обрабатывали?
      -- если обрабатывали -- то дропаем
      -- что мы пишем в сторейдж?
      -- кто потом это дропает?

      flip runContT pure $ callCC \exit -> do

        -- проверить подпись быстрее, чем читать диск
        let unboxed' = unboxSignedBox0 @(MessageContent s) (messageContent msg)

        -- ок, сообщение нормальное, шлём госсип, пишем, что обработали
        -- TODO: increment-malformed-messages-statistics
        --   $workflow: backlog
        (_, content) <- ContT $ maybe1 unboxed' none

        let h = hashObject @HbSync (serialise msg) & HashRef

        let routed = serialise (RoutedEntry h)
        let routedHash = hashObject routed

        seen <- hasBlock sto routedHash <&> isJust

        unless seen $ lift do
          gossip mess
          mailboxAcceptMessage adapter msg content
          -- TODO: expire-block-and-collect-garbage
          --   $class: leak
          void $ putBlock sto routed

    CheckMailbox{} -> do
      -- TODO: implement-CheckMailbox
      --  [ ]  check-signed-box-or-drop
      --  [ ]  if-client-has-mailbox-then
      --  [ ]     get-mailbox-status
      --  [ ]     answer-MailboxStatus
      --  [ ]  gossip-message?
      none

    MailboxStatus{} -> do
      -- TODO: implement-MailboxStatus
      --
      --  [ ]  if-do-gossip-setting-then
      --  [ ]     gossip-MailboxStatus
      --
      --  [ ]  check-signed-box-or-drop
      --  [ ]  if-client-has-mailbox-then
      --  [ ]     get-mailbox-status
      --  [ ]     answer-MailboxStatus
      --
      none

    SetPolicy{} -> do
      none

    GetPolicy{} -> do
      none

    CurrentPolicy{} -> do
      none

    DeleteMessages{} -> do
      none



