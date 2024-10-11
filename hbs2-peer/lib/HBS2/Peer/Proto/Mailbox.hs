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
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Storage
import HBS2.Actors.Peer.Types
import HBS2.Net.Auth.Credentials

import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Message
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Peer.Proto.Mailbox.Ref

import Codec.Serialise()
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Word
import Lens.Micro.Platform

data MailBoxStatusPayload s =
  MailBoxStatusPayload
  { mbsMailboxPayloadNonce  :: Word64
  , mbsMailboxKey           :: MailboxKey s
  , mbsMailboxType          :: MailboxType
  , mbsMailboxHash          :: Maybe HashRef
  , mbsMailboxPolicyVersion :: Maybe PolicyVersion
  , mbsMailboxPolicyHash    :: Maybe HashRef
  }
  deriving stock (Generic)

data SetPolicyPayload s =
  SetPolicyPayload
  { sppMailboxKey    :: MailboxKey s
  , sppPolicyVersion :: PolicyVersion
  , sppPolicyRef     :: HashRef
  }
  deriving stock (Generic)

data DeleteMessagesPayload s =
  DeleteMessagesPayload
  { dmpMailboxKey    :: MailboxKey s
  , dmpPredicate     :: MailboxMessagePredicate
  }
  deriving stock (Generic)

data MailBoxProtoMessage s e =
    SendMessage      (Message s) -- already has signed box
  | CheckMailbox     (MailboxKey s)
  | MailboxStatus    (SignedBox (MailBoxStatusPayload s) s) -- signed by peer
  | SetPolicy        (SignedBox (SetPolicyPayload s) s)
  | DeleteMessages   (SignedBox (DeleteMessagesPayload s) s)
  deriving stock (Generic)

data MailBoxProto s e =
  MailBoxProtoV1 { mailBoxProtoPayload :: MailBoxProtoMessage s e }
  deriving stock (Generic)

instance ForMailbox s => Serialise (MailBoxStatusPayload s)
instance ForMailbox s => Serialise (SetPolicyPayload s)
instance ForMailbox s => Serialise (DeleteMessagesPayload s)
instance ForMailbox s => Serialise (MailBoxProtoMessage s e)
instance ForMailbox s => Serialise (MailBoxProto s e)

class IsMailboxProtoAdapter s a where

  mailboxGetCredentials :: forall m . MonadIO m => a -> m (PeerCredentials s)

  mailboxGetStorage     :: forall m . MonadIO m => a -> m AnyStorage

  mailboxAcceptMessage  :: forall m . (ForMailbox s, MonadIO m)
                        => a
                        -> Message s
                        -> MessageContent s
                        -> m ()


class ForMailbox s => IsMailboxService s a where

  mailboxCreate :: forall m . MonadIO m
                => a
                -> MailboxType
                -> Recipient s
                -> m (Either MailboxServiceError ())

  mailboxDelete :: forall m . MonadIO m
                => a
                -> Recipient s
                -> m (Either MailboxServiceError ())

  mailboxSendMessage :: forall m . MonadIO m
                     => a
                     -> Message s
                     -> m (Either MailboxServiceError ())


  mailboxSendDelete :: forall m . MonadIO m
                    => a
                    -> MailboxRefKey s
                    -> MailboxMessagePredicate
                    -> m (Either MailboxServiceError ())

  mailboxListBasic :: forall m . MonadIO m
                   => a
                   -> m (Either MailboxServiceError [(MailboxRefKey s, MailboxType)])

  mailboxGetStatus :: forall m . MonadIO m
                   => a
                   -> MailboxRefKey s
                   -> m (Either MailboxServiceError (Maybe (MailBoxStatusPayload s)))


data AnyMailboxService s =
  forall a  . (IsMailboxService s a) => AnyMailboxService { mailboxService :: a }

data AnyMailboxAdapter s =
  forall a . (IsMailboxProtoAdapter s a) => AnyMailboxAdapter { mailboxAdapter :: a }

instance ForMailbox s => IsMailboxService s (AnyMailboxService s) where
  mailboxCreate (AnyMailboxService a) = mailboxCreate @s a
  mailboxDelete (AnyMailboxService a) = mailboxDelete @s a
  mailboxSendMessage (AnyMailboxService a) = mailboxSendMessage @s a
  mailboxSendDelete (AnyMailboxService a) = mailboxSendDelete @s a
  mailboxListBasic (AnyMailboxService a) = mailboxListBasic @s a
  mailboxGetStatus (AnyMailboxService a) = mailboxGetStatus @s a

instance IsMailboxProtoAdapter s (AnyMailboxAdapter s) where
  mailboxGetCredentials (AnyMailboxAdapter a) = mailboxGetCredentials @s a
  mailboxGetStorage (AnyMailboxAdapter a) = mailboxGetStorage @s a
  mailboxAcceptMessage (AnyMailboxAdapter a) = mailboxAcceptMessage @s a

instance ForMailbox s => Pretty (MailBoxStatusPayload s) where
  pretty MailBoxStatusPayload{..} =
    parens $ "mailbox-status" <> line <> st
    where
      st = indent 2 $
             brackets $
             vcat [ parens ("nonce" <+> pretty mbsMailboxPayloadNonce)
                  , parens ("key"   <+> pretty (AsBase58 mbsMailboxKey))
                  , parens ("type"  <+> pretty mbsMailboxType)
                  , element "mailbox-tree"   mbsMailboxHash
                  , element "policy-version" mbsMailboxPolicyVersion
                  , element "policy-tree"    mbsMailboxPolicyHash
                  ]

      element el = maybe mempty ( \v -> parens (el <+> pretty v) )

mailboxProto :: forall e s m p a . ( MonadIO m
                                   , Response e p m
                                   , HasDeferred p e m
                                   , HasGossip e p m
                                   , IsMailboxProtoAdapter s a
                                   , IsMailboxService s a
                                   , p ~ MailBoxProto s e
                                   , s ~ Encryption e
                                   , ForMailbox s
                                   )
             => Bool -- ^ inner, i.e from own peer
             -> a
             -> MailBoxProto (Encryption e) e
             ->  m ()

mailboxProto inner adapter mess = deferred @p do
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

    -- NOTE: CheckMailbox-auth
    --   поскольку пир не владеет приватными ключами,
    --   то и подписать это сообщение он не может.
    --
    --   В таком случае, и в фоновом режиме нельзя будет
    --   синхронизировать ящики.
    --
    --   Поскольку все сообщения зашифрованы (но не их метаданные!)
    --   статус мейлобокса является открытой в принципе информацией.
    --
    --   Теперь у нас два пути:
    --    1. Отдавать только авторизованными пирам (которые имеют майлобоксы)
    --       для этого сделаем сообщение CheckMailboxAuth{}
    --
    --    2. Шифровать дерево с метаданными, так как нам в принципе
    --       может быть известен публичный ключ шифрования автора,
    --       но это сопряжено со сложностями с обновлением ключей.
    --
    --    С другой стороны, если нас не очень беспокоит возможное раскрытие
    --    метаданных --- то тот, кто скачает мейлобокс для анализа --- будет
    --    участвовать в раздаче.
    --
    --    С другой стороны, может он и хочет участвовать в раздаче, что бы каким-то
    --    образом ей вредить или устраивать слежку.
    --
    --    С этим всем можно бороться поведением и policy:
    --
    --    например:
    --      - не отдавать сообщения неизвестным пирам
    --      - требовать авторизацию (CheckMailboxAuth не нужен т.к. пир авторизован
    --        и так и известен в протоколе)
    --

    CheckMailbox k -> deferred @p do
      creds <- mailboxGetCredentials @s adapter

      void $ runMaybeT do

      -- TODO: check-policy

        s <- mailboxGetStatus adapter (MailboxRefKey @s k)
               >>= toMPlus
               >>= toMPlus

        let box = makeSignedBox @s (view peerSignPk creds) (view peerSignSk creds) s

        lift $ response @_ @p (MailBoxProtoV1 (MailboxStatus box))

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

    DeleteMessages{} -> do
      none


