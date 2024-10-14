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
import HBS2.Data.Types.Peer
import HBS2.Net.Auth.Credentials


import HBS2.Net.Proto.Sessions
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.Mailbox.Types
import HBS2.Peer.Proto.Mailbox.Message
import HBS2.Peer.Proto.Mailbox.Entry
import HBS2.Peer.Proto.Mailbox.Policy
import HBS2.Peer.Proto.Mailbox.Ref

import HBS2.Misc.PrettyStuff
import HBS2.System.Logger.Simple

import Codec.Serialise()
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Word
import Lens.Micro.Platform


class ForMailbox s => IsMailboxProtoAdapter s a where

  mailboxGetCredentials :: forall m . MonadIO m => a -> m (PeerCredentials s)

  mailboxGetStorage     :: forall m . MonadIO m => a -> m AnyStorage

  mailboxGetPolicy      :: forall m . MonadIO m => a -> m (AnyPolicy s)

  mailboxAcceptMessage  :: forall m . (ForMailbox s, MonadIO m)
                        => a
                        -> Message s
                        -> MessageContent s
                        -> m ()

  mailboxAcceptDelete   :: forall m . (ForMailbox s, MonadIO m)
                        => a
                        -> MailboxRefKey s
                        -> DeleteMessagesPayload s
                        -> SignedBox (DeleteMessagesPayload s) s -- ^ we need this for proof
                        -> m ()

class ForMailbox s => IsMailboxService s a where

  mailboxCreate :: forall m . MonadIO m
                => a
                -> MailboxType
                -> Recipient s
                -> m (Either MailboxServiceError ())

  mailboxSetPolicy :: forall m . MonadIO m
                   => a
                   -> SignedBox (SetPolicyPayload s) s
                   -> m (Either MailboxServiceError HashRef)

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
                    -> SignedBox (DeleteMessagesPayload s) s
                    -> m (Either MailboxServiceError ())

  mailboxListBasic :: forall m . MonadIO m
                   => a
                   -> m (Either MailboxServiceError [(MailboxRefKey s, MailboxType)])

  mailboxGetStatus :: forall m . MonadIO m
                   => a
                   -> MailboxRefKey s
                   -> m (Either MailboxServiceError (Maybe (MailBoxStatusPayload s)))

  mailboxAcceptStatus :: forall m . MonadIO m
                      => a
                      -> MailboxRefKey s
                      -> PubKey 'Sign s -- ^ peer's key
                      -> MailBoxStatusPayload s
                      -> m (Either MailboxServiceError ())

  mailboxFetch :: forall m . MonadIO m
               => a
               -> MailboxRefKey s
               -> m (Either MailboxServiceError ())

data AnyMailboxService s =
  forall a  . (IsMailboxService s a) => AnyMailboxService { mailboxService :: a }

data AnyMailboxAdapter s =
  forall a . (IsMailboxProtoAdapter s a) => AnyMailboxAdapter { mailboxAdapter :: a }

instance ForMailbox s => IsMailboxService s (AnyMailboxService s) where
  mailboxCreate (AnyMailboxService a) = mailboxCreate @s a
  mailboxSetPolicy (AnyMailboxService a) = mailboxSetPolicy @s a
  mailboxDelete (AnyMailboxService a) = mailboxDelete @s a
  mailboxSendMessage (AnyMailboxService a) = mailboxSendMessage @s a
  mailboxSendDelete (AnyMailboxService a) = mailboxSendDelete @s a
  mailboxListBasic (AnyMailboxService a) = mailboxListBasic @s a
  mailboxGetStatus (AnyMailboxService a) = mailboxGetStatus @s a
  mailboxAcceptStatus (AnyMailboxService a) = mailboxAcceptStatus @s a
  mailboxFetch (AnyMailboxService a) = mailboxFetch @s a

instance ForMailbox s => IsMailboxProtoAdapter s (AnyMailboxAdapter s) where
  mailboxGetCredentials (AnyMailboxAdapter a) = mailboxGetCredentials @s a
  mailboxGetPolicy (AnyMailboxAdapter a) = mailboxGetPolicy @s a
  mailboxGetStorage (AnyMailboxAdapter a) = mailboxGetStorage @s a
  mailboxAcceptMessage (AnyMailboxAdapter a) = mailboxAcceptMessage @s a
  mailboxAcceptDelete (AnyMailboxAdapter a) = mailboxAcceptDelete @s a


mailboxProto :: forall e s m p a . ( MonadIO m
                                   , Response e p m
                                   , HasDeferred p e m
                                   , HasGossip e p m
                                   , IsMailboxProtoAdapter s a
                                   , IsMailboxService s a
                                   , Sessions e (KnownPeer e) m
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

  sto  <- mailboxGetStorage @s adapter
  policy <- mailboxGetPolicy @s adapter
  pc <- mailboxGetCredentials @s adapter

  now  <- liftIO $ getPOSIXTime <&> round
  that <- thatPeer @p
  se'  <- find (KnownPeerKey that) id

  flip runContT pure $ callCC \exit -> do

    se <- ContT $ maybe1 se' none

    pip <- if inner then do
              pure $ view peerSignPk pc
            else do
              pure $ view peerSignKey se

    acceptPeer <- policyAcceptPeer @s policy pip

    unless acceptPeer do
      debug $ red "Peer rejected by policy" <+> pretty (AsBase58 pip)
      exit ()

    case mailBoxProtoPayload mess of
      SendMessage msg -> do

        -- проверить подпись быстрее, чем читать диск
        let unboxed' = unboxSignedBox0 @(MessageContent s) (messageContent msg)

        -- ок, сообщение нормальное, шлём госсип, пишем, что обработали
        -- TODO: increment-malformed-messages-statistics
        --   $workflow: backlog
        (_, content) <- ContT $ maybe1 unboxed' none

        let h = hashObject @HbSync (serialise mess) & HashRef

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

      CheckMailbox _ k ->  do
        creds <- mailboxGetCredentials @s adapter

        void $ runMaybeT do

          s <- mailboxGetStatus adapter (MailboxRefKey @s k)
                 >>= toMPlus
                 >>= toMPlus

          let box = makeSignedBox @s (view peerSignPk creds) (view peerSignSk creds) s

          lift $ lift $ response @_ @p (MailBoxProtoV1 (MailboxStatus box))

      MailboxStatus box -> do

        let r = unboxSignedBox0 @(MailBoxStatusPayload s) box

        let PeerData{..} = se

        (who, content@MailBoxStatusPayload{..}) <- ContT $ maybe1 r none

        unless ( who == _peerSignKey ) $ exit ()

        -- FIXME: timeout-hardcode
        --   может быть вообще не очень хорошо
        --   авторизовываться по времени.
        --   возможно, надо слать нонс в CheckMailbox
        --   и тут его проверять
        unless ( abs (now - mbsMailboxPayloadNonce) < 3 ) $ exit ()

        -- NOTE: possible-poisoning-attack
        --  левый пир генерирует merkle tree сообщений и посылает его.
        --  чего он может добиться: добавить "валидных" сообщений, которых не было
        --  в ящике изначально. (зашифрованных, подписанных).
        --
        --  можно рассылать спам, ведь каждое спам-сообщение
        --  будет валидно.
        --  мы не можем подписывать что-либо подписью владельца ящика,
        --  ведь мы не владеем его ключом.
        --
        --  как бороться:  в policy ограничивать число пиров, которые
        --  могут отдавать статус и игнорировать статусы от прочих пиров.
        --
        --  другой вариант -- каким-то образом публикуется подтверждение
        --  от автора, что пир X владеет почтовым ящиком R.
        --
        --  собственно, это и есть policy.
        --
        --  а вот policy мы как раз можем публиковать с подписью автора,
        --  он участвует в процессе обновления policy.

        void $ mailboxAcceptStatus adapter (MailboxRefKey mbsMailboxKey) who content

      DeleteMessages box -> do

        -- TODO: possible-ddos
        --   посылаем левые сообщения, заставляем считать
        --   подписи
        --
        --   Решения:  ограничивать поток сообщения от пиров
        --
        --   Возможно, вообще принимать только сообщения от пиров,
        --   которые содержатся в U {Policy(Mailbox_i)}
        --
        --   Возможно: PoW

        let r = unboxSignedBox0 box

        (mbox, spp) <- ContT $ maybe1 r none

        let h = hashObject @HbSync (serialise mess) & HashRef

        let routed = serialise (RoutedEntry h)
        let routedHash = hashObject routed

        seen <- hasBlock sto routedHash <&> isJust

        unless seen $ lift do
          gossip mess
          -- TODO: expire-block-and-collect-garbage
          --   $class: leak
          void $ putBlock sto routed

        mailboxAcceptDelete adapter (MailboxRefKey mbox) spp box

        none

