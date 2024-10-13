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
import HBS2.Peer.Proto.Mailbox.Ref

import Codec.Serialise()
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Word
import Lens.Micro.Platform


data MergedEntry s = MergedEntry (MailboxRefKey s) HashRef
                   deriving stock (Generic)

instance ForMailbox s => Serialise (MergedEntry s)

data SetPolicyPayload s =
  SetPolicyPayload
  { sppMailboxKey    :: MailboxKey s
  , sppPolicyVersion :: PolicyVersion
  , sppPolicyRef     :: HashRef -- ^ merkle tree hash of policy description file
  }
  deriving stock (Generic)

-- for Hashable
deriving instance ForMailbox s => Eq (SetPolicyPayload s)

data MailBoxStatusPayload s =
  MailBoxStatusPayload
  { mbsMailboxPayloadNonce  :: Word64
  , mbsMailboxKey           :: MailboxKey s
  , mbsMailboxType          :: MailboxType
  , mbsMailboxHash          :: Maybe HashRef
  , mbsMailboxPolicy        :: Maybe (SignedBox (SetPolicyPayload s) s)
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
  | CheckMailbox     (Maybe Word64) (MailboxKey s)
  | MailboxStatus    (SignedBox (MailBoxStatusPayload s) s) -- signed by peer
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
             align $ vcat
                  [ parens ("nonce" <+> pretty mbsMailboxPayloadNonce)
                  , parens ("key"   <+> pretty (AsBase58 mbsMailboxKey))
                  , parens ("type"  <+> pretty mbsMailboxType)
                  , element "mailbox-tree"   mbsMailboxHash
                  , element "set-policy-payload-hash" (HashRef . hashObject . serialise <$> mbsMailboxPolicy)
                  , maybe mempty pretty spp
                  ]

      element el = maybe mempty ( \v -> parens (el <+> pretty v) )

      spp = mbsMailboxPolicy >>= unboxSignedBox0 <&> snd


instance ForMailbox s => Pretty (SetPolicyPayload s) where
  pretty SetPolicyPayload{..} = parens ( "set-policy-payload" <> line <> indent 2 (brackets w) )
    where
      w = align $
            vcat [ parens ( "version" <+> pretty sppPolicyVersion )
                 , parens ( "ref" <+> pretty sppPolicyRef )
                 ]

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
  now  <- liftIO $ getPOSIXTime <&> round
  that <- thatPeer @p
  se   <- find (KnownPeerKey that) id

  case mailBoxProtoPayload mess of
    SendMessage msg -> deferred @p do

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

    CheckMailbox _ k -> deferred @p do
      creds <- mailboxGetCredentials @s adapter

      void $ runMaybeT do

      -- TODO: check-policy

        s <- mailboxGetStatus adapter (MailboxRefKey @s k)
               >>= toMPlus
               >>= toMPlus

        let box = makeSignedBox @s (view peerSignPk creds) (view peerSignSk creds) s

        lift $ response @_ @p (MailBoxProtoV1 (MailboxStatus box))

    MailboxStatus box -> deferred @p do

      flip runContT pure $ callCC \exit -> do

        let r = unboxSignedBox0 @(MailBoxStatusPayload s) box

        PeerData{..} <- ContT $ maybe1 se none

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

    DeleteMessages{} -> do
      none


