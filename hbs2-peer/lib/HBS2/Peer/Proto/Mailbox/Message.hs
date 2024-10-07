{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox.Message where

import HBS2.Prelude.Plated

import HBS2.Peer.Proto.Mailbox.Types

import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Net.Auth.GroupKeySymm

import HBS2.OrDie
import HBS2.Base58
import HBS2.Storage
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Schema()

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Set
import Data.Set qualified as Set
import Data.Word
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import UnliftIO


newtype MessageTimestamp =
  MessageTimestamp Word64
  deriving newtype (Eq,Ord,Num,Enum,Integral,Real,Pretty,Show,Hashable)
  deriving stock Generic


newtype MessageTTL = MessageTTL Word32
  deriving newtype (Eq,Ord,Num,Enum,Integral,Real,Pretty,Show,Hashable)
  deriving stock Generic


data MessageCompression = GZip
  deriving stock (Eq,Ord,Generic,Show)

data MessageFlags =
  MessageFlags1
  { messageCreated     :: MessageTimestamp
  , messageTTL         :: Maybe MessageTTL
  , messageCompression :: Maybe MessageCompression
  , messageSchema      :: Maybe HashRef -- reserved
  }
  deriving stock (Eq,Ord,Generic,Show)

type MessageRecipient s = PubKey 'Sign s

data MessageContent s =
  MessageContent
  { messageFlags      :: MessageFlags
  , messageRecipients :: Set (MessageRecipient s)
  , messageGK0        :: Either HashRef (GroupKey 'Symm s)
  , messageParts      :: Set HashRef
  , messageData       :: SmallEncryptedBlock ByteString
  }
  deriving stock Generic

data Message s =
  MessageBasic
  { messageContent :: SignedBox (MessageContent s) s
  }
  deriving stock Generic


instance Serialise MessageTimestamp
instance Serialise MessageTTL
instance Serialise MessageCompression
instance Serialise MessageFlags
instance ForMailbox s => Serialise (MessageContent s)
instance ForMailbox s => Serialise (Message s)

-- TODO: mailbox-proto-handler

-- TODO: mailbox-proto-test?


data CreateMessageError =
    SenderNotSet
  | RecipientsNotSet
  | SigilNotFound HashRef
  | MalformedSigil (Maybe HashRef)
  | SenderNoAccesToGroupKey
  | NoCredentialsFound String
  | NoKeyringFound String
  deriving stock (Show,Typeable,Generic)

instance Exception CreateMessageError


defMessageFlags :: MonadIO m => m MessageFlags
defMessageFlags = MessageFlags1 <$> (round <$> liftIO getPOSIXTime)
                                <*> pure mzero
                                <*> pure mzero
                                <*> pure mzero

data CreateMessageServices s =
  CreateMessageServices
  { cmStorage          :: AnyStorage
  , cmLoadCredentials  :: forall m . MonadUnliftIO m => PubKey 'Sign s  -> m (Maybe (PeerCredentials s))
  , cmLoadKeyringEntry :: forall m . MonadUnliftIO m => PubKey 'Encrypt s -> m (Maybe (KeyringEntry s))
  }

createMessage :: forall s m . (MonadUnliftIO m , s ~ HBS2Basic)
              => CreateMessageServices s
              -> MessageFlags
              -> Maybe GroupSecret
              -> Either HashRef (Sigil s)    -- ^ sender
              -> [Either HashRef (Sigil s)]  -- ^ sigil keys (recipients)
              -> [HashRef]                   -- ^ message parts
              -> ByteString                  -- ^ payload
              -> m (Message s)
createMessage CreateMessageServices{..} _ gks sender' rcpts' parts bs = do
  -- TODO: support-flags
  flags <- defMessageFlags

  pips <- getKeys

  (sender, recipients) <- case pips of
                           []                 -> throwIO SenderNotSet
                           ( s : rs@(_ : _) ) -> pure (s,rs)
                           _                  -> throwIO RecipientsNotSet

  gk <- generateGroupKey @s gks (fmap snd pips)

  gkMt <- generateGroupKey @s gks mempty

  KeyringEntry pk sk _ <- cmLoadKeyringEntry (snd sender)
                            >>= orThrow (NoKeyringFound (show $ pretty $ AsBase58 (snd sender)))

  gks <- lookupGroupKey sk pk gk & orThrow SenderNoAccesToGroupKey

  encrypted <- encryptBlock cmStorage gks (Right gk) Nothing  bs

  let content = MessageContent @s
                  flags
                  (Set.fromList (fmap fst recipients))
                  (Right gk)
                  -- TODO: check-if-parts-exists
                  (Set.fromList parts)
                  encrypted

  creds <- cmLoadCredentials (fst sender)
             >>=  orThrow (NoCredentialsFound (show $ pretty $ AsBase58 (fst sender)))

  let ssk = view peerSignSk creds

  let box = makeSignedBox @s (fst sender) ssk content

  pure $ MessageBasic box

  where
    getKeys = do
      S.toList_ $ for_ (sender' : rcpts') $ \case
          Right si -> fromSigil Nothing si
          Left hs -> do
            si <- loadSigil @s cmStorage hs >>= orThrow (SigilNotFound hs)
            fromSigil (Just hs) si
    fromSigil h si = do
      (rcpt, SigilData{..}) <- unboxSignedBox0 (sigilData si) & orThrow (MalformedSigil h)
      S.yield (rcpt, sigilDataEncKey)


data ReadMessageServices s =
  ReadMessageServices
  { rmsFindGKS  :: forall m . MonadIO m => GroupKey 'Symm s -> m (Maybe GroupSecret)
  }

data ReadMessageError =
    ReadSignCheckFailed
  | ReadNoGroupKey
  | ReadNoGroupKeyAccess
  deriving stock (Show,Typeable)

instance Exception ReadMessageError

readMessage :: forall s m . ( MonadUnliftIO m
                            , s ~ HBS2Basic
                            )
            => ReadMessageServices s
            -> Message s
            -> m (PubKey 'Sign s, MessageContent s, ByteString)

readMessage ReadMessageServices{..} msg = do

  (pk, co@MessageContent{..}) <- unboxSignedBox0 (messageContent msg)
                                   & orThrow ReadSignCheckFailed

  -- TODO: support-groupkey-by-reference
  gk <- messageGK0 & orThrow ReadNoGroupKey

  gks <- rmsFindGKS gk >>= orThrow ReadNoGroupKeyAccess

  bs <- runExceptT (decryptBlockWithSecret @_ @s gks  messageData)
          >>= orThrowPassIO

  pure (pk, co, bs)

