{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.Mailbox.Message where

import HBS2.Prelude.Plated

import HBS2.Peer.Proto.Mailbox.Types

import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Merkle.MetaData

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
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Set qualified as Set
import Data.HashMap.Strict qualified as HM
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import UnliftIO


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
              -> [m ([(Text, Text)], LBS.ByteString)] -- ^ message parts
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

  trees <- for parts $ \mpart -> do
    (meta, lbs) <- mpart

    let mt = vcat [ pretty k <> ":" <+> dquotes (pretty v)
                  | (k,v) <- HM.toList (HM.fromList meta)
                  ]
               & show & Text.pack

    createEncryptedTree cmStorage gks gk (DefSource mt lbs)

  let content = MessageContent @s
                  flags
                  (Set.fromList (fmap fst recipients))
                  (Right gk)
                  (Set.fromList trees)
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

