{-# Language UndecidableInstances #-}
module HBS2.Peer.Proto.Mailbox.Ref where


import HBS2.Prelude
import HBS2.Hash
import HBS2.Base58
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs

import Data.Maybe (fromMaybe)
import Data.Hashable hiding (Hashed)

newtype MailboxRefKey s = MailboxRefKey (PubKey 'Sign s)

instance RefMetaData (MailboxRefKey s)

deriving stock instance IsRefPubKey s => Eq (MailboxRefKey s)

instance (IsRefPubKey s) => Hashable (MailboxRefKey s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance (IsRefPubKey s) => Hashed HbSync (MailboxRefKey s) where
  hashObject (MailboxRefKey pk) = hashObject ("mailboxv1|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (MailboxRefKey  s) where
  fromStringMay s = MailboxRefKey <$> fromStringMay s

instance IsRefPubKey s => IsString (MailboxRefKey s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s)) => Pretty (AsBase58 (MailboxRefKey s)) where
  pretty (AsBase58 (MailboxRefKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s)) => Pretty (MailboxRefKey s) where
  pretty (MailboxRefKey k) = pretty (AsBase58 k)


