module HBS2.Peer.Proto.Mailbox.Entry where

import HBS2.Prelude
import HBS2.Peer.Proto.Mailbox.Types

import Data.Word
import Codec.Serialise
import Data.Hashable

data MailboxEntry =
  Existed HashRef | Deleted HashRef
  deriving stock (Eq,Ord,Show,Generic)

instance Hashable MailboxEntry where
  hashWithSalt salt = \case
    Existed r -> hashWithSalt salt (0x177c1a3ad45b678e :: Word64, r)
    Deleted r -> hashWithSalt salt (0xac3196b4809ea027 :: Word64, r)

data RoutedEntry = RoutedEntry HashRef
                   deriving stock (Eq,Ord,Show,Generic)

instance Serialise MailboxEntry
instance Serialise RoutedEntry


