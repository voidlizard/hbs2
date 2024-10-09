module HBS2.Peer.Proto.Mailbox.Entry where

import HBS2.Prelude
import HBS2.Peer.Proto.Mailbox.Types

import Data.Word
import Codec.Serialise

data MailboxEntry =
  Existed HashRef | Deleted HashRef
  deriving stock (Eq,Ord,Show,Generic)

data RoutedEntry = RoutedEntry HashRef
                   deriving stock (Eq,Ord,Show,Generic)

instance Serialise MailboxEntry
instance Serialise RoutedEntry


