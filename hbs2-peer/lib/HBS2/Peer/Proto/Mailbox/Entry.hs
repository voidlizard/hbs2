module HBS2.Peer.Proto.Mailbox.Entry where

import HBS2.Prelude
import HBS2.Peer.Proto.Mailbox.Types

import Control.Applicative
import Data.Word
import Codec.Serialise
import Data.Hashable

data ProofOfDelete =
  ProofOfDelete
  { deletePolicy  :: Maybe HashRef
  , deleteMessage :: Maybe HashRef
  }
  deriving stock (Generic,Eq,Ord,Show)

data ProofOfExist =
  ProofOfExist
  { existedPolicy  :: Maybe HashRef
  }
  deriving stock (Generic,Eq,Ord,Show)

instance Monoid ProofOfDelete where
  mempty = ProofOfDelete mzero mzero

instance Semigroup ProofOfDelete where
  (<>) (ProofOfDelete a1 b1) (ProofOfDelete a2 b2) = ProofOfDelete (a1 <|> a2) (b1 <|> b2)

instance Monoid ProofOfExist where
  mempty = ProofOfExist mzero

instance Semigroup ProofOfExist where
  (<>) (ProofOfExist a1) (ProofOfExist a2) = ProofOfExist (a1 <|> a2)

data MailboxEntry =
    Existed ProofOfExist  HashRef
  | Deleted ProofOfDelete HashRef  -- ^ proof-of-message-to-validate
  deriving stock (Eq,Ord,Show,Generic)

instance Hashable MailboxEntry where
  hashWithSalt salt = \case
    Existed p r -> hashWithSalt salt (0x177c1a3ad45b678e :: Word64, serialise (p,r))
    Deleted p r -> hashWithSalt salt (0xac3196b4809ea027 :: Word64, serialise (p,r))

data RoutedEntry = RoutedEntry HashRef
                   deriving stock (Eq,Ord,Show,Generic)

instance Serialise MailboxEntry
instance Serialise RoutedEntry
instance Serialise ProofOfDelete
instance Serialise ProofOfExist


