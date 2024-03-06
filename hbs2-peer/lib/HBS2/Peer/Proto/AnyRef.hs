{-# Language UndecidableInstances #-}
module HBS2.Peer.Proto.AnyRef where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Base58
import HBS2.Net.Proto.Types
import HBS2.Data.Types.Refs

import Data.Maybe (fromMaybe)
import Data.Hashable hiding (Hashed)

newtype AnyRefKey t s = AnyRefKey (PubKey 'Sign s)

instance RefMetaData (AnyRefKey t s)

deriving stock instance IsRefPubKey s => Eq (AnyRefKey n s)

instance (IsRefPubKey s) => Hashable (AnyRefKey t s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance (IsRefPubKey s) => Hashed HbSync (AnyRefKey t s) where
  hashObject (AnyRefKey pk) = hashObject ("anyref|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (AnyRefKey t s) where
  fromStringMay s = AnyRefKey <$> fromStringMay s

instance IsRefPubKey s => IsString (AnyRefKey t s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s)) => Pretty (AsBase58 (AnyRefKey t s)) where
  pretty (AsBase58 (AnyRefKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s)) => Pretty (AnyRefKey n s) where
  pretty (AnyRefKey k) = pretty (AsBase58 k)


