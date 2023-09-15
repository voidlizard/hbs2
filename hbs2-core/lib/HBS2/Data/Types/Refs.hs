{-# Language DuplicateRecordFields #-}
{-# Language UndecidableInstances #-}
module HBS2.Data.Types.Refs
  ( module HBS2.Data.Types.Refs
  , serialise
  ) where

import HBS2.Base58
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Proto.Types (Encryption)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude

import Codec.Serialise(serialise)
import Data.Data
import GHC.Generics
import Data.Hashable hiding (Hashed)
import Data.Maybe (fromMaybe)

newtype HashRef = HashRef { fromHashRef :: Hash HbSync }
                  deriving newtype (Eq,Ord,IsString,Pretty,Hashable)
                  deriving stock (Data,Generic,Show)


instance Pretty (AsBase58 HashRef) where
  pretty (AsBase58 x) = pretty x
  -- TODO: should be instance Pretty (AsBase58 (Hash HbSync))

instance FromStringMaybe HashRef where
  fromStringMay = fmap HashRef . fromStringMay

data HashRefObject = HashRefObject HashRef (Maybe HashRefMetadata)
  deriving stock (Data,Show,Generic)

newtype HashRefMetadata =
  HashRefMetadata HashRef
  deriving newtype (Eq,Ord,Pretty)
  deriving stock (Data,Show,Generic)

data HashRefType =
    HashRefMerkle HashRefObject
  | HashRefBlob   HashRefObject
  deriving stock (Data,Show,Generic)

data AnnotatedHashRef =
  AnnotatedHashRef (Maybe HashRef) HashRef
  deriving stock (Data,Show,Generic)

data SequentialRef =
  SequentialRef Integer AnnotatedHashRef
  deriving stock (Data,Show,Generic)

instance Serialise AnnotatedHashRef
instance Serialise SequentialRef
instance Serialise HashRef



type IsRefPubKey s =  ( Eq (PubKey 'Sign s)
                      , Serialise (PubKey 'Sign s)
                      , FromStringMaybe (PubKey 'Sign s)
                      , Hashable (PubKey 'Sign s)
                      )


