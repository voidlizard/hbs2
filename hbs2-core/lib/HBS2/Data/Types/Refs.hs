module HBS2.Data.Types.Refs
  ( module HBS2.Data.Types.Refs
  , serialise
  ) where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Base58

import Codec.Serialise(serialise)
import Data.Data
import Data.String(IsString)
import GHC.Generics
import Prettyprinter

newtype HashRef = HashRef { fromHashRef :: Hash HbSync }
                  deriving newtype (Eq,Ord,IsString,Pretty)
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

newtype HashRefPrevState = HashRefPrevState HashRef
  deriving newtype (Eq,Ord,Pretty,IsString)
  deriving stock (Data,Show,Generic)

data HashRefType =
    HashRefMerkle HashRefObject
  | HashRefBlob   HashRefObject
  deriving stock (Data,Show,Generic)

data AnnotatedHashRef =
  AnnotatedHashRef (Maybe HashRefPrevState) HashRefType
  deriving stock (Data,Show,Generic)


instance Serialise AnnotatedHashRef
instance Serialise HashRef
instance Serialise HashRefMetadata
instance Serialise HashRefObject
instance Serialise HashRefPrevState
instance Serialise HashRefType

