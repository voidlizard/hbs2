module HBS2.Data.Types.Refs
  ( module HBS2.Data.Types.Refs
  , serialise
  ) where

import HBS2.Hash

import Codec.Serialise(serialise)
import Data.Data
import Data.String(IsString)
import GHC.Generics
import Prettyprinter

newtype HashRef = HashRef (Hash HbSync)
                  deriving newtype (Eq,Ord,IsString,Pretty)
                  deriving stock (Data,Generic,Show)



data HashRefObject = HashRefObject HashRef (Maybe HashRefMetadata)
  deriving stock (Data,Generic)

newtype HashRefMetadata =
  HashRefMetadata HashRef
  deriving newtype (Eq,Ord,Pretty)
  deriving stock (Data,Generic)

newtype HashRefPrevState = HashRefPrevState HashRef
  deriving newtype (Eq,Ord,Pretty,IsString)
  deriving stock (Data,Generic)

data HashRefType =
    HashRefMerkle HashRefObject
  | HashRefBlob   HashRefObject
  deriving stock (Data,Generic)

data AnnotatedHashRef =
  AnnotatedHashRef (Maybe HashRefPrevState) HashRefType
  deriving stock (Data,Generic)


instance Serialise AnnotatedHashRef
instance Serialise HashRef
instance Serialise HashRefMetadata
instance Serialise HashRefObject
instance Serialise HashRefPrevState
instance Serialise HashRefType

