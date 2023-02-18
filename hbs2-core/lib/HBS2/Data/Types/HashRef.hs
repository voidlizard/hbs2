module HBS2.Data.Types.HashRef
  ( module HBS2.Data.Types.HashRef
  , serialise
  ) where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Base58

import Codec.Serialise(serialise)
import Data.Data
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

instance Serialise HashRef

