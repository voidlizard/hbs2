module HBS2.Data.Types
  ( module HBS2.Hash
  , module HBS2.Data.Types
  )
  where

import HBS2.Hash

import Codec.Serialise()
import Data.Data
import Data.String(IsString)
import GHC.Generics
import Prettyprinter

newtype HashRef = HashRef (Hash HbSync)
                  deriving newtype (Eq,Ord,IsString,Pretty)
                  deriving stock (Data,Generic)

instance Serialise HashRef

