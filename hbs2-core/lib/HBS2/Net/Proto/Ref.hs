{-# Language TemplateHaskell #-}
module HBS2.Net.Proto.Ref where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef)

import Lens.Micro.Platform
import Codec.Serialise
import Data.Set (Set)

data Ref = Ref
  { _refACB  :: HashRef
  , _refData :: HashRef
  }
  deriving stock (Generic,Eq)

makeLenses 'Ref


instance Serialise Ref

newtype RefSet = RefSet (Set Ref)

