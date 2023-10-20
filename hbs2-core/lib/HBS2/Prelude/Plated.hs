module HBS2.Prelude.Plated
  ( module HBS2.Prelude.Plated
  , module HBS2.Prelude
  , module Data.Data
  , module Data.Generics.Uniplate.Operations
  , Generic
  ) where

import Data.Data
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import GHC.Generics()
import Safe

import HBS2.Prelude

uniLastMay :: forall to from . (Data from, Data to) => from -> Maybe to
uniLastMay = lastMay . universeBi

uniLastDef :: forall from to . (Data from, Data to) => to -> from -> to
uniLastDef d = lastDef d . universeBi

uniFirstMay :: forall to from . (Data from, Data to) => from -> Maybe to
uniFirstMay = headMay . universeBi

uniFirstDef :: forall from to . (Data from, Data to) => to -> from -> to
uniFirstDef d = headDef d . universeBi


