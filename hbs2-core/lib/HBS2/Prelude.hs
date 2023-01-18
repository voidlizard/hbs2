module HBS2.Prelude
  ( module Data.String
  , module Safe
  , MonadIO(..)
  , void, guard
  , maybe1
  , Hashable
  ) where

import Data.String (IsString(..))
import Safe
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void,guard)
import Data.Hashable (Hashable)


maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb

