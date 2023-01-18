module HBS2.Prelude
  ( module Data.String
  , module Safe
  , MonadIO(..)
  , void
  , maybe1
  ) where

import Data.String (IsString(..))
import Safe
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void)


maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb

