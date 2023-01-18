module HBS2.Prelude
  ( module Data.String
  , module Safe
  , MonadIO(..)
  , void, guard, when, unless
  , maybe1
  , Hashable
  , lift
  ) where

import Data.String (IsString(..))
import Safe
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void,guard,when,unless)
import Data.Hashable (Hashable)
import Control.Monad.Trans.Class (lift)

maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb

