module HBS2.Prelude
  ( module Data.String
  , module Safe
  , MonadIO(..)
  , void, guard, when, unless
  , maybe1
  , Hashable
  , lift
  , AsFileName(..)
  -- , Pretty
  , FromStringMaybe(..)
  , none
  , module Prettyprinter
  , ToByteString(..)
  , FromByteString(..)
  , Text.Text
  ) where

import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Safe
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void,guard,when,unless)
import Control.Monad.Trans.Class (lift)

import Data.Function
import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Hashable
import Prettyprinter
import Data.Word

none :: forall m . Monad m => m ()
none = pure ()

maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb


newtype AsFileName a = AsFileName a


instance Pretty a => Pretty (AsFileName a) where
  pretty (AsFileName f) = pretty x <> "@" <> uniq
    where
      uniq = pretty (fromIntegral $ hash (show (pretty f)) :: Word16)
      x = show (pretty f) & Text.pack
                          & Text.filter (not . Char.isPunctuation)

class FromStringMaybe a where
  fromStringMay :: String -> Maybe a

class ToByteString a where
  toByteString :: a -> ByteString

class FromByteString a where
  fromByteString :: ByteString -> Maybe a
