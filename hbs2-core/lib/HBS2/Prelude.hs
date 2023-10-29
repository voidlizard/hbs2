module HBS2.Prelude
  ( module Data.String
  , module Safe
  , module X
  , MonadIO(..)
  , void, guard, when, unless
  , maybe1
  , eitherToMaybe
  , ToMPlus(..)
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
  , (&), (<&>), for_, for
  ) where

import Data.Typeable as X
import GHC.Generics as X (Generic)

import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Safe
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (guard,when,unless,MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe


import Data.Foldable(for_)
import Data.Traversable(for)
import Data.Kind
import Data.Function
import Data.Functor
import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Hashable
import Prettyprinter
import Data.Word
import GHC.Generics
import Data.Time.Clock (NominalDiffTime(..))
import Codec.Serialise

import UnliftIO
import Control.Monad.IO.Unlift

none :: forall m . Monad m => m ()
none = pure ()

maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- deriving instance Generic NominalDiffTime
-- instance Serialise NominalDiffTime

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


class MonadPlus m => ToMPlus m a  where
  type family ToMPlusResult a :: Type
  toMPlus  :: a -> m (ToMPlusResult a)

instance Monad m => ToMPlus (MaybeT m) (Maybe a) where
  type instance ToMPlusResult (Maybe a) = a
  toMPlus Nothing = mzero
  toMPlus (Just a) = MaybeT (pure (Just a))

instance Monad m => ToMPlus (MaybeT m) (Either x a) where
  type instance ToMPlusResult (Either x a) = a
  toMPlus (Left{}) = mzero
  toMPlus (Right x) = MaybeT $ pure (Just x)

