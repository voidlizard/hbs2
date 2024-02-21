{-# Language FunctionalDependencies #-}
module HBS2.Prelude
  ( module Data.String
  , module Safe
  , module X
  , MonadIO(..)
  , void, guard, when, unless
  , maybe1
  , eitherToMaybe
  , asyncLinked
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
  , HasErrorStatus(..), ErrorStatus(..), SomeError(..), WithSomeError(..), mayE, someE
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
import Control.Monad.Except

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


data ErrorStatus = Complete
                 | HasIssuesButOkay
                 | Failed
                 | SNAFU
                 | Unknown
                 deriving stock (Eq,Ord,Show,Enum,Generic)

class HasErrorStatus e where
  getStatus :: e -> ErrorStatus

-- instance {-# OVERLAPPABLE #-} HasErrorStatus e where
--   getStatus _ = Unknown

data SomeError = forall e . (Show e, HasErrorStatus e) =>
  SomeError e

instance Show SomeError where
  show (SomeError x) = show x

instance HasErrorStatus SomeError where
  getStatus (SomeError e) = getStatus e

someE :: forall e . (Show e, HasErrorStatus e) => e -> SomeError
someE = SomeError

mayE :: forall e b . (Show e, HasErrorStatus e) => e -> b -> SomeError
mayE e _ = SomeError e

class WithSomeError m a b | a -> b where
  toSomeError :: (forall e . Show e => e -> SomeError) -> m a -> ExceptT SomeError m b


instance Monad m => WithSomeError m (Maybe a) a where
  toSomeError f m = do
    lift m >>= \case
      Nothing -> throwError (f ())
      Just v  -> pure v

instance (Monad m, Show e) => WithSomeError m (Either e a) a where
  toSomeError f m = do
    lift m >>= \case
      Left e  -> throwError (f e)
      Right v -> pure v


instance (MonadUnliftIO m, Exception e) => MonadUnliftIO (ExceptT e m) where
    withRunInIO exceptToIO = ExceptT $ try $ do
        withRunInIO $ \runInIO ->
            exceptToIO (runInIO . (either throwIO pure <=< runExceptT))


asyncLinked :: MonadUnliftIO m => m a -> m (Async a)
asyncLinked m = do
  l <- async m
  link l
  pure l


-- asyncLinked :: forall m . MonadUnliftIO m =>

