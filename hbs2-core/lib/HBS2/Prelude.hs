{-# Language FunctionalDependencies #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Prelude
  ( module Data.String
  , module Safe
  , module X
  , module Numeric.Natural
  , module HBS2.Clock
  , MonadIO(..), MonadPlus(..)
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
  , ByFirst(..)
  , Probe(..)
  , ProbeSnapshot(..)
  , ToProbeSnapshot(..)
  , ProbeSnapshotElement(..)
  , AnyProbe(..)
  , newSimpleProbe
  , whenTrue, whenFalse
  , dontHandle
  ) where

import HBS2.Clock

import Data.Typeable as X
import GHC.Generics as X (Generic)

import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Safe
import Control.Monad as X
import Control.Monad.Fix as X
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe


import Data.Foldable(for_)
import Data.Traversable(for)
import Data.Kind
import Data.Function
import Data.Functor
import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Hashable
import Data.HashMap.Strict(HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Prettyprinter
import Data.Word
import GHC.Generics
import Control.Monad.Except
import Numeric.Natural
import Streaming.Prelude qualified as S
import UnliftIO
import Codec.Serialise

none :: forall m . Monad m => m ()
none = pure ()

maybe1 :: Maybe a -> b -> (a -> b) -> b
maybe1 mb n j = maybe n j mb

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

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

whenTrue :: forall m b a . (Monad m) => b -> Bool -> m a -> (b -> m a) -> m a
whenTrue b f fallback continue = if f then continue b else fallback

whenFalse :: forall m b a . (Monad m) => b -> Bool -> m a -> (b -> m a) -> m a
whenFalse b f fallback continue = if not f then continue b else fallback

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


data ByFirst a b = ByFirst a b

instance Eq a => Eq (ByFirst a b) where
  (==) (ByFirst a _) (ByFirst b _) = a == b

instance Hashable a => Hashable (ByFirst a b) where
  hashWithSalt s (ByFirst a _) = hashWithSalt s a

class ToProbeSnapshot a => Probe a where
  acceptReport :: forall m . MonadIO m => a -> [(Text, Integer)] -> m ()

data ProbeSnapshotElement =
  ProbeSnapshotElement Text Integer
  deriving stock (Eq,Ord,Show,Generic)

instance Serialise ProbeSnapshotElement

instance Pretty ProbeSnapshotElement where
  pretty (ProbeSnapshotElement x y) = pretty x <+> pretty y

class ProbeSnapshot a where
  probeSnapshot :: MonadIO m => a -> m [ProbeSnapshotElement]

class ToProbeSnapshot a where
  toSnapshotElements :: forall m . MonadIO m => a -> m [ProbeSnapshotElement]

data SimpleProbe =
  SimpleProbe
  { spName        :: Text
  , spTimestamp   :: TVar Word64
  , spProbeValues :: TVar (HashMap Text Integer)
  }

instance ToProbeSnapshot SimpleProbe where
  toSnapshotElements SimpleProbe{..} = do
      vs <- readTVarIO spProbeValues <&> HM.toList
      pure [ ProbeSnapshotElement (spName <> "." <> n) i | (n,i) <- vs ]


instance ProbeSnapshot [AnyProbe] where
  probeSnapshot spx = do
    what <- S.toList_ do
      for_ spx $ \s ->  do
        toSnapshotElements s >>= S.each
    pure $ Set.toList $ Set.fromList what

newSimpleProbe :: forall m . MonadIO m => Text -> m AnyProbe
newSimpleProbe name = do
  s <- SimpleProbe name
        <$> (liftIO getPOSIXTime >>= newTVarIO . round)
        <*> newTVarIO mempty
  pure $ AnyProbe s

instance ToProbeSnapshot () where
  toSnapshotElements _ = pure mempty

instance Probe () where
  acceptReport _ _ = pure ()

data AnyProbe = forall a . Probe a => AnyProbe a

instance Probe AnyProbe where
  acceptReport (AnyProbe p) = acceptReport p

instance ToProbeSnapshot AnyProbe where
  toSnapshotElements (AnyProbe p) = toSnapshotElements p

instance Probe SimpleProbe where
  acceptReport SimpleProbe{..} values = do
    t <- liftIO getPOSIXTime <&> round
    atomically do
      writeTVar spTimestamp t
      old <- readTVar spProbeValues
      writeTVar spProbeValues (HM.fromList values <> old)


dontHandle :: Applicative f => a -> f ()
dontHandle = const $ pure ()

