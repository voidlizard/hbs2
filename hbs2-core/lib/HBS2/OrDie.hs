{-# Language AllowAmbiguousTypes #-}
module HBS2.OrDie
  ( module HBS2.OrDie
  ) where

import Data.Kind
import Control.Monad.IO.Class
import System.Exit
import Prettyprinter
import UnliftIO
import Control.Monad.Except

class OrDie m a where
  type family OrDieResult a :: Type
  orDie :: m a -> String -> m (OrDieResult a)

orDieM :: (Monad m, OrDie m a) => a -> String -> m (OrDieResult a)
orDieM a msg = pure a `orDie` msg

instance MonadIO m => OrDie m (Maybe a) where
  type instance OrDieResult (Maybe a) = a
  orDie mv err = mv >>= \case
      Nothing -> liftIO $ die err
      Just x  -> pure x

instance MonadIO m => OrDie m (Either a b) where
  type instance OrDieResult (Either a b) = b
  orDie mv err = mv >>= \case
      Left{} -> liftIO $ die err
      Right x  -> pure x

instance MonadIO m => OrDie m ExitCode where
  type instance OrDieResult ExitCode = ()
  orDie mv err = mv >>= \case
    ExitSuccess   -> pure ()
    ExitFailure{} -> liftIO $ die err


-- TODO: move-to-library
class OrThrow a  where
  type family OrThrowResult a :: Type
  orThrow :: forall e m  . (MonadIO m, Exception e) =>  e -> a -> m (OrThrowResult a)


instance OrThrow (Maybe a) where
  type instance OrThrowResult (Maybe a) = a
  orThrow e a = case a of
    Nothing -> throwIO e
    Just x  -> pure x

instance OrThrow (Either b a) where
  type instance OrThrowResult (Either b a) = a
  orThrow e a = case a of
    Left{}  -> throwIO e
    Right x -> pure x

class OrThrowError a where
  type family OrThrowErrorResult a :: Type
  orThrowError :: forall e m . (MonadError e m ) => e -> a -> m (OrThrowErrorResult a)

{- HLINT ignore "Eta reduce" -}
instance OrThrowError (Maybe a) where
  type instance (OrThrowErrorResult (Maybe a)) = a
  orThrowError e a = maybe (throwError e) pure a

instance OrThrowError (Either b a) where
  type instance (OrThrowErrorResult (Either b a)) = a
  orThrowError e a = either (const $ throwError e) pure a

orThrowUser :: (OrThrow a1, MonadIO m)
            => Doc ann
            -> a1
            -> m (OrThrowResult a1)

orThrowUser p = orThrow (userError (show p))

orThrowPassIO :: (MonadIO m, Exception e) => Either e a -> m a
orThrowPassIO = \case
  Left e -> throwIO e
  Right x -> pure x


