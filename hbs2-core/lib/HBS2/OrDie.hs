module HBS2.OrDie where

import Data.Kind
import Control.Monad.IO.Class
import System.Exit

class OrDie m a where
  type family OrDieResult a :: Type
  orDie :: m a -> String -> m (OrDieResult a)

instance OrDie IO (Maybe a) where
  type instance OrDieResult (Maybe a) = a
  orDie mv err = mv >>= \case
      Nothing -> die err
      Just x  -> pure x

instance OrDie IO (Either String a) where
  type instance OrDieResult (Either String a) = a
  orDie ev err = ev >>= \case
      Left e -> die (e <> ": " <> err)
      Right x  -> pure x

instance MonadIO m => OrDie m ExitCode where
  type instance OrDieResult ExitCode = ()
  orDie mv err = mv >>= \case
    ExitSuccess   -> pure ()
    ExitFailure{} -> liftIO $ die err
