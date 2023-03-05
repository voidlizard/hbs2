{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module GitRemoteTypes where

import HBS2.Prelude

import HBS2.Git.Types

import HBS2Git.Types
import HBS2Git.Config
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Set (Set)
import System.Exit as Exit

newtype RemoteEnv =
  RemoteEnv
  { _reHttpCat :: API
  }

makeLenses 'RemoteEnv

newtype GitRemoteApp m a =
  GitRemoteApp { fromRemoteApp :: ReaderT RemoteEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader RemoteEnv
                   )

runRemoteM :: MonadIO m => RemoteEnv -> GitRemoteApp m a -> m a
runRemoteM env m = runReaderT (fromRemoteApp m) env

instance MonadIO m => HasCatAPI (GitRemoteApp m) where
  getHttpCatAPI = view (asks reHttpCat)


exitSuccess :: MonadIO m => m ()
exitSuccess = liftIO Exit.exitSuccess

exitFailure :: MonadIO m => m ()
exitFailure = liftIO Exit.exitFailure

die :: MonadIO m => String -> m a
die s = do
  liftIO $ Exit.die s

