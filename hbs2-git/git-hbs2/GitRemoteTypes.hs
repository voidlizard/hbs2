{-# Language TemplateHaskell #-}
module GitRemoteTypes where

import HBS2.Prelude

import HBS2Git.Types
import Control.Monad.Reader
import Lens.Micro.Platform


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

