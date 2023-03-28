{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module GitRemoteTypes where

import HBS2.Prelude
import HBS2.OrDie
import HBS2.Net.Auth.Credentials (PeerCredentials)
import HBS2.Net.Proto.Definition()

import HBS2Git.Types
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Control.Concurrent.STM
import Control.Monad.Catch

data RemoteEnv =
  RemoteEnv
  { _reHttpCat :: API
  , _reHttpSize :: API
  , _reHttpPut  :: API
  , _reHttpRefGet  :: API
  , _reCreds :: TVar (HashMap RepoRef (PeerCredentials Schema))
  }

makeLenses 'RemoteEnv

newtype GitRemoteApp m a =
  GitRemoteApp { fromRemoteApp :: ReaderT RemoteEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader RemoteEnv
                   , MonadThrow
                   , MonadCatch
                   )

runRemoteM :: MonadIO m => RemoteEnv -> GitRemoteApp m a -> m a
runRemoteM env m = runReaderT (fromRemoteApp m) env

instance MonadIO m => HasCatAPI (GitRemoteApp m) where
  getHttpCatAPI = view (asks reHttpCat)
  getHttpSizeAPI = view (asks reHttpSize)
  getHttpPutAPI = view (asks reHttpPut)
  getHttpRefLogGetAPI = view (asks reHttpRefGet)

instance MonadIO m => HasRefCredentials (GitRemoteApp m) where

  setCredentials ref cred = do
    asks (view reCreds) >>= \t -> liftIO $ atomically $
      modifyTVar' t (HashMap.insert ref cred)

  getCredentials ref = do
    hm <- asks (view reCreds) >>= liftIO . readTVarIO
    pure (HashMap.lookup ref hm) `orDie` "keyring not set"




