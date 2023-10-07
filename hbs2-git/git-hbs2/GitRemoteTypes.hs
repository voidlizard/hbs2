{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module GitRemoteTypes where

import HBS2.Prelude
import HBS2.OrDie
import HBS2.Net.Auth.Credentials (PeerCredentials)
import HBS2.Net.Proto.Definition()
import HBS2.Peer.RPC.Client.StorageClient

import HBS2Git.Types
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.Trans.Resource

data RemoteEnv =
  RemoteEnv
  { _reCreds :: TVar (HashMap RepoRef (PeerCredentials Schema))
  , _reRpc   :: RPCEndpoints
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
                   , MonadUnliftIO
                   , MonadMask
                   , MonadTrans
                   )

instance Monad m => HasStorage (GitRemoteApp m) where
  getStorage =  asks (rpcStorage . view reRpc) <&> AnyStorage . StorageClient

instance Monad m => HasRPC (GitRemoteApp m) where
  getRPC =  asks (view reRpc)

runRemoteM :: MonadIO m => RemoteEnv -> GitRemoteApp m a -> m a
runRemoteM env m = runReaderT (fromRemoteApp m) env

instance MonadIO m => HasRefCredentials (GitRemoteApp m) where

  setCredentials ref cred = do
    asks (view reCreds) >>= \t -> liftIO $ atomically $
      modifyTVar' t (HashMap.insert ref cred)

  getCredentials ref = do
    hm <- asks (view reCreds) >>= liftIO . readTVarIO
    pure (HashMap.lookup ref hm) `orDie` "keyring not set"




