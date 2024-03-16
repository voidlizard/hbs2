{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Git.Client.App.Types.GitEnv where

import HBS2.Git.Client.Prelude hiding (info)

import HBS2.Git.Client.Progress

import HBS2.Net.Auth.GroupKeySymm

import Data.Config.Suckless
import DBPipe.SQLite
import Data.HashMap.Strict (HashMap)

data ExportType = ExportNew
                | ExportFork HashRef
                | ExportInc
                  deriving stock (Eq,Ord,Generic,Show)

data ExportEncryption =
    ExportPublic
  | ExportPrivate FilePath
  deriving stock (Eq,Ord,Generic,Show)

type Config = [Syntax C]

class Monad m => HasProgressIndicator m where
  getProgressIndicator :: m AnyProgress

class HasAPI api proto m where
  getAPI :: m (ServiceCaller api proto)

data GitEnv =
  GitEnv
  { _gitTraceEnabled :: Bool
  , _gitDebugEnabled :: Bool
  , _gitApplyHeads   :: Bool
  , _gitExportType   :: ExportType
  , _gitExportEnc    :: ExportEncryption
  , _gitPath         :: FilePath
  , _configPath      :: FilePath
  , _config          :: Config
  , _peerAPI         :: ServiceCaller PeerAPI UNIX
  , _refLogAPI       :: ServiceCaller RefLogAPI UNIX
  , _lwwRefAPI       :: ServiceCaller LWWRefAPI UNIX
  , _storage         :: AnyStorage -- ServiceCaller StorageAPI UNIX
  , _db              :: DBPipeEnv
  , _progress        :: AnyProgress
  , _keyringCache    :: TVar (HashMap HashRef [KeyringEntry HBS2Basic])
  }

instance (Monad m, MonadReader GitEnv m) => HasProgressIndicator m where
  getProgressIndicator = asks _progress

instance MonadReader GitEnv m => HasStorage m where
  getStorage = asks _storage

instance MonadReader GitEnv m => HasAPI PeerAPI UNIX m where
  getAPI = asks _peerAPI

instance MonadReader GitEnv m => HasAPI LWWRefAPI UNIX m where
  getAPI = asks _lwwRefAPI

instance MonadReader GitEnv m => HasAPI RefLogAPI UNIX m where
  getAPI = asks _refLogAPI

makeLenses 'GitEnv
