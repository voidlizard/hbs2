{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Git.Client.App.Types.GitEnv where

import HBS2.Git.Client.Prelude hiding (info)

import HBS2.Git.Client.Progress

import HBS2.Git.Data.GK

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
  | ExportPrivateGK GK0
  deriving stock (Eq)

type Config = [Syntax C]

class Monad m => HasProgressIndicator m where
  getProgressIndicator :: m AnyProgress

class HasAPI api proto m where
  getAPI :: m (ServiceCaller api proto)

data ManifestUpdateEnv =
  ManifestUpdateEnv
  {  _manifest :: (Text, Text, Maybe Text)
  }

data GitEnv =
  GitEnv
  { _gitTraceEnabled      :: Bool
  , _gitDebugEnabled      :: Bool
  , _gitApplyHeads        :: Bool
  , _gitExportType        :: ExportType
  , _gitExportEnc         :: ExportEncryption
  , _gitManifestUpdateEnv :: Maybe ManifestUpdateEnv
  , _gitPath              :: FilePath
  , _configPath           :: FilePath
  , _config               :: Config
  , _peerAPI              :: ServiceCaller PeerAPI UNIX
  , _refLogAPI            :: ServiceCaller RefLogAPI UNIX
  , _refChanAPI           :: ServiceCaller RefChanAPI UNIX
  , _lwwRefAPI            :: ServiceCaller LWWRefAPI UNIX
  , _storage              :: AnyStorage -- ServiceCaller StorageAPI UNIX
  , _db                   :: DBPipeEnv
  , _progress             :: AnyProgress
  , _keyringCache         :: TVar (HashMap HashRef [KeyringEntry 'HBS2Basic])
  }


makeLenses 'GitEnv
