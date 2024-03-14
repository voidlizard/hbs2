{-# Language TemplateHaskell #-}
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

makeLenses 'GitEnv
