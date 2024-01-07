{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
module HBS2.Share.App.Types
  ( module HBS2.Share.App.Types
  , module HBS2.Data.Types.Refs
  , module Data.Config.Suckless
  , module HBS2.Peer.RPC.API.Peer
  , module HBS2.Peer.RPC.API.Storage
  , module HBS2.Peer.RPC.API.RefChan
  , module UnliftIO
  , module Control.Monad.Trans.Cont
  , module Control.Monad.Reader
  , module Lens.Micro.Platform
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.Service
import HBS2.Net.Auth.Credentials

import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.API.RefChan

import Data.Config.Suckless
import DBPipe.SQLite

import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Data.Maybe
import Lens.Micro.Platform
import UnliftIO

newtype RChan = RChan { toRefChanId :: RefChanId L4Proto }

deriving newtype instance  FromStringMaybe RChan

instance Pretty RChan where
  pretty (RChan x) = pretty (AsBase58 x)

instance IsString RChan where
  fromString s = fromMaybe (error "invalid refchan") $ fromStringMay s

data RpcEndpoints e =
  RpcEndpoints
  { rpcPeer     :: ServiceCaller PeerAPI e
  , rpcStorage  :: ServiceCaller StorageAPI e
  , rpcRefChan  :: ServiceCaller RefChanAPI e
  }


data EncryptionStuff =
  EncryptionStuff
  { _creds :: PeerCredentials HBS2Basic
  , _kre   :: KeyringEntry HBS2Basic
  }

makeLenses ''EncryptionStuff


data AppOption = AppDontPostOpt
               | AppDebugOpt
               | AppTraceOpt
               | AppReplicaOpt
                 deriving stock (Eq,Ord,Show,Data,Generic)

data AppEnv =
  AppEnv
  { _appOpts        :: [AppOption]
  , _appConf        :: [Syntax C]
  , _appRefChan     :: RChan
  , _appDb          :: DBPipeEnv
  , _appWorkDir     :: FilePath
  , _appRpcSock     :: FilePath
  , _appEnc         :: IORef (Maybe EncryptionStuff)
  }

makeLenses ''AppEnv


newtype ShareCLI m a = ShareCLI { fromShareCLI :: ReaderT AppEnv m a }
                       deriving newtype
                       ( Applicative
                       , Functor
                       , Monad
                       , MonadIO
                       , MonadUnliftIO
                       , MonadReader AppEnv
                       )

type AppPerks m = MonadUnliftIO m

instance (Monad m) => HasConf (ShareCLI m) where
  getConf = asks (view appConf)

instance Monad m => HasConf (ContT a (ShareCLI m)) where
  getConf = lift getConf


-- instance FromField HashRef


