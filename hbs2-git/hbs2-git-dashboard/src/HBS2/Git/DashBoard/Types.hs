{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.DashBoard.Types
  ( module HBS2.Git.DashBoard.Types
  , module HBS2.Git.Data.Tx.Index
  ) where

import HBS2.Prelude.Plated

import HBS2.Git.Data.Tx.Index

import HBS2.Net.Proto.Service
import HBS2.Storage
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Net.Messaging.Unix

import Data.Config.Suckless

import DBPipe.SQLite
import Control.Monad.Reader

import UnliftIO

data HttpPortOpt

data DevelopAssetsOpt

instance HasConf m => HasCfgKey HttpPortOpt a m where
  key = "port"


instance HasConf m => HasCfgKey DevelopAssetsOpt a m where
  key = "develop-assets"

data RunDashBoardOpts = RunDashBoardOpts
  { configPath :: Maybe FilePath }

instance Monoid  RunDashBoardOpts where
  mempty = RunDashBoardOpts Nothing

instance Semigroup RunDashBoardOpts where
  (<>) _ b = RunDashBoardOpts { configPath = configPath b }


data DashBoardEnv =
  DashBoardEnv
  { _peerAPI        :: ServiceCaller PeerAPI UNIX
  , _refLogAPI      :: ServiceCaller RefLogAPI UNIX
  , _refChanAPI     :: ServiceCaller RefChanAPI UNIX
  , _lwwRefAPI      :: ServiceCaller LWWRefAPI UNIX
  , _sto            :: AnyStorage
  , _dashBoardConf  :: TVar [Syntax C]
  , _db             :: DBPipeEnv
  , _pipeline       :: TQueue (IO ())
  }

type DashBoardPerks m = MonadUnliftIO m

newtype DashBoardM m a = DashBoardM { fromDashBoardM :: ReaderT DashBoardEnv m a }
                         deriving newtype
                         ( Applicative
                         , Functor
                         , Monad
                         , MonadIO
                         , MonadUnliftIO
                         , MonadTrans
                         , MonadReader DashBoardEnv
                         )

instance (MonadIO m, Monad m, MonadReader DashBoardEnv m) => HasConf m where
  getConf = do
    asks _dashBoardConf >>= readTVarIO

newDashBoardEnv :: MonadIO m
                => [Syntax C]
                -> FilePath
                -> ServiceCaller PeerAPI UNIX
                -> ServiceCaller RefLogAPI UNIX
                -> ServiceCaller RefChanAPI UNIX
                -> ServiceCaller LWWRefAPI UNIX
                -> AnyStorage
                -> m DashBoardEnv
newDashBoardEnv cfg dbFile peer rlog rchan lww sto  = do
  DashBoardEnv peer rlog rchan lww sto
        <$> newTVarIO cfg
        <*> newDBPipeEnv dbPipeOptsDef dbFile
        <*> newTQueueIO

withDashBoardEnv :: Monad m => DashBoardEnv -> DashBoardM m a -> m a
withDashBoardEnv env m = runReaderT (fromDashBoardM m) env

withState :: (MonadIO m, MonadReader DashBoardEnv m) => DBPipeM m a -> m a
withState f = do
  asks _db >>= flip withDB f


addJob :: (DashBoardPerks m, MonadReader DashBoardEnv m) => IO () -> m ()
addJob f = do
  q <- asks _pipeline
  atomically $ writeTQueue q f

