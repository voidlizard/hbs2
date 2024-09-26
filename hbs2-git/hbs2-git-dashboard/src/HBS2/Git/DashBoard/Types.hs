{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module HBS2.Git.DashBoard.Types
  ( module HBS2.Git.DashBoard.Types
  , module HBS2.Git.Data.Tx.Index
  ) where

import HBS2.Git.DashBoard.Prelude

import HBS2.Git.Data.Tx.Index

import HBS2.Net.Messaging.Unix

import DBPipe.SQLite

import HBS2.System.Dir

import System.FilePath

import Data.Word

data HttpPortOpt

data DevelopAssetsOpt

instance HasCfgKey HttpPortOpt a where
  key = "port"


instance HasCfgKey DevelopAssetsOpt a where
  key = "develop-assets"

data RunDashBoardOpts = RunDashBoardOpts
  { configPath :: Maybe FilePath }

instance Monoid  RunDashBoardOpts where
  mempty = RunDashBoardOpts Nothing

instance Semigroup RunDashBoardOpts where
  (<>) _ b = RunDashBoardOpts { configPath = configPath b }


data DashBoardEnv =
  DashBoardEnv
  { _peerAPI            :: ServiceCaller PeerAPI UNIX
  , _refLogAPI          :: ServiceCaller RefLogAPI UNIX
  , _refChanAPI         :: ServiceCaller RefChanAPI UNIX
  , _lwwRefAPI          :: ServiceCaller LWWRefAPI UNIX
  , _sto                :: AnyStorage
  , _dataDir            :: FilePath
  , _db                 :: DBPipeEnv
  , _pipeline           :: TQueue (IO ())
  , _dashBoardHttpPort  :: TVar (Maybe Word16)
  , _dashBoardDevAssets :: TVar (Maybe FilePath)
  }

makeLenses 'DashBoardEnv

repoDataPath  :: (DashBoardPerks m, MonadReader DashBoardEnv m) => LWWRefKey 'HBS2Basic -> m FilePath
repoDataPath lw = asks _dataDir <&> (</> (show $ pretty lw)) >>= canonicalizePath

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

newDashBoardEnv :: MonadIO m
                => FilePath
                -> ServiceCaller PeerAPI UNIX
                -> ServiceCaller RefLogAPI UNIX
                -> ServiceCaller RefChanAPI UNIX
                -> ServiceCaller LWWRefAPI UNIX
                -> AnyStorage
                -> m DashBoardEnv
newDashBoardEnv dbFile peer rlog rchan lww sto  = do
  let ddir = takeDirectory dbFile
  DashBoardEnv peer rlog rchan lww sto ddir
        <$> newDBPipeEnv dbPipeOptsDef dbFile
        <*> newTQueueIO
        <*> newTVarIO (Just 8911)
        <*> newTVarIO Nothing

getHttpPortNumber :: (MonadIO m, MonadReader DashBoardEnv m, Integral a) => m a
getHttpPortNumber = do
  asks _dashBoardHttpPort
    >>= readTVarIO
    <&> fromIntegral . fromMaybe 8911

getDevAssets :: (MonadIO m, MonadReader DashBoardEnv m, Integral a) => m (Maybe FilePath)
getDevAssets = do
  asks _dashBoardDevAssets
    >>= readTVarIO

withDashBoardEnv :: Monad m => DashBoardEnv -> DashBoardM m a -> m a
withDashBoardEnv env m = runReaderT (fromDashBoardM m) env

withState :: (MonadIO m, MonadReader DashBoardEnv m) => DBPipeM m a -> m a
withState f = do
  asks _db >>= flip withDB f


addJob :: (DashBoardPerks m, MonadReader DashBoardEnv m) => IO () -> m ()
addJob f = do
  q <- asks _pipeline
  atomically $ writeTQueue q f

