module HBS2.KeyMan.App.Types
  ( module HBS2.KeyMan.App.Types
  , module HBS2.Base58
  , module HBS2.Net.Proto.Types
  , module Lens.Micro.Platform
  , module Prettyprinter
  ) where

import HBS2.KeyMan.Prelude
import HBS2.KeyMan.Config
import HBS2.KeyMan.State

import HBS2.Prelude
import HBS2.Base58
import HBS2.Storage

-- FIXME: remove-this
import HBS2.Net.Auth.Credentials()
import HBS2.Net.Proto.Types

import HBS2.System.Logger.Simple.ANSI
import Data.Config.Suckless
import DBPipe.SQLite

import Control.Monad.Cont
import Control.Monad.Reader
import Prettyprinter
import Lens.Micro.Platform
import UnliftIO

data StorageNotBound =
  StorageNotBound
  deriving stock (Show, Typeable)

instance Exception StorageNotBound


data AppEnv =
  AppEnv
  { appConf :: [Syntax C]
  , appDb   :: DBPipeEnv
  , appSto  :: TVar (Maybe AnyStorage)
  }

newtype KeyManCLI m a = KeyManCLI { fromKeyManCLI :: ReaderT AppEnv m a }
                        deriving newtype
                        ( Applicative
                        , Functor
                        , Monad
                        , MonadIO
                        , MonadUnliftIO
                        , MonadReader AppEnv
                        )

newAppEnv :: MonadUnliftIO m => m AppEnv
newAppEnv = do
  let dbOpts = dbPipeOptsDef
  AppEnv <$> readConfig
         <*> (getStatePath >>= newDBPipeEnv dbOpts)
         <*> newTVarIO Nothing

runApp :: MonadUnliftIO m => KeyManCLI m () -> m ()
runApp action = do
  env <- liftIO newAppEnv
  let db = appDb env

  setLogging @INFO   defLog
  setLogging @ERROR  (logPrefix "" . toStderr)
  setLogging @WARN   (logPrefix "" . toStdout)
  setLogging @NOTICE (logPrefix "" . toStdout)
  setLogging @DEBUG  (logPrefix "" . toStderr)

  setLoggingOff @TRACE

  flip runContT pure $ do
    void $ ContT $ bracket (async (runPipe db)) cancel
    lift $ withAppEnv env do
      withState populateState
      action

  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE


withAppEnv :: MonadIO m => AppEnv -> KeyManCLI m a -> m a
withAppEnv env action = do
  runReaderT (fromKeyManCLI action) env

withState :: (MonadReader AppEnv m, MonadIO m)
          => DBPipeM m b
          -> m b

withState m = do
  d <- asks appDb
  withDB d m

instance MonadIO m => HasConf (ReaderT AppEnv m) where
  getConf = asks appConf

instance MonadIO m => HasConf (KeyManCLI m) where
  getConf = asks appConf

-- instance MonadIO m => HasStorage (KeyManCLI m) where
--   getStorage = do
--     msto <- asks appSto >>= readTVarIO
--     case msto of
--       Just x -> pure x
--       Nothing -> do
--         throwIO StorageNotBound

