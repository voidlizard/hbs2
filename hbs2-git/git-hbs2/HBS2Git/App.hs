{-# Language TemplateHaskell #-}
module HBS2Git.App where

import HBS2.Prelude
import HBS2.System.Logger.Simple

import HBS2Git.Config as Config

import System.FilePath
import Control.Monad.Reader
import Lens.Micro.Platform

data AppEnv =
  AppEnv
  { _appCurDir :: FilePath
  , _appGitDir :: FilePath
  }

makeLenses 'AppEnv

newtype App m a =
  App { fromApp :: ReaderT AppEnv m a }
  deriving newtype ( Applicative, Functor, Monad, MonadIO, MonadReader AppEnv )

logPrefix s = set loggerTr (s <>)

tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix ""

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix ""

runApp :: MonadIO m => App m () -> m ()
runApp m = do

  setLogging @DEBUG  debugPrefix
  setLogging @ERROR  errorPrefix
  setLogging @NOTICE noticePrefix
  setLogging @TRACE  tracePrefix

  pwd <- Config.configInit

  let env = AppEnv pwd (pwd </> ".git")

  runReaderT (fromApp m) env

  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE



