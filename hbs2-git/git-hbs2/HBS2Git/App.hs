{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2Git.App
  ( module HBS2Git.App
  , module HBS2Git.Types
  )
  where

import HBS2.Prelude
import HBS2.System.Logger.Simple

import HBS2Git.Types
import HBS2Git.Config as Config
import HBS2Git.State

import Data.Config.Suckless

import Data.Set (Set)
import Data.Set  qualified as Set
-- import System.FilePath
import Control.Monad.Reader
import Lens.Micro.Platform
import System.Directory
import System.FilePath

logPrefix s = set loggerTr (s <>)

tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix ""

instance HasCfgKey ConfBranch (Set String) where
  key = "branch"

shutUp :: MonadIO m => m ()
shutUp = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE

data WithLog = NoLog | WithLog

runApp :: MonadIO m => WithLog -> App m () -> m ()
runApp l m = do

  case l of
    NoLog   -> pure ()
    WithLog -> do
      setLogging @DEBUG  debugPrefix
      setLogging @ERROR  errorPrefix
      setLogging @NOTICE noticePrefix
      setLogging @TRACE  tracePrefix

  (pwd, syn) <- Config.configInit

  home <- liftIO getHomeDirectory
  xdgstate <- liftIO $ getXdgDirectory XdgData Config.appName

  let statePath = xdgstate </> makeRelative home pwd

  let dbPath = statePath </> "state.db"

  db <- dbEnv dbPath

  trace $ "state" <+> pretty statePath

  here <- liftIO $ doesDirectoryExist statePath

  unless here do
    liftIO $ createDirectoryIfMissing True statePath

  withDB db stateInit

  let env = AppEnv pwd (pwd </> ".git") syn dbPath db
  runReaderT (fromApp m) env

  debug $ vcat (fmap pretty syn)

  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE

