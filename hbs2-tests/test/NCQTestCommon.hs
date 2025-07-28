module NCQTestCommon where

import HBS2.Prelude
import HBS2.System.Logger.Simple.ANSI

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import System.IO.Temp as Temp
import Control.Monad.Trans.Cont
import UnliftIO

data TestEnv =
  TestEnv
  { testEnvDir :: FilePath
  }

runTest :: forall m a . MonadUnliftIO m => (TestEnv -> m a) -> RunM C m a
runTest action = do
  pref <- lookupValueDef nil "test:root" >>= \case
            StringLike dir -> pure dir
            _              -> pure "/tmp/ncq-tests"

  keep <- lookupValueDef nil "test:dir:keep" >>= \case
            LitBoolVal True -> pure True
            _               -> pure False

  mkdir pref

  tmp <- liftIO (Temp.createTempDirectory pref "ncq-test")
  SC.bind "test:dir"  (mkStr tmp)

  flip runContT pure do
    ContT $ bracket none $ const do
      unless keep (rm tmp)
      flushLoggers

    lift $ lift $ action (TestEnv tmp)


setupLogger :: MonadIO m => m ()
setupLogger = do
  setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE

