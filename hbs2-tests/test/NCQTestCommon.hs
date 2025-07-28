module NCQTestCommon where

import HBS2.Prelude
import HBS2.System.Logger.Simple.ANSI

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import Data.ByteString (ByteString)
import Control.Monad.Trans.Cont
import Data.Fixed
import System.IO.Temp as Temp
import System.Random.Stateful
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


genRandomBS :: forall g m . (Monad m, StatefulGen g m) => g -> Int -> m ByteString
genRandomBS g n = do
  uniformByteStringM n g

sec6 :: RealFrac a => a -> Fixed E6
sec6 = realToFrac

sec2 :: RealFrac a => a -> Fixed E2
sec2 = realToFrac

sec3 :: RealFrac a => a -> Fixed E3
sec3 = realToFrac


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

