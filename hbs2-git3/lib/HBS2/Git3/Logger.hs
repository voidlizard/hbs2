{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Logger ( setupLogger
                        , flushLoggers
                        , silence
                        , debugPrefix
                        , status, setStatusOn, STATUS
                        ) where

import HBS2.Prelude
import HBS2.System.Logger.Simple.ANSI as Logger

data STATUS

-- debugPrefix :: LoggerEntry -> LoggerEntry
-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStderr . logPrefix ""
  setLogging @INFO   $ toStderr . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @INFO

instance HasLogLevel STATUS where
  type instance LogLevel STATUS = 10

status :: forall a m . (MonadIO m) => Doc a ->  m ()
status = Logger.writeLog @STATUS . show

setStatusOn :: MonadIO m => m ()
setStatusOn = do
  setLogging @STATUS $ toStderr . logPrefix ""


