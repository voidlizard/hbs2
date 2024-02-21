{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.System.Logger.Simple.ANSI
  ( module HBS2.System.Logger.Simple.Class
  , trace
  , debug
  , err
  , warn
  , notice
  , info
  , writeLog
  , AnsiStyle
  , ToLogStr(..)
  -- , INFO,NOTICE,WARN,ERROR,DEBUG
  , setLogging,setLoggingOff
  , toStderr,toStdout,logPrefix,defLog
  , SetLoggerEntry
  , withSimpleLogger
  , HasLogLevel(..)
  ) where

import Prettyprinter.Render.Terminal
import HBS2.System.Logger.Simple.Class
import HBS2.System.Logger.Simple qualified as Logger
import HBS2.System.Logger.Simple ( setLoggingOff
                                 , setLogging
                                 , toStderr
                                 , toStdout
                                 , logPrefix
                                 , defLog
                                 , SetLoggerEntry
                                 , writeLog
                                 , withSimpleLogger
                                 )
import Control.Monad.IO.Class
import Prettyprinter
import System.Log.FastLogger

instance ToLogStr (Doc AnsiStyle) where
  toLogStr = toLogStr . renderStrict . layoutPretty defaultLayoutOptions

trace :: forall m . MonadIO m => Doc AnsiStyle -> m ()
trace = Logger.trace @AnsiStyle

debug :: forall m . MonadIO m => Doc AnsiStyle -> m ()
debug = Logger.debug @AnsiStyle

warn :: forall m. MonadIO m => Doc AnsiStyle -> m ()
warn = Logger.warn @AnsiStyle

err :: forall m . MonadIO m => Doc AnsiStyle -> m ()
err = Logger.err @AnsiStyle

notice :: forall m . MonadIO m => Doc AnsiStyle -> m ()
notice = Logger.notice @AnsiStyle

info :: MonadIO m => Doc AnsiStyle -> m ()
info = Logger.info @AnsiStyle


