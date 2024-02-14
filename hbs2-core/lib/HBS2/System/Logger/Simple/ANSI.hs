{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.System.Logger.Simple.ANSI
  ( trace
  , debug
  , err
  , warn
  , notice
  , info
  , AnsiStyle
  , ToLogStr(..)
  , INFO,NOTICE,WARN,ERROR,DEBUG,TRACE
  , setLogging,setLoggingOff
  , toStderr,toStdout,logPrefix,defLog
  ) where

import Prettyprinter.Render.Terminal
import HBS2.System.Logger.Simple qualified as Logger
import HBS2.System.Logger.Simple (INFO,NOTICE,WARN,ERROR,DEBUG,TRACE,setLoggingOff,setLogging,toStderr,toStdout,logPrefix,defLog)
import Control.Monad.IO.Class
import Prettyprinter
import System.Log.FastLogger

trace :: MonadIO m => Doc AnsiStyle -> m ()
trace = Logger.trace @(Doc AnsiStyle)

debug :: MonadIO m => Doc AnsiStyle -> m ()
debug = Logger.debug @(Doc AnsiStyle)

warn :: MonadIO m => Doc AnsiStyle -> m ()
warn = Logger.warn @(Doc AnsiStyle)

err :: MonadIO m => Doc AnsiStyle -> m ()
err = Logger.err @(Doc AnsiStyle)

notice :: MonadIO m => Doc AnsiStyle -> m ()
notice = Logger.notice @(Doc AnsiStyle)

info :: MonadIO m => Doc AnsiStyle -> m ()
info = Logger.info @(Doc AnsiStyle)

instance ToLogStr (Doc AnsiStyle) where
  toLogStr = toLogStr . renderStrict . layoutPretty defaultLayoutOptions
