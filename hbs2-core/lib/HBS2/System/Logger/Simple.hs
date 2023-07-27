{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TypeFamilyDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.System.Logger.Simple
  ( withSimpleLogger
  , trace
  , debug
  , log
  , err
  , warn
  , notice
  , info
  , setLogging, setLoggingOff
  , defLog
  , loggerTr
  , toStderr
  , toStdout
  , logPrefix
  , SetLoggerEntry
  , module HBS2.System.Logger.Simple.Class
  , ToLogStr(..)
  ) where

import HBS2.System.Logger.Simple.Class

import Prelude hiding (log)
import Data.Functor
import Data.Foldable(for_)
import Control.Monad.IO.Class
import System.Log.FastLogger
import Data.IORef
import System.IO.Unsafe
import Prettyprinter
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Lens.Micro.Platform

data LoggerType = LoggerStdout
                | LoggerStderr
                | LoggerNull

data LoggerEntry =
  LoggerEntry
  { _loggerSet  :: !LoggerSet
  , _loggerTr   :: LogStr -> LogStr
  , _loggerType :: !LoggerType
  }

makeLenses 'LoggerEntry

defLog :: a -> a
defLog = id

{-# OPTIONS_GHC -fno-cse #-}
{-# NOINLINE loggers #-}
loggers :: IORef (IntMap LoggerEntry)
loggers = unsafePerformIO $ newIORef mempty

withSimpleLogger :: IO () -> IO ()
withSimpleLogger program = do
  void program
  lo <- readIORef loggers <&> IntMap.elems
  for_ lo (flushLogStr . view loggerSet)

type SetLoggerEntry  = ( LoggerEntry -> LoggerEntry )

delLogger :: forall m . MonadIO m => Maybe LoggerEntry -> m ()
delLogger e =
  case view loggerSet <$> e of
    Nothing -> pure ()
    Just s  -> liftIO $ rmLoggerSet s

toStderr :: SetLoggerEntry
toStderr = set loggerType LoggerStderr

toStdout :: SetLoggerEntry
toStdout = set loggerType LoggerStdout

setLogging :: forall a m . (MonadIO m, HasLogLevel a)
           => (LoggerEntry -> LoggerEntry)
           -> m ()

setLogging f = do
  se <- liftIO $ newStdoutLoggerSet 10000 -- FIXME: ??
  def <- updateLogger $ f (LoggerEntry se id LoggerNull)
  let key = logKey @a
  e <- liftIO $ atomicModifyIORef' loggers (\x -> (IntMap.insert key def x, IntMap.lookup key x))
  delLogger e

  where
    updateLogger e = case view loggerType e of

      LoggerNull -> pure e

      LoggerStderr -> do
        delLogger (Just e)
        se <- liftIO $ newStderrLoggerSet 10000 -- FIXME: ??
        pure $ set loggerSet se e

      LoggerStdout -> do
        delLogger (Just e)
        se <- liftIO $ newStdoutLoggerSet 10000 -- FIXME: ??
        pure $ set loggerSet se e


setLoggingOff :: forall a m . (MonadIO m, HasLogLevel a) => m ()
setLoggingOff = do
  let key = logKey @a
  e <- liftIO $ atomicModifyIORef' loggers (\x -> (IntMap.delete key x, IntMap.lookup key x))
  delLogger e

withLogger :: forall a m . (HasLogLevel a, MonadIO m) => (LoggerEntry -> m ()) -> m ()
withLogger f = do
  lo <- liftIO $ readIORef loggers <&> IntMap.lookup (logKey @a)
  maybe (pure ()) f lo

log :: forall a s m . (MonadIO m, HasLogLevel a, ToLogStr s) => s -> m ()
log s = liftIO $ withLogger @a
               $ \le -> pushLogStrLn (view loggerSet le)
                                     (view loggerTr le (toLogStr s))


trace :: (MonadIO m, ToLogStr a) => a -> m ()
trace = log @TRACE

debug :: (MonadIO m, ToLogStr a) => a -> m ()
debug = log @DEBUG

warn :: (MonadIO m, ToLogStr a) => a -> m ()
warn = log @WARN

err :: (MonadIO m, ToLogStr a) => a -> m ()
err = log @ERROR

notice :: (MonadIO m, ToLogStr a) => a -> m ()
notice = log @NOTICE

info :: (MonadIO m, ToLogStr a) => a -> m ()
info = log @INFO

-- instance {-# OVERLAPPABLE #-} Pretty a => ToLogStr a where
--   toLogStr p = toLogStr (show (pretty p))

instance {-# OVERLAPPABLE #-} ToLogStr (Doc ann) where
  toLogStr p = toLogStr (show p)

logPrefix :: LogStr -> LoggerEntry-> LoggerEntry
logPrefix s = set loggerTr (s <>)
