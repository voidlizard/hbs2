{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TypeFamilyDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.System.Logger.Simple
  ( withSimpleLogger
  , debug
  , log
  , err
  , warn
  , notice
  , info
  , setLogging
  , defLog
  , loggerTr
  , module HBS2.System.Logger.Simple.Class
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

data LoggerEntry =
  LoggerEntry
  { _loggerSet  :: !LoggerSet
  , _loggerTr   :: LogStr -> LogStr
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

setLogging :: forall a m . (MonadIO m, HasLogLevel a)
           => (LoggerEntry -> LoggerEntry)
           -> m ()

setLogging f = do
  se <- liftIO $ newStdoutLoggerSet 10000 -- FIXME: ??
  let def = f (LoggerEntry se id)
  let key = logKey @a
  void $ liftIO $ atomicModifyIORef' loggers (\x -> (IntMap.insert key def x, ()))

withLogger :: forall a m . (HasLogLevel a, MonadIO m) => (LoggerEntry -> m ()) -> m ()
withLogger f = do
  lo <- liftIO $ readIORef loggers <&> IntMap.lookup (logKey @a)
  maybe (pure ()) f lo

log :: forall a s m . (MonadIO m, HasLogLevel a, ToLogStr s) => s -> m ()
log s = liftIO $ withLogger @a
               $ \le -> pushLogStrLn (view loggerSet le)
                                     (view loggerTr le (toLogStr s))

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

