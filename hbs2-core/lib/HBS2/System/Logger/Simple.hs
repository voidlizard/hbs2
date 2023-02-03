{-# Language UndecidableInstances #-}
module HBS2.System.Logger.Simple
  ( withSimpleLogger
  , debug
  ) where

import Control.Monad
import Data.Foldable
import Control.Monad.IO.Class
import System.Log.FastLogger
import System.Log.FastLogger.LoggerSet
import Data.IORef
import System.IO.Unsafe
import Prettyprinter

loggers :: IORef (Maybe LoggerSet)
loggers = unsafePerformIO (newIORef Nothing)
{-# NOINLINE loggers #-}


withSimpleLogger :: IO () -> IO ()
withSimpleLogger program = do
  set <- newStdoutLoggerSet 10000
  void $ atomicModifyIORef' loggers $ \case
    Nothing -> (Just set, Just set)
    Just s  -> (Just s, Just s)
  program
  withLogger flushLogStr

withLogger :: MonadIO m => (LoggerSet -> m b) -> m ()
withLogger f = do
  lo <- liftIO $ readIORef loggers
  forM_ lo f

debug :: (MonadIO m, ToLogStr a) => a -> m ()
debug s = do
  liftIO $ withLogger $ \set -> pushLogStrLn set (toLogStr s)


instance {-# OVERLAPPABLE #-} Pretty a => ToLogStr a where
  toLogStr p = toLogStr (show (pretty p))


instance {-# OVERLAPPABLE #-} ToLogStr (Doc ann) where
  toLogStr p = toLogStr (show p)

