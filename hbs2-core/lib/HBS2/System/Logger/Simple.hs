{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TypeFamilyDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.System.Logger.Simple
  ( withSimpleLogger
  , trace
  , debug
  , writeLog
  , err
  , warn
  , notice
  , info
  , setLogging, setLoggingOff
  , defLog
  , loggerTr
  , toStderr
  , toStdout
  , toFile
  , logPrefix
  , SetLoggerEntry
  , module HBS2.System.Logger.Simple.Class
  , ToLogStr(..)
  ) where

import HBS2.System.Logger.Simple.Class

import Prelude hiding (log)
import Data.Functor
import Data.Foldable (for_)
import Control.Monad.IO.Class
import System.Log.FastLogger
import Data.IORef
import System.IO.Unsafe
import Prettyprinter
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Lens.Micro.Platform
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Control.Concurrent.STM

data LoggerType = LoggerStdout
                | LoggerStderr
                | LoggerFile FilePath
                | LoggerNull
  deriving (Eq, Ord)

data LoggerEntry =
  LoggerEntry
  { _loggerTr   :: LogStr -> LogStr
  , _loggerType :: !LoggerType
  }

makeLenses 'LoggerEntry

defLog :: a -> a
defLog = id

{-# OPTIONS_GHC -fno-cse #-}
{-# NOINLINE loggers #-}
loggers :: IORef (IntMap LoggerEntry)
loggers = unsafePerformIO $ newIORef mempty

data LoggerSetWrapper = LoggerSetWrapper
  { _loggerSet :: LoggerSet,
    _loggerSetUsedBy :: Int
  }

makeLenses 'LoggerSetWrapper

{-# OPTIONS_GHC -fno-cse #-}
{-# NOINLINE loggerSets #-}
loggerSets :: TVar (Map LoggerType LoggerSetWrapper)
loggerSets = unsafePerformIO $ newTVarIO mempty

withSimpleLogger :: IO () -> IO ()
withSimpleLogger program = do
  void program
  loggers' <- readIORef loggers <&> IntMap.elems
  loggerSets' <- readTVarIO loggerSets
  for_
    loggers'
    ( \loggerEntry -> do
        let loggerType' = view loggerType loggerEntry
        let maybeLoggerSet = Map.lookup loggerType' loggerSets'
        maybe (pure ()) (flushLogStr . view loggerSet) maybeLoggerSet
    )

type SetLoggerEntry  = ( LoggerEntry -> LoggerEntry )

delLoggerSet :: forall m . MonadIO m => LoggerType -> m ()
delLoggerSet loggerType' = do
  action <- liftIO $ atomically $ do
    loggerSets' <- readTVar loggerSets
    case Map.lookup loggerType' loggerSets' of
      Nothing -> pure $ pure ()
      Just loggerSet' -> do
        let usedBy = view loggerSetUsedBy loggerSet'
        if usedBy < 2
          then do
            modifyTVar' loggerSets (Map.delete loggerType')
            pure $ rmLoggerSet (view loggerSet loggerSet')
          else do
            modifyTVar' loggerSets (Map.adjust (over loggerSetUsedBy (\x -> x - 1)) loggerType')
            pure $ pure ()
  liftIO action

toStderr :: SetLoggerEntry
toStderr = set loggerType LoggerStderr

toStdout :: SetLoggerEntry
toStdout = set loggerType LoggerStdout

toFile :: FilePath -> SetLoggerEntry
toFile filePath = set loggerType (LoggerFile filePath)

createLoggerSet :: MonadIO m => LoggerType -> m ()
createLoggerSet loggerType' = liftIO $ do
  loggerSets' <- readTVarIO loggerSets
  if Map.member loggerType' loggerSets'
    then
      -- Increment `_loggerSetUsedBy` value if logger set of a given type already exist
      atomically $ modifyTVar' loggerSets (Map.adjust (over loggerSetUsedBy (+ 1)) loggerType')
    else do
      -- Otherwise create new logger set
      newLoggerSet' <- case loggerType' of
        LoggerStdout -> Just <$> newStdoutLoggerSet defaultBufSize
        LoggerStderr -> Just <$> newStderrLoggerSet defaultBufSize
        LoggerFile f -> Just <$> newFileLoggerSet defaultBufSize f
        LoggerNull -> pure Nothing
      case newLoggerSet' of
        Nothing -> pure ()
        Just loggerSet' ->
          atomically $ modifyTVar' loggerSets (Map.insert loggerType' $ LoggerSetWrapper loggerSet' 0)

setLogging :: forall a m . (MonadIO m, HasLogLevel a)
           => (LoggerEntry -> LoggerEntry)
           -> m ()
setLogging setLoggerEntry = do
  let key = logKey @a
      dummyLoggerEntry = LoggerEntry id LoggerStdout
      loggerEntry = setLoggerEntry dummyLoggerEntry
      loggerType' = view loggerType loggerEntry
  liftIO $ createLoggerSet loggerType'
  liftIO $ atomicModifyIORef' loggers (\x -> (IntMap.insert key loggerEntry x, ()))

setLoggingOff :: forall a m. (MonadIO m, HasLogLevel a) => m ()
setLoggingOff = do
  let key = logKey @a
  maybeLoggerEntry <- liftIO $ atomicModifyIORef' loggers (\x -> (IntMap.delete key x, IntMap.lookup key x))
  case maybeLoggerEntry of
    Nothing -> pure ()
    Just loggerEntry -> do
      let loggerType' = view loggerType loggerEntry
      delLoggerSet loggerType'

withLogger :: forall a m . (HasLogLevel a, MonadIO m) => (LoggerEntry -> m ()) -> m ()
withLogger f = do
  maybeLoggerEntry <- liftIO $ readIORef loggers <&> IntMap.lookup (logKey @a)
  maybe (pure ()) f maybeLoggerEntry

writeLog :: forall a s m . (MonadIO m, HasLogLevel a, ToLogStr s) => s -> m ()
writeLog s = liftIO $ withLogger @a $ \loggerEntry -> do
  loggerSets' <- readTVarIO loggerSets
  let loggerType' = view loggerType loggerEntry
      maybeLoggerSet = Map.lookup loggerType' loggerSets'
      msg = view loggerTr loggerEntry (toLogStr s)
  maybe (pure ()) (\x -> pushLogStrLn (view loggerSet x) msg) maybeLoggerSet

trace :: forall a m . (ToLogStr (Doc a), MonadIO m) => Doc a -> m ()
trace = writeLog @TRACE

debug :: forall a m . (ToLogStr (Doc a), MonadIO m) => Doc a -> m ()
debug = writeLog @DEBUG

warn :: forall a m . (ToLogStr (Doc a), MonadIO m) => Doc a -> m ()
warn = writeLog @WARN

err :: forall a m . (ToLogStr (Doc a), MonadIO m) => Doc a -> m ()
err = writeLog @ERROR

notice :: forall a m . (ToLogStr (Doc a), MonadIO m) => Doc a -> m ()
notice = writeLog @NOTICE

info :: forall a m . (ToLogStr (Doc a), MonadIO m) => Doc a -> m ()
info = writeLog @INFO

-- instance {-# OVERLAPPABLE #-} Pretty a => ToLogStr a where
--   toLogStr p = toLogStr (show (pretty p))


instance {-# OVERLAPPABLE #-} ToLogStr (Doc ann) where
  toLogStr = toLogStr . show


logPrefix :: LogStr -> LoggerEntry-> LoggerEntry
logPrefix s = set loggerTr (s <>)

