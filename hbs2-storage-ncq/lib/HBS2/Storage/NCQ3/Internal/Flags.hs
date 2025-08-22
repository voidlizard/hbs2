module HBS2.Storage.NCQ3.Internal.Flags where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types

import Control.Concurrent.STM qualified as STM

ncqSetFlagSTM :: TVar Bool -> STM ()
ncqSetFlagSTM t = writeTVar t True

ncqSetFlag :: MonadIO m => TVar Bool -> m ()
ncqSetFlag t = atomically $ writeTVar t True

ncqClearFlagSTM :: TVar Bool -> STM ()
ncqClearFlagSTM t = writeTVar t False

ncqClearFlag :: MonadIO m => TVar Bool -> m ()
ncqClearFlag t = liftIO (atomically $ ncqClearFlagSTM t)

ncqWaitFlagSTM :: TVar Bool -> STM Bool
ncqWaitFlagSTM t = do
  val <- readTVar t
  unless val STM.retry
  writeTVar t False
  pure val

ncqGetFlagSTM :: TVar Bool -> STM Bool
ncqGetFlagSTM = readTVar

ncqGetFlag :: MonadIO m => TVar Bool -> m Bool
ncqGetFlag = liftIO . readTVarIO

