module HBS2.Actors
  ( Pipeline
  , newPipeline
  , runPipeline
  , stopPipeline
  , addJob
  ) where

import HBS2.Prelude
import HBS2.Clock

import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue qualified as TBMQ
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Monad
import Data.Function
import Data.Functor
import Data.Kind
import Control.Concurrent

data Pipeline m a =
  Pipeline
  { stopAdding :: TVar Bool
  , toQueue    :: TBMQueue ( m a )
  }

newPipeline :: forall a (m1 :: Type -> Type)  (m :: Type -> Type) . (MonadIO m, MonadIO m1)  => Int -> m (Pipeline m1 a)
newPipeline size = do
  tv <- liftIO $ TVar.newTVarIO False
  liftIO $ TBMQ.newTBMQueueIO size <&> Pipeline tv


runPipeline :: MonadIO m => Pipeline m a -> m ()
runPipeline pip = fix \next -> do
  mbJob <- liftIO $ atomically $ TBMQ.readTBMQueue (toQueue pip)

  case mbJob of
    Nothing  -> pure ()
    Just job -> void (liftIO yield >> job) >> next

stopPipeline :: MonadIO m => Pipeline m a -> m ()
stopPipeline pip = liftIO $ do
  atomically $ TVar.writeTVar ( stopAdding pip ) True
  fix \next -> do
    mt <- atomically $ TBMQ.isEmptyTBMQueue ( toQueue pip )
    if mt then
      atomically $ TBMQ.closeTBMQueue ( toQueue pip )
    else do
      pause ( 0.01 :: Timeout 'Seconds) >> next

addJob :: forall a m m1 . (MonadIO m, MonadIO m1) => Pipeline m a -> m a -> m1 ()
addJob pip act = liftIO $ do
  doWrite <- atomically $ TVar.readTVar ( stopAdding pip )
  unless doWrite $ do
    atomically $ TBMQ.writeTBMQueue (toQueue pip) act


