{-# Language MultiWayIf #-}
module HBS2.Storage.NCQ3.Internal.Memtable where

import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Prelude

import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Vector qualified as V
import Control.Concurrent.STM qualified as STM

ncqShardIdx :: NCQStorage -> HashRef -> Int
ncqShardIdx NCQStorage{..} h =
  fromIntegral (BS.head (coerce h)) `mod` V.length ncqMemTable
{-# INLINE ncqShardIdx #-}

ncqGetShard :: NCQStorage -> HashRef -> Shard
ncqGetShard ncq@NCQStorage{..} h = ncqMemTable ! ncqShardIdx ncq h
{-# INLINE ncqGetShard #-}


ncqLookupEntrySTM :: NCQStorage -> HashRef -> STM (Maybe NCQEntry)
ncqLookupEntrySTM ncq h = readTVar (ncqGetShard ncq h) <&> HM.lookup h

ncqAlterEntrySTM :: NCQStorage
                 -> HashRef
                 -> (Maybe NCQEntry -> Maybe NCQEntry)
                 -> STM ()
ncqAlterEntrySTM ncq h alterFn = do
  let shard = ncqGetShard ncq h
  modifyTVar shard (HM.alter alterFn h)

ncqStorageSync :: forall m . MonadUnliftIO m => NCQStorage -> m ()
ncqStorageSync NCQStorage{..} = atomically $ writeTVar ncqSyncReq True

ncqOperation :: MonadIO m => NCQStorage -> m a -> m a -> m a
ncqOperation NCQStorage{..} m0 m = do
  what <- atomically do
            alive <- readTVar ncqAlive
            stop  <- readTVar ncqStopReq

            if | not alive && not stop -> STM.retry
               | not alive && stop     -> pure False
               | otherwise             -> pure True

  if what then m else m0



