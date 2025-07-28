module HBS2.Storage.NCQ3.Internal.Memtable where

import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Prelude

import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Vector qualified as V

ncqShardIdx :: NCQStorage3 -> HashRef -> Int
ncqShardIdx NCQStorage3{..} h =
  fromIntegral (BS.head (coerce h)) `mod` V.length ncqMemTable
{-# INLINE ncqShardIdx #-}

ncqGetShard :: NCQStorage3 -> HashRef -> Shard
ncqGetShard ncq@NCQStorage3{..} h = ncqMemTable ! ncqShardIdx ncq h
{-# INLINE ncqGetShard #-}


ncqLookupEntrySTM :: NCQStorage3 -> HashRef -> STM (Maybe NCQEntry)
ncqLookupEntrySTM ncq h = readTVar (ncqGetShard ncq h) <&> HM.lookup h

ncqAlterEntrySTM :: NCQStorage3
                 -> HashRef
                 -> (Maybe NCQEntry -> Maybe NCQEntry)
                 -> STM ()
ncqAlterEntrySTM ncq h alterFn = do
  let shard = ncqGetShard ncq h
  modifyTVar shard (HM.alter alterFn h)
