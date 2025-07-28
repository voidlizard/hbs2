module HBS2.Storage.NCQ3.Internal.MMapCache where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.State

import Data.HashPSQ as HPSQ


ncqGetCachedIndex :: forall m . MonadUnliftIO m
                  => NCQStorage3
                  -> FileKey
                  -> m CachedIndex
ncqGetCachedIndex ncq@NCQStorage3{..} fk = do
  now <- getTimeCoarse

  atomically (HPSQ.lookup fk <$> readTVar ncqMMapCachedIdx) >>= \case
    Just (_, idx) -> do
      atomically $ modifyTVar' ncqMMapCachedIdx (HPSQ.insert fk now idx)
      pure idx

    Nothing -> do
      let path = ncqGetFileName ncq (toFileName (IndexFile fk))
      nwayHashMMapReadOnly path >>= \case
        Nothing -> throwIO $ NCQStorageCantMapFile path
        Just (bs, nway) -> do
          let new = CachedIndex bs nway
          atomically do
            cache <- readTVar ncqMMapCachedIdx
            let cache' =
                  if HPSQ.size cache >= ncqMaxCachedIndex
                    then HPSQ.deleteMin cache
                    else cache
            writeTVar ncqMMapCachedIdx (HPSQ.insert fk now new cache')
          pure new


ncqDelCachedIndex :: forall m . MonadUnliftIO m
                  => NCQStorage3
                  -> FileKey
                  -> m ()

ncqDelCachedIndex NCQStorage3{..} fk =
  atomically (modifyTVar ncqMMapCachedIdx$ HPSQ.delete fk)


