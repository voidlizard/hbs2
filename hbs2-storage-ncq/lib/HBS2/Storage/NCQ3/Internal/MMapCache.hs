module HBS2.Storage.NCQ3.Internal.MMapCache where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files

import Data.HashPSQ as HPSQ
import System.IO.MMap

cacheLookupOrInsert :: forall m val.
                       MonadUnliftIO m
                    => Int                                  -- ^ max size
                    -> (FileKey -> m val)                   -- ^ loader
                    -> TVar (HashPSQ FileKey CachePrio val) -- ^ the cache
                    -> FileKey
                    -> m val
cacheLookupOrInsert maxSize load cacheTVar fk = do
  now <- getTimeCoarse

  atomically (HPSQ.lookup fk <$> readTVar cacheTVar) >>= \case
    Just (_, val) -> do
      atomically $ modifyTVar' cacheTVar (HPSQ.insert fk now val)
      pure val

    Nothing -> do
      val <- load fk
      atomically do
        old <- readTVar cacheTVar
        let new =
              if HPSQ.size old >= maxSize
                then HPSQ.insert fk now val (HPSQ.deleteMin old)
                else HPSQ.insert fk now val old
        writeTVar cacheTVar new
      pure val

ncqGetCachedData :: MonadUnliftIO m => NCQStorage3 -> FileKey -> m CachedData
ncqGetCachedData ncq@NCQStorage3{..} =
  cacheLookupOrInsert ncqMaxCachedData load ncqMMapCachedData
  where
    load fk = do
      let path = ncqGetFileName ncq (toFileName (DataFile fk))
      bs <- liftIO (mmapFileByteString path Nothing)
      pure (CachedData bs)

ncqGetCachedIndex :: MonadUnliftIO m => NCQStorage3 -> FileKey -> m CachedIndex
ncqGetCachedIndex ncq@NCQStorage3{..} =
  cacheLookupOrInsert ncqMaxCachedIndex load ncqMMapCachedIdx
  where
    load fk = do
      let path = ncqGetFileName ncq (toFileName (IndexFile fk))
      nwayHashMMapReadOnly path >>= \case
        Nothing -> throwIO $ NCQStorageCantMapFile path
        Just (bs, nway) -> pure (CachedIndex bs nway)

ncqDelCachedIndex :: forall m . MonadUnliftIO m
                  => NCQStorage3
                  -> FileKey
                  -> m ()

ncqDelCachedIndex NCQStorage3{..} fk =
  atomically (modifyTVar ncqMMapCachedIdx$ HPSQ.delete fk)


ncqDelCachedData :: forall m . MonadUnliftIO m
                  => NCQStorage3
                  -> FileKey
                  -> m ()

ncqDelCachedData NCQStorage3{..} fk =
  atomically (modifyTVar ncqMMapCachedData $ HPSQ.delete fk)

