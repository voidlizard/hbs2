{-# Language RecordWildCards #-}
{-# Language MultiWayIf #-}
module HBS2.Storage.NCQ3.Internal where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Run
import HBS2.Storage.NCQ3.Internal.Memtable
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.MMapCache

import Control.Monad.Trans.Cont
import Data.HashPSQ qualified as HPSQ
import Data.Vector qualified as V
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Either
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Posix.Files qualified as PFS
import Control.Concurrent.STM.TSem
import System.FileLock as FL

ncqStorageOpen :: MonadIO m => FilePath -> (NCQStorage -> NCQStorage) -> m NCQStorage
ncqStorageOpen fp upd = do
  let ncqRoot            = fp
  let ncqGen             = 0
  -- let ncqFsync          = 16 * megabytes
  let ncqFsync           = 16   * megabytes
  let ncqWriteQLen       = 1024 * 4
  let ncqMinLog          = 512  * megabytes
  let ncqMaxLog          = 32   * gigabytes
  let ncqWriteBlock      = max 256 $ ncqWriteQLen `div` 2
  let ncqMaxCachedIndex  = 64
  let ncqMaxCachedData   = 64
  let ncqIdleThrsh       = 50.0
  let ncqPostponeService = 20
  let ncqSweepTime       = 30.00
  let ncqMergeTimeA      = 20.00
  let ncqMergeTimeB      = 120.00
  let ncqCompactTimeA    = 10.00
  let ncqCompactTimeB    = 60.00
  let ncqSalt            = "EstEFasxrCFqsGDxcY4haFcha9e4ZHRzsPbGUmDfdxLk"

  cap <- getNumCapabilities

  let shardNum = fromIntegral cap
  let wopNum   = 2

  let !ncqReadThreads = wopNum * 4

  ncqWriteQ         <- newTVarIO mempty
  ncqMemTable       <- V.fromList <$> replicateM shardNum (newTVarIO mempty)
  ncqMMapCachedIdx  <- newTVarIO HPSQ.empty
  ncqMMapCachedData <- newTVarIO HPSQ.empty
  ncqWrites         <- newTVarIO 0
  ncqWriteEMA       <- newTVarIO 0.0
  ncqWriteOps       <- V.fromList <$> replicateM wopNum newTQueueIO
  ncqSyncOps        <- newTQueueIO
  ncqReadReq        <- newTQueueIO
  ncqAlive          <- newTVarIO False
  ncqStopReq        <- newTVarIO False
  ncqSyncReq        <- newTVarIO False
  ncqSweepReq       <- newTVarIO False
  ncqMergeReq       <- newTVarIO False
  ncqCompactReq     <- newTVarIO False
  ncqOnRunWriteIdle <- newTVarIO none
  ncqSyncNo         <- newTVarIO 0
  ncqState          <- newTVarIO mempty
  ncqStateKey       <- newTVarIO ncqNullStateKey
  ncqStateUse       <- newTVarIO mempty
  ncqServiceSem     <- atomically $ newTSem 1
  ncqRunSem         <- atomically $ newTSem 1
  ncqFileLock       <- newTVarIO Nothing
  ncqCurrentFossils <- newTVarIO mempty
  ncqReplQueue      <- newTVarIO mempty

  let ncq = NCQStorage{..} & upd

  mkdir (ncqGetWorkDir ncq)

  pure ncq

{- HLINT ignore "Eta reduce" -}

ncqWithStorage :: MonadUnliftIO m
               => FilePath
               -> (NCQStorage -> m a) -> m a
ncqWithStorage fp action = ncqWithStorage0 fp id action

ncqWithStorage0 :: MonadUnliftIO m
                => FilePath
                -> (NCQStorage -> NCQStorage)
                -> (NCQStorage -> m a) -> m a
ncqWithStorage0 fp tune action = flip runContT pure do
  sto <- lift (ncqStorageOpen fp tune)
  w <- ContT $ withAsync (ncqStorageRun sto)
  link w
  r <- lift (action sto)
  lift (ncqStorageStop sto)
  wait w
  pure r

ncqPutBlock :: MonadUnliftIO  m
            => NCQStorage
            -> LBS.ByteString
            -> m (Maybe HashRef)

-- FIXME: Nothing-on-exception
ncqPutBlock sto lbs = ncqPutBlock0 sto lbs True
{-# INLINE ncqPutBlock #-}

ncqTossBlock :: MonadUnliftIO  m
            => NCQStorage
            -> LBS.ByteString
            -> m (Maybe HashRef)

ncqTossBlock sto lbs = ncqPutBlock0 sto lbs False
{-# INLINE ncqTossBlock #-}

-- FIXME: ncqLocate-slows-write

-- FIXME: resources-exhausted
-- test-ncq debug off and test:root temp and test:dir:keep and test:ncq3:merkle:file ./f10g
-- test-ncq: openAnonymousTempFileFromDir:
--   resource exhausted (Too many open files)
ncqPutBlock0 :: MonadUnliftIO m
             => NCQStorage
             -> LBS.ByteString
             -> Bool
             -> m (Maybe HashRef)
ncqPutBlock0 sto lbs wait = do
  ncqLocate sto ohash >>= \case
    Nothing                -> Just <$> work
    Just l | ncqIsTomb l   -> Just <$> work
    _                      -> pure (Just ohash)
    -- _                      ->  Just <$> work
  where
    bs =  LBS.toStrict lbs
    ohash = HashRef $ hashObject @HbSync bs

    work | wait      = ncqPutBS sto (Just B) (Just ohash) bs
         | otherwise = ncqTossBS sto (Just B) (Just ohash) bs
    {-# INLINE work #-}

{-# INLINE ncqPutBlock0 #-}

ncqPutBS :: MonadUnliftIO m
         => NCQStorage
         -> Maybe NCQSectionType
         -> Maybe HashRef
         -> ByteString
         -> m HashRef
ncqPutBS = ncqPutBS0 True
{-# INLINE ncqPutBS #-}


ncqTossBS :: MonadUnliftIO m
          => NCQStorage
          -> Maybe NCQSectionType
          -> Maybe HashRef
          -> ByteString
          -> m HashRef
ncqTossBS = ncqPutBS0 False
{-# INLINE ncqTossBS #-}

-- FIXME: maybe-on-storage-closed
ncqPutBS0 :: MonadUnliftIO m
         => Bool
         -> NCQStorage
         -> Maybe NCQSectionType
         -> Maybe HashRef
         -> ByteString
         -> m HashRef
ncqPutBS0 wait ncq@NCQStorage{..} mtp mhref bs' = ncqOperation ncq (pure $ fromMaybe hash0 mhref) do
  waiter <- newEmptyTMVarIO

  let h = fromMaybe (HashRef (hashObject @HbSync bs')) mhref

  let work = do
        let bs = ncqMakeSectionBS mtp h bs'
        let shard = ncqGetShard ncq h

        atomically do
          upd <- readTVar shard <&> HM.lookup h >>= \case
            Nothing -> do
              here <- newTVar (EntryHere bs)
              modifyTVar shard (HM.insert h (NCQEntry here))
              pure True

            Just (NCQEntry e) -> readTVar e >>= \case
              EntryHere bs'' | bs == bs''-> pure False
                             | otherwise -> writeTVar e (EntryHere bs) >> pure True

              EntryThere{} -> writeTVar e (EntryHere bs) >> pure True

          when upd  do
            modifyTVar ncqWriteQ (|> h)

          putTMVar waiter h

  atomically do
    modifyTVar ncqWrites succ
    nw <- readTVar ncqWrites <&> (`mod` V.length ncqWriteOps)
    writeTQueue (ncqWriteOps ! nw) work

  if not wait then pure h else atomically (takeTMVar waiter)

  where hash0 = HashRef (hashObject @HbSync bs')



class IsTomb a where
  ncqIsTomb :: a -> Bool

instance IsTomb IndexEntry where
  ncqIsTomb (IndexEntry _ _ s) = s <= (ncqSLen + ncqKeyLen + ncqPrefixLen)

instance IsTomb Location where
  ncqIsTomb = \case
    InFossil (FileLocation _ _ s) -> ncqIsTombEntrySize s
    InMemory bs ->  case ncqEntryUnwrap bs of
                        (_, Right (T, _)) -> True
                        _                 -> False

instance IsTomb FileLocation where
  ncqIsTomb (FileLocation _ _ s) = ncqIsTombEntrySize s

ncqGetEntryBS :: MonadUnliftIO m => NCQStorage -> Location -> m (Maybe ByteString)
ncqGetEntryBS me = \case
  InMemory bs -> pure $ Just bs

  InFossil l@(FileLocation fk off size) -> flip fix (0 :: Int) \next i -> do
    ncqWithState me $ const do
      try @_ @SomeException (ncqGetCachedData me fk) >>= \case
        Left e -> err (viaShow e) >> pure Nothing
        Right (CachedData mmap) -> do

          if | BS.length mmap >= fromIntegral off + fromIntegral size -> do
                 pure $ Just $ BS.take (fromIntegral size) $ BS.drop (fromIntegral off) mmap

             | i < 1 -> do
                atomically (ncqDelCachedDataSTM me fk) >> next (succ i)

             | otherwise -> do
                 err $ red "can't remap fossil" <+> pretty l
                 pure Nothing

ncqEntrySize :: forall a . Integral a => Location -> a
ncqEntrySize = \case
  InFossil (FileLocation _ _ size) -> fromIntegral size
  InMemory bs       -> fromIntegral (BS.length bs)

ncqDelEntry :: MonadUnliftIO m
            => NCQStorage
            -> HashRef
            -> m ()
ncqDelEntry me href = do
  -- всегда пишем tomb и надеемся на лучшее
  -- merge/compact разберутся
  -- однако не пишем, если записи еще нет
  -- void $ ncqPutBS me (Just T) (Just href) ""
  ncqLocate me href >>= \case
    Just loc | not (ncqIsTomb loc) -> do
      void $ ncqPutBS me (Just T) (Just href) ""
    _ -> none


