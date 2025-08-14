{-# Language RecordWildCards #-}
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
  let ncqRoot           = fp
  let ncqGen            = 0
  let ncqFsync          = 16 * megabytes
  let ncqWriteQLen      = 1024 * 4
  let ncqMinLog         = 2  * gigabytes
  let ncqMaxLog         = 32 * gigabytes
  let ncqWriteBlock     = max 128 $ ncqWriteQLen `div` 2
  let ncqMaxCachedIndex = 64
  let ncqMaxCachedData  = 64
  let ncqIdleThrsh      = 50.0
  let ncqPostponeMerge  = 300.0
  let ncqPostponeSweep  = 2 * ncqPostponeMerge
  let ncqSalt           = "EstEFasxrCFqsGDxcY4haFcha9e4ZHRzsPbGUmDfdxLk"

  cap <- getNumCapabilities

  let shardNum = fromIntegral cap
  let wopNum   = 2

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
  ncqOnRunWriteIdle <- newTVarIO none
  ncqSyncNo         <- newTVarIO 0
  ncqState          <- newTVarIO mempty
  ncqStateKey       <- newTVarIO (FileKey maxBound)
  ncqStateUse       <- newTVarIO mempty
  ncqServiceSem     <- atomically $ newTSem 1
  ncqFileLock       <- newTVarIO Nothing

  let ncq = NCQStorage{..} & upd

  mkdir (ncqGetWorkDir ncq)

  liftIO (FL.tryLockFile (ncqGetFileName ncq ".lock") Exclusive)
    >>= orThrow NCQStorageCurrentAlreadyOpen
    >>= atomically . writeTVar ncqFileLock . Just

  liftIO (ncqTryLoadState ncq)

  pure ncq

ncqWithStorage :: MonadUnliftIO m => FilePath -> (NCQStorage -> m a) -> m a
ncqWithStorage fp action = flip runContT pure do
  sto <- lift (ncqStorageOpen fp id)
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
    Nothing -> Just <$> work sto (Just B) (Just ohash) bs
    _       -> pure (Just ohash)
  where
    bs =  LBS.toStrict lbs
    ohash = HashRef $ hashObject @HbSync bs

    work | wait = ncqPutBS
         | otherwise = ncqTossBS

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
        zero <- newTVarIO Nothing

        atomically do
          upd <- stateTVar shard $ flip HM.alterF h \case
                   Nothing -> (True, Just (NCQEntry bs zero))
                   Just e | ncqEntryData e /= bs  -> (True, Just (NCQEntry bs zero))
                          | otherwise -> (False, Just e)

          when upd  do
            modifyTVar ncqWriteQ (|> h)

          putTMVar waiter h

  atomically do
    modifyTVar ncqWrites succ
    nw <- readTVar ncqWrites <&> (`mod` V.length ncqWriteOps)
    writeTQueue (ncqWriteOps ! nw) work

  if not wait then pure h else atomically (takeTMVar waiter)

  where hash0 = HashRef (hashObject @HbSync bs')

ncqTryLoadState :: forall m. MonadUnliftIO m
                => NCQStorage
                -> m ()

ncqTryLoadState me@NCQStorage{..} = do

  stateFiles <- ncqListFilesBy me ( List.isPrefixOf "s-" )

  r <- flip fix  ([], ncqState0, stateFiles) $ \next -> \case
            (r, s, []) -> pure (r,s,[])
            (l, s0, (_,s):ss) -> do

              readStateMay me s >>= \case
                Nothing -> next (s : l, s0, ss)
                Just ns  -> do
                  ok <- checkState ns
                  if ok then
                    pure (l <> fmap snd ss, ns, ss)
                  else
                    next (s : l, s0, ss)

  let (bad, new@NCQState{..}, rest) = r

  atomically $ modifyTVar ncqState (<> new)

  for_ [ (d,s) | P (PData d s) <- Set.toList ncqStateFacts ] $ \(dataFile,s) -> do

    let path = ncqGetFileName me dataFile
    realSize <- fileSize path

    let sizewtf = realSize /= fromIntegral s

    flip fix 0 $ \again i -> do

      good <- try @_ @NCQFsckException (ncqFileFastCheck path)

      let corrupted = isLeft good

      if not corrupted then do
        debug $ yellow "indexing" <+> pretty dataFile
        ncqIndexFile me dataFile
      else do

        o <- ncqFileTryRecover path
        warn $ "ncqFileTryRecover" <+> pretty path <+> pretty o <+> parens (pretty realSize)

        let best = if i < 1 then max s o else s

        warn $ red "trim" <+> pretty s <+> pretty best  <+> red (pretty (fromIntegral best - realSize)) <+> pretty (takeFileName path)

        liftIO $ PFS.setFileSize path (fromIntegral best)

        if i <= 1 then again (succ i) else pure Nothing


  for_ (bad <> fmap snd rest) $ \f -> do
    let old = ncqGetFileName me (StateFile f)
    rm old

  where

    -- TODO: created-but-not-indexed-file?

    checkState NCQState{..} = flip runContT pure $ callCC \exit -> do

      for_ ncqStateFiles $ \fk -> do

        let dataFile = ncqGetFileName me (DataFile fk)
        here <- doesFileExist dataFile

        unless here $ exit False

        lift  (try @_ @SomeException (ncqFileFastCheck dataFile)) >>= \case
          Left e -> err (viaShow e) >> exit False
          Right () -> none

      pure True





class IsTomb a where
  ncqIsTomb :: a -> Bool

instance IsTomb IndexEntry where
  ncqIsTomb (IndexEntry _ _ s) = s <= (ncqSLen + ncqKeyLen + ncqPrefixLen)

instance IsTomb Location where
  ncqIsTomb = \case
    InFossil _ _ s -> ncqIsTombEntrySize s
    InMemory bs ->  case ncqEntryUnwrap bs of
                        (_, Right (T, _)) -> True
                        _                 -> False

ncqGetEntryBS :: MonadUnliftIO m => NCQStorage -> Location -> m (Maybe ByteString)
ncqGetEntryBS me = \case
  InMemory bs -> pure $ Just bs
  InFossil fk off size -> do
    try @_ @SomeException (ncqGetCachedData me fk) >>= \case
      Left{} -> pure Nothing
      Right (CachedData mmap) -> do
        pure $ Just $ BS.take (fromIntegral size) $ BS.drop (fromIntegral off) mmap

ncqEntrySize :: forall a . Integral a => Location -> a
ncqEntrySize = \case
  InFossil _ _ size -> fromIntegral size
  InMemory bs       -> fromIntegral (BS.length bs)

ncqDelEntry :: MonadUnliftIO m
            => NCQStorage
            -> HashRef
            -> m ()
ncqDelEntry me href = do
  -- всегда пишем tomb и надеемся на лучшее
  -- merge/compact разберутся
  -- однако не пишем, если записи еще нет
  ncqLocate me href >>= \case
    Just loc | not (ncqIsTomb loc) -> do
      void $ ncqPutBS me (Just T) (Just href) ""
    _ -> none


