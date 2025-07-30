{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ3.Internal where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Run
import HBS2.Storage.NCQ3.Internal.Memtable
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Index

import Control.Monad.Trans.Cont
import Network.ByteOrder qualified as N
import Data.HashPSQ qualified as HPSQ
import Data.Vector qualified as V
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Either
import Lens.Micro.Platform
import Data.ByteString qualified as BS
import Data.Sequence qualified as Seq
import System.FilePath.Posix
import System.Posix.Files qualified as Posix
import System.Posix.IO as PosixBase
import System.Posix.Types as Posix
import System.Posix.Unistd
import System.Posix.IO.ByteString as Posix
import System.Posix.Files ( getFileStatus
                          , modificationTimeHiRes
                          , setFileTimesHiRes
                          , getFdStatus
                          , FileStatus(..)
                          , setFileMode
                          )
import System.Posix.Files qualified as PFS
import System.IO.MMap as MMap
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TSem

ncqStorageOpen3 :: MonadIO m => FilePath -> (NCQStorage3 -> NCQStorage3) -> m NCQStorage3
ncqStorageOpen3 fp upd = do
  let ncqRoot           = fp
  let ncqGen            = 0
  let ncqFsync          = 16 * megabytes
  let ncqWriteQLen      = 1024 * 4
  let ncqMinLog         = 512 * megabytes
  let ncqMaxLog         = 2 * ncqMinLog
  let ncqWriteBlock     = max 128 $ ncqWriteQLen `div` 2
  let ncqMaxCachedIndex = 16
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

  let ncq = NCQStorage3{..} & upd

  mkdir (ncqGetWorkDir ncq)

  liftIO (ncqTryLoadState ncq)

  pure ncq

ncqWithStorage3 :: MonadUnliftIO m => FilePath -> (NCQStorage3 -> m a) -> m a
ncqWithStorage3 fp action = flip runContT pure do
  sto <- lift (ncqStorageOpen3 fp id)
  w <- ContT $ withAsync (ncqStorageRun3 sto) -- TODO: implement run
  link w
  r <- lift (action sto)
  lift (ncqStorageStop3 sto)
  wait w
  pure r

-- FIXME: maybe-on-storage-closed
ncqPutBS :: MonadUnliftIO m
         => NCQStorage3
         -> Maybe NCQSectionType
         -> Maybe HashRef
         -> ByteString
         -> m HashRef
ncqPutBS ncq@NCQStorage3{..} mtp mhref bs' = ncqOperation ncq (pure $ fromMaybe hash0 mhref) do
  waiter <- newEmptyTMVarIO

  let work = do
        let h = fromMaybe (HashRef (hashObject @HbSync bs')) mhref
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

  atomically $ takeTMVar waiter

  where hash0 = HashRef (hashObject @HbSync bs')

ncqLocate :: MonadUnliftIO m => NCQStorage3 -> HashRef -> m (Maybe Location)
ncqLocate me@NCQStorage3{..} href = ncqOperation me (pure Nothing) do
  answ <- newEmptyTMVarIO

  atomically do
    modifyTVar ncqWrites succ
    writeTQueue ncqReadReq (href, answ)

  atomically $ takeTMVar answ

ncqTryLoadState :: forall m. MonadUnliftIO m
                => NCQStorage3
                -> m ()

ncqTryLoadState me@NCQStorage3{..} = do

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
    let color = if sizewtf then red else id

    good <- try @_ @NCQFsckException (ncqFileFastCheck path)

    let corrupted = isLeft good

    when corrupted $ liftIO do
      warn $ red "trim" <+> pretty s <+> pretty (takeFileName path)
      PFS.setFileSize path (fromIntegral s)

    debug $ yellow "indexing" <+> pretty dataFile <+> pretty s <+> color (pretty realSize)

    ncqIndexFile me dataFile

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


