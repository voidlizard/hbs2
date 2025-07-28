{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ3.Internal where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Run
import HBS2.Storage.NCQ3.Internal.Memtable
import HBS2.Storage.NCQ3.Internal.Files

import Control.Monad.Trans.Cont
import Network.ByteOrder qualified as N
import Data.HashPSQ qualified as HPSQ
import Data.Vector qualified as V
import Data.HashMap.Strict qualified as HM
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
  ncqStateFiles     <- newTVarIO mempty
  ncqStateIndex     <- newTVarIO mempty
  ncqStateFileSeq   <- newTVarIO 0
  ncqStateVersion   <- newTVarIO 0
  ncqStateUsage     <- newTVarIO mempty
  ncqStateFacts     <- newTVarIO mempty
  ncqWrites         <- newTVarIO 0
  ncqWriteEMA       <- newTVarIO 0.0
  ncqWriteOps       <- V.fromList <$> replicateM wopNum newTQueueIO
  ncqReadReq        <- newTQueueIO
  ncqAlive          <- newTVarIO False
  ncqStopReq        <- newTVarIO False
  ncqSyncReq        <- newTVarIO False
  ncqOnRunWriteIdle <- newTVarIO none
  ncqSyncNo         <- newTVarIO 0

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
ncqPutBS ncq@NCQStorage3{..} mtp mhref bs' = ncqOperation ncq (pure $ fromMaybe (HashRef (hashObject @HbSync bs')) mhref) do
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
    nw <- readTVar ncqWrites <&> (`mod` V.length ncqWriteOps)
    modifyTVar ncqWrites succ
    writeTQueue (ncqWriteOps ! nw) work

  atomically $ takeTMVar waiter

ncqLocate :: MonadUnliftIO m => NCQStorage3 -> HashRef -> m (Maybe Location)
ncqLocate me@NCQStorage3{..} href = ncqOperation me (pure Nothing) do
  answ <- newEmptyTMVarIO

  atomically do
    modifyTVar ncqWrites succ
    writeTQueue ncqReadReq (href, answ)

  atomically $ takeTMVar answ

