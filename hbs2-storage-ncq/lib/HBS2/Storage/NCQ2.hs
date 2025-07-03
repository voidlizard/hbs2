{-# Language MultiWayIf #-}
{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ2
  ( module HBS2.Storage.NCQ2
  , module HBS2.Storage.NCQ.Types
  )
  where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Storage
import HBS2.Misc.PrettyStuff
import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.NCQ
import HBS2.Data.Log.Structured.SD

import HBS2.Storage.NCQ.Types

import Data.Config.Suckless.System
import Data.Config.Suckless.Script hiding (void)

import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Lazy as ZstdL
import Codec.Compression.Zstd.Streaming qualified as ZstdS
import Codec.Compression.Zstd.Streaming (Result(..))

import Control.Applicative
import Data.ByteString.Builder
import Network.ByteOrder qualified as N
import Data.HashMap.Strict (HashMap)
import Control.Monad.Except
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Ord (Down(..),comparing)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TSem
import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.IntSet qualified as IntSet
import Data.IntSet (IntSet)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..), (|>))
import Data.List qualified as List
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit)
import Data.Fixed
import Data.Coerce
import Data.Word
import Data.Either
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Int
import Data.Vector qualified as V
import Data.Vector (Vector, (!))
import Lens.Micro.Platform
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import System.Directory (makeAbsolute)
import System.FilePath.Posix
import System.Posix.Fcntl
import System.Posix.Files qualified as Posix
import System.Posix.IO as PosixBase
import System.Posix.Types as Posix
import System.Posix.IO.ByteString as Posix
import System.Posix.Unistd
import System.Posix.Files ( getFileStatus
                          , modificationTimeHiRes
                          , setFileTimesHiRes
                          , getFdStatus
                          , FileStatus(..)
                          , setFileMode
                          )
import System.Posix.Files qualified as PFS
import System.IO.Error (catchIOError)
import System.IO.MMap as MMap
import System.IO.Temp (emptyTempFile)
import System.Mem
-- import Foreign.Ptr
-- import Foreign di
import qualified Data.ByteString.Internal as BSI
import Streaming.Prelude qualified as S

import UnliftIO
import UnliftIO.Concurrent(getNumCapabilities)
import UnliftIO.IO.File

import System.FileLock as FL


type FOff = Word64

data NCQEntry =
    NCQEntryNew      Int ByteString
  -- | NCQEntryWritten  Int FileKey FOff (Maybe ByteString)

type Shard = TVar (HashMap HashRef (TVar NCQEntry))

data NCQStorage2 =
  NCQStorage2
  { ncqRoot           :: FilePath
  , ncqGen            :: Int
  , ncqFsync          :: Int
  , ncqWriteQLen      :: Int
  , ncqWriteBlock     :: Int
  , ncqMinLog         :: Int
  , ncqMemTable       :: Vector  Shard
  , ncqWriteSem       :: TSem
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqStorageStopReq :: TVar Bool
  , ncqStorageSyncReq :: TVar Bool
  , ncqSyncNo         :: TVar Int
  } deriving (Generic)


megabytes :: forall a . Integral a => a
megabytes = 1024 ^ 2

ncqStorageOpen2 :: MonadIO m => FilePath -> (NCQStorage2 -> NCQStorage2)-> m NCQStorage2
ncqStorageOpen2 fp upd = do
  let ncqRoot       = fp
  let ncqGen        = 0
  let ncqFsync      = 16   * megabytes
  let ncqWriteQLen  = 1024 * 16
  let ncqMinLog     = 256  * megabytes
  let ncqWriteBlock = 1024
  cap               <- getNumCapabilities <&> fromIntegral
  ncqWriteQ         <- newTVarIO mempty
  ncqWriteSem       <- atomically $ newTSem 16 -- (fromIntegral cap)
  ncqMemTable       <- V.fromList <$> replicateM cap (newTVarIO mempty)
  ncqStorageStopReq <- newTVarIO False
  ncqStorageSyncReq <- newTVarIO False
  ncqSyncNo         <- newTVarIO 0
  let ncq = NCQStorage2{..} & upd

  mkdir (ncqGetWorkDir ncq)

  pure ncq

ncqGetFileName :: NCQStorage2 -> FilePath -> FilePath
ncqGetFileName ncq fp = ncqGetWorkDir ncq </> takeFileName fp

ncqGetWorkDir :: NCQStorage2 -> FilePath
ncqGetWorkDir NCQStorage2{..} = ncqRoot </> show ncqGen

ncqGetLockFileName :: NCQStorage2 -> FilePath
ncqGetLockFileName ncq = ncqGetFileName ncq ".lock"

ncqStorageStop2 :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqStorageStop2 NCQStorage2{..} = do
  atomically $ writeTVar ncqStorageStopReq True

ncqStorageSync2 :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqStorageSync2 NCQStorage2{..} = do
  atomically $ writeTVar ncqStorageSyncReq True

ncqShardIdx :: NCQStorage2 -> HashRef -> Int
ncqShardIdx NCQStorage2{..} h =
  fromIntegral (BS.head (coerce h)) `mod` V.length ncqMemTable
{-# INLINE ncqShardIdx #-}

ncqGetShard :: NCQStorage2 -> HashRef -> Shard
ncqGetShard ncq@NCQStorage2{..} h = ncqMemTable ! ncqShardIdx ncq h
{-# INLINE ncqGetShard #-}

ncqLookupEntrySTM :: NCQStorage2 -> HashRef -> STM (Maybe (NCQEntry, TVar NCQEntry))
ncqLookupEntrySTM ncq h = do
  readTVar (ncqGetShard ncq h)
    <&> HM.lookup h
    >>= \case
      Nothing -> pure Nothing
      Just tv -> do
        v <- readTVar tv
        pure $ Just (v, tv)

ncqPutBS :: MonadUnliftIO m
         => NCQStorage2
         -> Maybe NCQSectionType
         -> Maybe HashRef
         -> ByteString
         -> m HashRef
ncqPutBS ncq@NCQStorage2{..} mtp mhref bs' = do
  let h = fromMaybe (HashRef (hashObject @HbSync bs')) mhref
  let bs = ncqMakeSectionBS mtp h bs'
  atomically do
    waitTSem ncqWriteSem
    stop <- readTVar ncqStorageStopReq
    filled <- readTVar ncqWriteQ <&> Seq.length

    when (not stop && filled > ncqWriteQLen) STM.retry

    n <- readTVar ncqSyncNo
    ncqAlterEntrySTM ncq h $ \case
      Just e  -> Just e
      Nothing -> Just (NCQEntryNew n bs)
    modifyTVar' ncqWriteQ (|> h)
    signalTSem ncqWriteSem

  pure h

ncqLookupEntry :: MonadUnliftIO m => NCQStorage2 -> HashRef -> m (Maybe NCQEntry)
ncqLookupEntry sto hash = atomically (ncqLookupEntrySTM sto hash) <&> fmap fst

ncqAlterEntrySTM :: NCQStorage2 -> HashRef -> (Maybe NCQEntry -> Maybe NCQEntry) -> STM ()
ncqAlterEntrySTM ncq h alterFn = do
  let shard = ncqGetShard ncq h
  readTVar shard <&> HM.lookup h >>= \case
    Just tve  -> do
      e  <- readTVar tve
      case alterFn (Just e) of
        Nothing -> modifyTVar' shard (HM.delete h)
        Just e' -> writeTVar tve e'

    Nothing -> case alterFn Nothing of
      Nothing -> modifyTVar' shard (HM.delete h)
      Just e  -> do
        tve <- newTVar e
        modifyTVar' shard (HM.insert h tve)


data RunSt =
    RunNew
  | RunWrite (FileKey, Fd, Int, Int)
  | RunSync  (FileKey, Fd, Int, Int, Bool)

ncqStorageRun2 :: forall m . MonadUnliftIO m
               => NCQStorage2
               -> m ()
ncqStorageRun2 ncq@NCQStorage2{..} = flip runContT pure $ callCC \exit -> do

  jobQ <- newTQueueIO
  closeQ <- newTQueueIO

  closer <- ContT $ withAsync $ liftIO $ forever do
    atomically (readTQueue closeQ) >>= \(fk, fh) -> do
      closeFd fh
      let fname = BS8.unpack (coerce fk)
      -- notice $ yellow "indexing" <+> pretty fname
      idx <- ncqIndexFile ncq fname
      nwayHashMMapReadOnly idx >>= \case
        Nothing -> err $ "can't open index" <+> pretty idx
        Just (bs,nway) -> do
          nwayHashScanAll nway bs $ \_ k _ -> do
            unless (k == emptyKey) do
              none
              atomically do
                ncqAlterEntrySTM ncq (coerce k) (const Nothing)

  link closer

  jobz <- ContT $ withAsync $ forever (atomically (readTQueue jobQ) >>= join)
  link jobz

  ContT $ bracket none $ const $ liftIO do
    fhh <- atomically (STM.flushTQueue closeQ)
    for_ fhh ( closeFd . snd )

  flip fix RunNew $ \loop -> \case

    RunNew -> do
      stop <- readTVarIO ncqStorageStopReq
      mt   <- readTVarIO ncqWriteQ <&> Seq.null

      when (stop && mt) do
        exit ()

      (fk,fhx) <- openNewDataFile
      loop $ RunWrite (fk,fhx,0,0)

    RunSync (fk, fh, w, total, continue) -> do

      sync <- readTVarIO ncqStorageSyncReq
      let needClose = total >= ncqMinLog

      rest <- if not (sync || needClose || w > ncqFsync) then
                  pure w
                else liftIO do
                  s <- Posix.fileSize <$> Posix.getFdStatus fh
                  void (appendSection fh (fileTailRecord s))
                  fileSynchronise fh
                  atomically do
                    writeTVar   ncqStorageSyncReq False
                    modifyTVar' ncqSyncNo succ
                  pure 0

      if | needClose && continue -> do
              atomically $ writeTQueue closeQ (fk, fh)
              loop RunNew

         | not continue -> exit ()

         | otherwise -> loop $ RunWrite (fk, fh, rest, total)

    RunWrite (fk, fh, w, total') -> do

      chunk <- atomically do
        stop  <- readTVar ncqStorageStopReq
        sy    <- readTVar ncqStorageSyncReq
        chunk <- stateTVar ncqWriteQ (Seq.splitAt ncqWriteBlock)

        if | Seq.null chunk && stop             -> pure $ Left ()
           | Seq.null chunk && not (stop || sy) -> STM.retry
           | otherwise                          -> pure $ Right chunk

      case chunk of
        Left{} -> loop $ RunSync (fk, fh, w, total', False) -- exit ()
        Right chu -> do
          ws <- for chu $ \h -> do
                  atomically (ncqLookupEntrySTM ncq h) >>= \case
                    Just (r@(NCQEntryNew ns bs),t)  -> do
                      lift (appendSection fh bs)

                    _ -> pure 0

          let written = sum ws
          loop $ RunSync (fk, fh, w + written, total' + written, True)

  where

    emptyKey = BS.replicate ncqKeyLen 0

    zeroSyncEntry = ncqMakeSectionBS (Just B) zeroHash zeroPayload
      where zeroPayload = N.bytestring64 0
            zeroHash    = HashRef (hashObject zeroPayload)
    {-# INLINE zeroSyncEntry #-}

    zeroSyncEntrySize = fromIntegral (BS.length zeroSyncEntry)
    {-# INLINE zeroSyncEntrySize #-}

    -- 1. It's B-record
    -- 2. It's last w64be == fileSize
    -- 3. It's hash == hash (bytestring64be fileSize)
    -- 4. recovery-strategy: start-to-end, end-to-start
    fileTailRecord w = do
      -- on open: last w64be == fileSize
      let paylo = N.bytestring64 (fromIntegral w + zeroSyncEntrySize)
      let h     = hashObject @HbSync paylo & coerce
      ncqMakeSectionBS (Just B) h paylo
    {-# INLINE fileTailRecord #-}

    appendSection :: forall m . MonadUnliftIO m
                => Fd
                -> ByteString
                -> m Int -- (FOff, Int)

    appendSection fh section = do
      -- off <- liftIO $ fdSeek fh SeekFromEnd 0
      -- pure (fromIntegral off, fromIntegral len)
      liftIO (Posix.fdWrite fh section) <&> fromIntegral

    {-# INLINE appendSection #-}

    openNewDataFile :: forall mx . MonadIO mx => mx (FileKey, Fd)
    openNewDataFile = do
      fname <-  liftIO $ emptyTempFile (ncqGetWorkDir ncq) "fossil-.data"
      touch fname
      let flags = defaultFileFlags { exclusive = False, creat = Just 0o666 }
      (fromString fname,) <$> liftIO (PosixBase.openFd fname  Posix.ReadWrite flags)

ncqFileFastCheck :: MonadUnliftIO m => FilePath -> m ()
ncqFileFastCheck fp = do
  mmaped <- liftIO $ mmapFileByteString fp Nothing
  let size = BS.length mmaped
  let s = BS.drop (size - 8) mmaped & N.word64

  unless ( BS.length mmaped == fromIntegral s ) do
    throwIO $ NCQFsckIssueExt (FsckInvalidFileSize (fromIntegral s))



ncqStorageScanDataFile :: MonadIO m
                       => NCQStorage2
                       -> FilePath
                       -> ( Integer -> Integer -> HashRef -> ByteString -> m () )
                       -> m ()
ncqStorageScanDataFile ncq fp' action = do
  let fp = ncqGetFileName ncq fp'
  mmaped <- liftIO (mmapFileByteString fp Nothing)

  flip runContT pure $ callCC \exit -> do
    flip fix (0,mmaped) $ \next (o,bs) -> do

     when (BS.length bs < ncqSLen) $ exit ()

     let w = BS.take ncqSLen bs & N.word32 & fromIntegral

     when (BS.length bs < ncqSLen + w) $ exit ()

     let kv = BS.drop ncqSLen bs

     let k = BS.take ncqKeyLen kv & coerce @_ @HashRef
     let v = BS.take (ncqFullDataLen (NCQFullRecordLen w)) $ BS.drop ncqKeyLen kv

     lift (action o (fromIntegral w) k v)

     next (ncqSLen + o + fromIntegral w, BS.drop (w+ncqSLen) bs)


ncqIndexFile :: MonadUnliftIO m => NCQStorage2 -> FilePath -> m FilePath
ncqIndexFile n@NCQStorage2{}  fp'' = do

  let fp' = addExtension (ncqGetFileName n fp'') ".data"

  let fp = ncqGetFileName n fp'
            & takeBaseName
            & (`addExtension` ".cq")
            & ncqGetFileName n

  trace $ "INDEX" <+> pretty fp' <+> pretty fp

  items <- S.toList_ do
    ncqStorageScanDataFile n fp' $ \o w k v -> do
      let rs = w - 32 & fromIntegral @_ @Word32 & N.bytestring32
      let os = fromIntegral @_ @Word64 o & N.bytestring64
      let record = os <> rs
      -- debug $ "write record" <+> pretty (BS.length record)
      S.yield (coerce k, record)

  let (dir,name) = splitFileName fp

  result <- nwayWriteBatch (nwayAllocDef 1.10 32 8 12) dir name items

  mv result fp

  pure fp


