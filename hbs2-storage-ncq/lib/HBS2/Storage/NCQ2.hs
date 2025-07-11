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
import Data.Bit.ThreadSafe qualified as BV
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

newtype NCQEntry = NCQEntry ByteString

type Shard = TVar (HashMap HashRef NCQEntry)

type NCQOffset = Word64
type NCQSize   = Word32

data Location =
       InFossil  ByteString NCQOffset NCQSize
     | InMemory  ByteString

data NCQStorage2 =
  NCQStorage2
  { ncqRoot           :: FilePath
  , ncqGen            :: Int
  , ncqSalt           :: HashRef
  , ncqFsync          :: Int
  , ncqWriteQLen      :: Int
  , ncqWriteBlock     :: Int
  , ncqMinLog         :: Int
  , ncqMaxCached      :: Int
  , ncqMemTable       :: Vector  Shard
  , ncqWriteSem       :: TSem
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqStorageStopReq :: TVar Bool
  , ncqStorageSyncReq :: TVar Bool
  , ncqSyncNo         :: TVar Int
  , ncqTrackedFiles   :: TVar (HashPSQ FileKey FilePrio (Maybe CachedEntry))
  , ncqCachedEntries  :: TVar Int
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
  let ncqMaxCached  = 128
  cap               <- getNumCapabilities <&> fromIntegral
  ncqWriteQ         <- newTVarIO mempty
  ncqWriteSem       <- atomically $ newTSem 16 -- (fromIntegral cap)
  ncqMemTable       <- V.fromList <$> replicateM cap (newTVarIO mempty)
  ncqStorageStopReq <- newTVarIO False
  ncqStorageSyncReq <- newTVarIO False
  ncqSyncNo         <- newTVarIO 0
  ncqTrackedFiles   <- newTVarIO HPSQ.empty
  ncqCachedEntries  <- newTVarIO 0

  let ncqSalt       = "EstEFasxrCFqsGDxcY4haFcha9e4ZHRzsPbGUmDfdxLk"

  let ncq = NCQStorage2{..} & upd

  mkdir (ncqGetWorkDir ncq)

  ncqRepair ncq
  ncqLoadIndexes ncq

  pure ncq


ncqWithStorage :: MonadUnliftIO m => FilePath -> ( NCQStorage2 -> m a ) -> m a
ncqWithStorage fp action = flip runContT pure do
  sto <- lift (ncqStorageOpen2 fp id)
  w <- ContT $ withAsync (ncqStorageRun2 sto)
  link w
  r <- lift (action sto)
  lift (ncqStorageStop2 sto)
  wait w
  pure r

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

ncqLookupEntrySTM :: NCQStorage2 -> HashRef -> STM (Maybe NCQEntry)
ncqLookupEntrySTM ncq h = readTVar (ncqGetShard ncq h) <&> HM.lookup h

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

    ncqAlterEntrySTM ncq h $ \case
      Just e  -> Just e
      Nothing -> Just (NCQEntry bs)
    modifyTVar' ncqWriteQ (|> h)
    signalTSem ncqWriteSem

  pure h

ncqLookupEntry :: MonadUnliftIO m => NCQStorage2 -> HashRef -> m (Maybe NCQEntry)
ncqLookupEntry sto hash = atomically (ncqLookupEntrySTM sto hash)

ncqGetEntryBS :: NCQStorage2 -> Location -> ByteString
ncqGetEntryBS _ = \case
  InMemory bs -> bs
  InFossil mmap off size -> do
    BS.take (fromIntegral size) $ BS.drop (fromIntegral off) mmap

ncqEntrySize :: forall a . Integral a => Location -> a
ncqEntrySize = \case
  InFossil _ _ size -> fromIntegral size
  InMemory bs       -> fromIntegral (BS.length bs)

ncqLocate2 :: MonadUnliftIO m => NCQStorage2 -> HashRef -> m (Maybe Location)
ncqLocate2 ncq@NCQStorage2{..} href = flip runContT pure $ callCC \exit -> do
  now <- getTimeCoarse

  lift (ncqLookupEntry ncq href) >>= maybe none (exit . Just . InMemory . coerce)

  tracked <- readTVarIO ncqTrackedFiles <&> HPSQ.toList

  for_ tracked $ \(fk, prio, mCached) -> case mCached of
      Just CachedEntry{..} -> do
        lookupEntry href (cachedMmapedIdx, cachedNway) >>= \case
          Nothing -> none
          Just (offset,size) -> do
            atomically $ writeTVar cachedTs now
            exit (Just $ InFossil cachedMmapedData offset size)

      Nothing ->  do
        let indexFile = ncqGetFileName ncq (toFileName (IndexFile fk))
        let dataFile  = ncqGetFileName ncq (toFileName (DataFile fk))

        (idxBs, idxNway) <- liftIO (nwayHashMMapReadOnly indexFile)
                             >>= orThrow (NCQStorageCantMapFile indexFile)

        datBs <- liftIO $ mmapFileByteString dataFile Nothing

        ce <- CachedEntry idxBs datBs idxNway <$> newTVarIO now

        lookupEntry href (idxBs, idxNway) >>= \case
          Nothing -> none
          Just (offset, size) -> do

            atomically do
              modifyTVar ncqTrackedFiles (HPSQ.insert fk prio (Just ce))
              modifyTVar ncqCachedEntries (+1)
              evictIfNeededSTM ncq (Just 1)

            exit $ Just (InFossil datBs offset size)

  pure mzero

  where
    lookupEntry (hx :: HashRef) (mmaped, nway) = runMaybeT do
      entryBs <- liftIO (nwayHashLookup nway mmaped (coerce hx)) >>= toMPlus
      pure
        ( fromIntegral $ N.word64 (BS.take 8 entryBs)
        , fromIntegral $ N.word32 (BS.take 4 (BS.drop 8 entryBs)) )

ncqAlterEntrySTM :: NCQStorage2 -> HashRef -> (Maybe NCQEntry -> Maybe NCQEntry) -> STM ()
ncqAlterEntrySTM ncq h alterFn = do
  let shard = ncqGetShard ncq h
  modifyTVar shard (HM.alter alterFn h)


ncqStorageDel :: MonadUnliftIO m => NCQStorage2 -> HashRef -> m ()
ncqStorageDel ncq@NCQStorage2{..} h = flip runContT pure $ callCC \exit -> do
  -- 1. absent
  -- 1. in memtable only
  -- 2. in disk
  none

data RunSt =
    RunNew
  | RunWrite (FileKey, Fd, Int, Int)
  | RunSync  (FileKey, Fd, Int, Int, Bool)
  | RunFin

ncqStorageRun2 :: forall m . MonadUnliftIO m
               => NCQStorage2
               -> m ()
ncqStorageRun2 ncq@NCQStorage2{..} = flip runContT pure do

  jobQ <- newTQueueIO
  closeQ <- newTQueueIO

  closer <- ContT $ withAsync $ liftIO $ fix \loop -> do
    what <- atomically do
              stop <- readTVar ncqStorageStopReq
              tryReadTQueue closeQ >>= \case
                Just e  -> pure $ Just e
                Nothing | not stop -> STM.retry
                        | otherwise -> pure Nothing

    maybe1 what none $ \(fk, fh) ->  do
        closeFd fh
        -- notice $ yellow "indexing" <+> pretty fname
        idx <- ncqIndexFile ncq (DataFile fk)
        ncqAddTrackedFile ncq (DataFile fk)
        nwayHashMMapReadOnly idx >>= \case
          Nothing -> err $ "can't open index" <+> pretty idx
          Just (bs,nway) -> do
            nwayHashScanAll nway bs $ \_ k _ -> do
              unless (k == emptyKey) $ atomically do
                ncqAlterEntrySTM ncq (coerce k) (const Nothing)
        loop

  link closer

  jobz <- ContT $ withAsync $ forever (atomically (readTQueue jobQ) >>= join)
  link jobz

  ContT $ bracket none $ const $ liftIO do
    fhh <- atomically (STM.flushTQueue closeQ)
    for_ fhh ( closeFd . snd )

  flip fix RunNew $ \loop -> \case

    RunFin -> do
      debug "wait finalizing"
      atomically $ pollSTM closer >>= maybe STM.retry (const none)
      debug "exit storage"

    RunNew -> do
      stop <- readTVarIO ncqStorageStopReq
      mt   <- readTVarIO ncqWriteQ <&> Seq.null

      if stop && mt then do
        loop RunFin
      else do
        (fk,fhx) <- openNewDataFile
        loop $ RunWrite (fk,fhx,0,0)

    RunSync (fk, fh, w, total, continue) -> do

      stop <- readTVarIO ncqStorageStopReq
      sync <- readTVarIO ncqStorageSyncReq
      let needClose = total >= ncqMinLog || stop

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

         | not continue -> loop RunFin

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
                    Just (NCQEntry bs)  -> do
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
      ncqMakeSectionBS (Just M) h paylo
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


ncqIndexFile :: MonadUnliftIO m => NCQStorage2 -> DataFile FileKey -> m FilePath
ncqIndexFile n@NCQStorage2{}  fk = do

  let fp   = toFileName fk & ncqGetFileName n
  let dest = toFileName (IndexFile (coerce @_ @FileKey fk)) & ncqGetFileName n

  debug $ "INDEX" <+> pretty fp <+> pretty dest

  items <- S.toList_ do
    ncqStorageScanDataFile n fp $ \o w k _ -> do
      let rs = w - 32 & fromIntegral @_ @Word32 & N.bytestring32
      let os = fromIntegral @_ @Word64 o & N.bytestring64
      let record = os <> rs
      -- debug $ "write record" <+> pretty (BS.length record)
      S.yield (coerce k, record)

  let (dir,name) = splitFileName fp
  let idxTemp = (dropExtension name <> "-") `addExtension` ".cq$"

  result <- nwayWriteBatch (nwayAllocDef 1.10 32 8 12) dir idxTemp items

  mv result dest
  pure dest

ncqAddTrackedFile :: MonadIO m => NCQStorage2 -> DataFile FileKey -> m Bool
ncqAddTrackedFile ncq@NCQStorage2{..} fkey = flip runContT pure $ callCC \exit -> do
  let fname = ncqGetFileName ncq (toFileName fkey)
  let idxName = ncqGetFileName ncq (toFileName (IndexFile (coerce @_ @FileKey fkey)))

  idxHere <- doesFileExist idxName

  unless idxHere do
    err $ "Index does not exist"  <+> pretty (takeFileName idxName)
    exit False

  stat <- liftIO $ PFS.getFileStatus fname
  -- FIXME: maybe-creation-time-actually
  let ts = posixToTimeSpec $ PFS.modificationTimeHiRes stat
  let fk = fromString (takeFileName fname)
  atomically do
    modifyTVar' ncqTrackedFiles (HPSQ.insert fk (FilePrio (Down ts)) Nothing)
    pure True

evictIfNeededSTM :: NCQStorage2 -> Maybe Int -> STM ()
evictIfNeededSTM NCQStorage2{..} howMany = do
  cur <- readTVar ncqCachedEntries

  let need   = fromMaybe (cur `div` 2) howMany
      excess = max 0 (cur + need - ncqMaxCached)

  when (excess > 0) do
    files <- readTVar ncqTrackedFiles <&> HPSQ.toList

    oldest <- forM files \case
      (k, prio, Just ce) -> do
        ts <- readTVar (cachedTs ce)
        pure (Just (ts, k, prio))
      _ -> pure Nothing

    let victims =
          oldest
          & catMaybes
          & List.sortOn (\(ts,_,_) -> ts)
          & List.take excess

    for_ victims $ \(_,k,prio) -> do
      modifyTVar ncqTrackedFiles (HPSQ.insert k prio Nothing)
      modifyTVar ncqCachedEntries (subtract 1)


{- HLINT ignore "Functor law" -}

ncqListTrackedFiles :: MonadIO m => NCQStorage2 -> m [FilePath]
ncqListTrackedFiles ncq = do
  let wd = ncqGetWorkDir ncq
  dirFiles wd
     >>= mapM (pure . takeBaseName)
     <&> List.filter (List.isPrefixOf "fossil-")
     <&> HS.toList . HS.fromList


ncqLoadSomeIndexes :: MonadIO m => NCQStorage2 -> [FileKey] -> m ()
ncqLoadSomeIndexes ncq@NCQStorage2{..} keys = do
  now <- getTimeCoarse

  mapM_ (ncqAddTrackedFile ncq) (fmap DataFile keys)

  loaded <- catMaybes <$> forM keys \key -> runMaybeT do
    mEntry <- liftIO $ readTVarIO ncqTrackedFiles <&> HPSQ.lookup key
    guard (maybe True (\(_, m) -> isNothing m) mEntry)

    let idxFile = ncqGetFileName ncq (toFileName $ IndexFile key)
    let datFile = ncqGetFileName  ncq (toFileName $ DataFile key)

    (mmIdx, nway) <- MaybeT $ liftIO $ nwayHashMMapReadOnly idxFile
    mmData        <- liftIO $ mmapFileByteString datFile Nothing
    tnow <- newTVarIO now
    pure (key, CachedEntry mmIdx mmData nway tnow)

  atomically do
    evictIfNeededSTM ncq (Just (List.length loaded))

    for_ loaded \(k, ce) -> do
      files <- readTVar ncqTrackedFiles
      case HPSQ.lookup k files of
        Just (p, Nothing) -> do
          modifyTVar ncqTrackedFiles (HPSQ.insert k p (Just ce))
          modifyTVar ncqCachedEntries (+1)
        _ -> pure ()

ncqLoadIndexes :: MonadIO m => NCQStorage2 -> m ()
ncqLoadIndexes ncq@NCQStorage2{..} = do
  w <- readTVarIO ncqTrackedFiles
          <&> List.take (ncqMaxCached `div` 2) . HPSQ.keys
  ncqLoadSomeIndexes ncq w

ncqRepair :: MonadIO m => NCQStorage2 -> m ()
ncqRepair me@NCQStorage2{} = do
  fossils <- ncqListTrackedFiles me

  for_ fossils $ \fo -> liftIO $ flip fix 0 \next i -> do
    let dataFile = ncqGetFileName me $ toFileName (DataFile fo)
    try @_ @SomeException (ncqFileFastCheck dataFile) >>= \case
      Left e -> do
        err (viaShow e)
        -- TODO: try-fix-later
        mv dataFile (dropExtension dataFile `addExtension` ".broken")
        rm (ncqGetFileName me (toFileName (IndexFile fo)))

      Right{} | i <= 1 -> do
        let dataKey = DataFile (fromString fo)
        idx <- doesFileExist (toFileName (IndexFile dataFile))

        unless idx do
          debug $ "indexing" <+> pretty (toFileName dataKey)
          r <- ncqIndexFile me dataKey
          debug  $ "indexed" <+> pretty r
          next (succ i)

        void $ ncqAddTrackedFile me dataKey

      Right{} -> do
        err $ "skip indexing" <+> pretty dataFile


ncqRefHash :: NCQStorage2 -> HashRef -> HashRef
ncqRefHash NCQStorage2 {..} h = HashRef (hashObject (coerce @_ @ByteString h <> coerce ncqSalt))

