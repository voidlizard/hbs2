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
import Data.Sequence (Seq(..), (|>),(<|))
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

type StateVersion = Word64

data StateOP = D FileKey | F TimeSpec FileKey
               deriving (Eq,Ord,Show)

data NCQFlag =
  NCQMergeNow | NCQCompactNow
  deriving (Eq,Ord,Generic)

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
  , ncqIdleThrsh      :: Double
  , ncqMemTable       :: Vector Shard
  , ncqWriteSem       :: TSem
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqStorageTasks   :: TVar Int
  , ncqStorageStopReq :: TVar Bool
  , ncqStorageSyncReq :: TVar Bool
  , ncqSyncNo         :: TVar Int
  , ncqTrackedFiles   :: TVar (HashPSQ FileKey FilePrio (Maybe CachedEntry))
  , ncqStateVersion   :: TVar StateVersion
  , ncqStateUsage     :: TVar (IntMap (Int, HashSet FileKey))
  , ncqCachedEntries  :: TVar Int
  , ncqWrites         :: TVar Int
  , ncqWriteEMA       :: TVar Double  -- for writes-per-seconds
  , ncqJobQ           :: TQueue (IO ())
  }

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
  let ncqIdleThrsh  = 50.00

  cap                <- getNumCapabilities <&> fromIntegral
  ncqWriteQ          <- newTVarIO mempty
  ncqWriteSem        <- atomically $ newTSem 16 -- (fromIntegral cap)
  ncqMemTable        <- V.fromList <$> replicateM cap (newTVarIO mempty)
  ncqStorageStopReq  <- newTVarIO False
  ncqStorageSyncReq  <- newTVarIO False
  ncqSyncNo          <- newTVarIO 0
  ncqTrackedFiles    <- newTVarIO HPSQ.empty
  ncqStateVersion    <- newTVarIO 0
  ncqStateUsage      <- newTVarIO mempty
  ncqCachedEntries   <- newTVarIO 0
  ncqStorageTasks    <- newTVarIO 0
  ncqWrites          <- newTVarIO 0
  ncqWriteEMA        <- newTVarIO 0.00
  ncqJobQ            <- newTQueueIO

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

ncqGetNewFossilName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewFossilName ncq = do
 liftIO $ emptyTempFile (ncqGetWorkDir ncq) "fossil-.data"

ncqGetNewStateName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewStateName ncq = do
 liftIO $ emptyTempFile (ncqGetWorkDir ncq) "state-"

ncqGetNewCompactName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewCompactName n@NCQStorage2{} = do
  let (p,tpl)  = splitFileName (ncqGetFileName n "compact-.data")
  liftIO $ emptyTempFile p tpl

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

    modifyTVar' ncqWrites succ
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

  atomically do
    modifyTVar' ncqWrites succ

  -- FIXME: race
  --   merge can-delete-file-while-in-use

  tracked <- readTVarIO ncqTrackedFiles <&> HPSQ.toList

  for_ tracked $ \(fk, prio, mCached) -> do
    case mCached of
      Just CachedEntry{..} -> do
        lookupEntry href (cachedMmapedIdx, cachedNway) >>= \case
          Nothing -> none
          Just (offset,size) -> do
            atomically $ writeTVar cachedTs now
            exit (Just $ InFossil cachedMmapedData offset size)

      Nothing ->  useVersion do
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

    useVersion m = ContT (bracket succV predV) >> m
      where
        succV = atomically (ncqStateUseSTM ncq)
        predV = const $ atomically (ncqStateUseSTM ncq)

    lookupEntry (hx :: HashRef) (mmaped, nway) =
      liftIO (nwayHashLookup nway mmaped (coerce hx)) >>= \case
        Nothing -> pure Nothing
        Just entryBs -> do
          pure $ Just
            ( fromIntegral $ N.word64 (BS.take 8 entryBs)
            , fromIntegral $ N.word32 (BS.take 4 (BS.drop 8 entryBs)) )

    {-# INLINE lookupEntry #-}

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

  closeQ <- newTQueueIO

  closer <- spawnActivity $ liftIO $ fix \loop -> do
    what <- atomically do
              stop <- readTVar ncqStorageStopReq
              tryReadTQueue closeQ >>= \case
                Just e  -> pure $ Just e
                Nothing | not stop -> STM.retry
                        | otherwise -> pure Nothing

    maybe1 what none $ \(fk, fh) ->  do
        closeFd fh
        -- notice $ yellow "indexing" <+> pretty fname
        idx <- ncqRunTaskNoMatterWhat ncq (ncqIndexFile ncq (DataFile fk))
        ncqStateUpdate ncq [F 0 fk]
        -- ncqAddTrackedFile ncq (DataFile fk)
        nwayHashMMapReadOnly idx >>= \case
          Nothing -> err $ "can't open index" <+> pretty idx
          Just (bs,nway) -> do
            nwayHashScanAll nway bs $ \_ k _ -> do
              unless (k == emptyKey) $ atomically do
                ncqAlterEntrySTM ncq (coerce k) (const Nothing)
        loop

  spawnActivity $ forever (liftIO $ join $ atomically (readTQueue ncqJobQ))

  spawnActivity measureWPS

  spawnActivity $ fix \again -> (>> again) do
      ema <- readTVarIO ncqWriteEMA

      if ema > ncqIdleThrsh then do
        pause @'Seconds 2.5

      else do
        mq <- newEmptyTMVarIO

        spawnJob $ do
          -- TODO: back-to-merge
          -- merged <- ncqStorageMergeStep ncq
          let merged = True
          atomically $ putTMVar mq merged

        -- TODO: detect-dead-merge
        void $ race (pause @'Seconds 300) (atomically $ readTMVar mq) >>= \case
          Left{}     -> none
          Right True -> none
          Right False -> do
            debug "merge: all done, wait..."
            n0 <- readTVarIO ncqTrackedFiles <&> HPSQ.size
            atomically do
              n <- readTVar ncqTrackedFiles <&> HPSQ.size
              when (n == n0) STM.retry

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
                else do
                  appendTailSection fh >> liftIO (fileSynchronise fh)
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

    openNewDataFile :: forall mx . MonadIO mx => mx (FileKey, Fd)
    openNewDataFile = do
      fname <- ncqGetNewFossilName ncq
      touch fname
      let flags = defaultFileFlags { exclusive = False, creat = Just 0o666 }
      (fromString fname,) <$> liftIO (PosixBase.openFd fname  Posix.ReadWrite flags)

    spawnJob :: IO () -> m ()
    spawnJob m = atomically $ writeTQueue ncqJobQ m

    spawnActivity m = do
      a <- ContT $ withAsync m
      link a
      pure a

    measureWPS = void $ flip fix Nothing \loop -> \case
      Nothing      -> do
        w <- readTVarIO ncqWrites
        t <- getTimeCoarse
        pause @'Seconds step >> loop (Just (w,t))

      Just (w0,t0) -> do
        w1 <- readTVarIO ncqWrites
        t1 <- getTimeCoarse
        let dt = max 1e-9 (realToFrac @_ @Double (t1 - t0)) / 1e9
            dw = fromIntegral (w1 - w0)
        atomically $ modifyTVar' ncqWriteEMA \ema -> alpha * (dw/dt) + 0.9 * ema
        pause @'Seconds step >> loop (Just (w1,t1))

      where
        alpha = 0.1
        step  = 1.00


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

  atomically $ ncqAddTrackedFileSTM ncq fk ts
  pure True

ncqAddTrackedFileSTM :: NCQStorage2 -> FileKey -> TimeSpec -> STM ()
ncqAddTrackedFileSTM NCQStorage2{..} fk ts = do
    modifyTVar' ncqTrackedFiles (HPSQ.insert fk (FilePrio (Down ts)) Nothing)
{-# INLINE ncqAddTrackedFileSTM #-}

evictIfNeededSTM :: NCQStorage2 -> Maybe Int -> STM ()
evictIfNeededSTM NCQStorage2{..} howMany = do
  cur <- readTVar ncqCachedEntries

  let need   = fromMaybe cur howMany
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

  -- TODO: use-state
  warn $ red "TODO: use state for load"

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

  void (liftIO $ ncqStateUpdate me mempty)

ncqRefHash :: NCQStorage2 -> HashRef -> HashRef
ncqRefHash NCQStorage2 {..} h = HashRef (hashObject (coerce @_ @ByteString h <> coerce ncqSalt))

ncqRunTaskNoMatterWhat :: MonadUnliftIO m => NCQStorage2 -> m a -> m a
ncqRunTaskNoMatterWhat NCQStorage2{..} task = do
  atomically (modifyTVar ncqStorageTasks succ)
  task `finally` atomically (modifyTVar ncqStorageTasks pred)

ncqRunTask :: MonadUnliftIO m => NCQStorage2 -> a -> m a -> m a
ncqRunTask ncq@NCQStorage2{..} def task = readTVarIO ncqStorageStopReq >>= \case
  True -> pure def
  False -> ncqRunTaskNoMatterWhat ncq task

ncqWaitTasks :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqWaitTasks NCQStorage2{..} = atomically do
  tno <- readTVar ncqStorageTasks
  when (tno > 0) STM.retry

ncqStateUseSTM :: NCQStorage2 -> STM ()
ncqStateUseSTM NCQStorage2{..} = do
  k <- readTVar ncqStateVersion <&> fromIntegral
  modifyTVar' ncqStateUsage (IntMap.update (Just . over _1 succ) k)

ncqStateUnuseSTM :: NCQStorage2 -> STM ()
ncqStateUnuseSTM NCQStorage2{..} = do
  k <- readTVar ncqStateVersion <&> fromIntegral
  modifyTVar' ncqStateUsage (IntMap.update (Just . over _1 pred) k)

ncqStateUpdate :: MonadUnliftIO m => NCQStorage2 -> [StateOP] -> m Bool
ncqStateUpdate me@NCQStorage2{..} ops' = flip runContT pure $ callCC \exit -> do
  t1 <- fromIntegral <$> liftIO getTimeCoarse

  ops <- for ops' $ \case
    f@(F _ fk) -> do
      let idxFile = ncqGetFileName me (toFileName $ IndexFile fk)
      let datFile = ncqGetFileName me (toFileName $ DataFile fk)

      e1 <- doesFileExist idxFile
      e2 <- doesFileExist datFile

      unless (e1 && e2) do
        err $ "ncqStateUpdate invariant fail" <+> pretty idxFile <+> pretty datFile
        exit False

      ts <- liftIO (getFileStatus datFile) <&>
               posixToTimeSpec . PFS.modificationTimeHiRes

      pure (F ts fk)

    d    -> pure d

  changed <- atomically do
    t0  <- readTVar ncqStateVersion
    let k0  = fromIntegral t0

    c <- if List.null ops then do
            pure False
          else do
            writeTVar ncqStateVersion (max (succ t0) t1)
            for_ ops $ \case
              D fk -> modifyTVar' ncqTrackedFiles (HPSQ.delete fk)
              F t fk -> ncqAddTrackedFileSTM me (coerce fk) t
            pure True

    old <- readTVar ncqTrackedFiles <&> HS.fromList . HPSQ.keys

    let doAlter = \case
          Nothing    -> Just (0, old)
          Just (u,f) -> Just (u,f)

    modifyTVar' ncqStateUsage (IntMap.alter doAlter k0)

    pure c

  when changed (lift $ ncqDumpCurrentState me)

  pure changed

ncqDumpCurrentState :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqDumpCurrentState me@NCQStorage2{..} = do
  keys <- readTVarIO ncqTrackedFiles <&> List.sort . HPSQ.keys
  name <- ncqGetNewStateName me
  writeBinaryFileDurableAtomic name (BS8.unlines [coerce k | k <- keys])

-- FIXME: sometime-causes-no-such-file-or-directory
ncqStorageMergeStep :: MonadUnliftIO m => NCQStorage2 -> m Bool
ncqStorageMergeStep ncq@NCQStorage2{..}  = ncqRunTask ncq False $ flip runContT pure do

  liftIO do

    tracked <- readTVarIO ncqTrackedFiles <&> HPSQ.toList
    files <- for tracked $ \(f,p,_) -> do
      let fn = ncqGetFileName ncq (toFileName $ DataFile f)
      let idx = ncqGetFileName ncq (toFileName $ IndexFile f)
      sz <- liftIO (fileSize fn)
      idxHere <- doesFileExist idx
      pure (f, sz, p, idxHere)

    let bothIdx a b = view _4 a && view _4 b

    let found = flip fix (files, Nothing, Nothing) $ \next -> \case
          ([], _, r) -> r

          (a:b:rest, Nothing, _) | bothIdx a b -> do
            next (b:rest, Just (view _2 a + view _2 b), Just (a,b))

          (a:b:rest, Just s, _ ) | bothIdx a b && view _2 a + view _2 b < s -> do
            next (b:rest, Just (view _2 a + view _2 b), Just (a,b))

          (_:rest, s, r) -> do
            next (rest, s, r)

    case found of
      Just (a,b) -> mergeStep a b >> pure True
      _          -> pure False

  where

    ncqGetNewMergeName :: MonadIO m => NCQStorage2 -> m FilePath
    ncqGetNewMergeName n@NCQStorage2{} = do
      let (p,tpl) = splitFileName (ncqGetFileName n "merge-.data")
      liftIO $ emptyTempFile p tpl

    mergeStep (a,_,p1,_) (b,_,p2,_) = do
      debug $ "merge" <+> pretty a <+> pretty b

      let fDataNameA  = ncqGetFileName ncq $ toFileName (DataFile a)
      let fIndexNameA = ncqGetFileName ncq $ toFileName (IndexFile a)

      let fDataNameB  = ncqGetFileName ncq $ toFileName (DataFile b)
      let fIndexNameB = ncqGetFileName ncq $ toFileName (IndexFile b)

      debug $ "file A" <+> pretty (timeSpecFromFilePrio p1) <+> pretty fDataNameA <+> pretty fIndexNameA
      debug $ "file B" <+> pretty (timeSpecFromFilePrio p2) <+> pretty fDataNameB <+> pretty fIndexNameB

      doesFileExist fDataNameA   `orFail` ("not exist" <+> pretty fDataNameA)
      doesFileExist fDataNameB   `orFail` ("not exist" <+> pretty fDataNameB)
      doesFileExist fIndexNameA  `orFail` ("not exist" <+> pretty fIndexNameA)

      flip runContT pure $ callCC \exit -> do

        mfile <- ncqGetNewMergeName ncq

        ContT $ bracket none $ const do
          rm mfile

        liftIO $ withBinaryFileAtomic mfile WriteMode $ \fwh -> do

          debug $ "merge: okay, good to go" <+> pretty (takeFileName mfile)

          (mmIdx, nway) <- nwayHashMMapReadOnly fIndexNameA
                            >>= orThrow (NCQMergeInvariantFailed (show $ "can't mmap" <+> pretty fIndexNameA))

          debug $ "SCAN FILE A" <+> pretty fDataNameA

          writeFiltered ncq fDataNameA fwh $ \_ _ _ _ -> do
            pure True

          debug $ "SCAN FILE B" <+> pretty fDataNameA

          writeFiltered ncq fDataNameB fwh $ \_ _ k _ -> do
            foundInA <- liftIO (nwayHashLookup nway mmIdx (coerce k)) <&> isJust
            let skip = foundInA
            pure $ not skip

          appendTailSection =<< handleToFd fwh

        liftIO do

          result <- fileSize mfile

          idx <- if result == 0 then
                    pure Nothing
                   else do
                    fossil <- ncqGetNewFossilName ncq
                    mv mfile fossil
                    statA <- getFileStatus fDataNameA
                    let ts = modificationTimeHiRes statA
                    setFileTimesHiRes fossil ts ts
                    let fk = DataFile (fromString fossil)
                    void $ ncqIndexFile ncq fk
                    pure $ Just (ts,fk)

          atomically do
            modifyTVar ncqTrackedFiles (HPSQ.delete a)
            modifyTVar ncqTrackedFiles (HPSQ.delete b)
            for_ idx $ \(ts,fk) -> do
              ncqAddTrackedFileSTM ncq (coerce fk) (posixToTimeSpec ts)

          for_ idx $ \(ts,DataFile fk) -> do
            void $ ncqStateUpdate ncq [D a, D b, F (posixToTimeSpec ts) fk]

    orFail what e = do
      r <- what
      unless r (throwIO (NCQMergeInvariantFailed (show e)))


ncqCompact :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqCompact ncq@NCQStorage2{..} = do

  q <- newTVarIO ( mempty :: HashMap FileKey (HashSet HashRef) )

  ncqLinearScanForCompact ncq $ \fk h -> atomically do
    modifyTVar q (HM.insertWith (<>) fk (HS.singleton h))

  state0 <- readTVarIO q

  for_ (HM.toList state0) $ \(fk, es) -> do
    trace $ "TO DELETE" <+> pretty fk <+> pretty (HS.size es)

    let fDataNameA = ncqGetFileName ncq (toFileName $ DataFile fk)
    let fIndexNameA = ncqGetFileName ncq (toFileName (IndexFile fk))

    flip runContT pure do

      mfile <- ncqGetNewCompactName ncq

      ContT $ bracket none $ const do
        rm mfile

      liftIO do
        withBinaryFileAtomic mfile WriteMode $ \fwh -> do
            writeFiltered ncq fDataNameA fwh $ \_ _ k v -> do
              pure $ not $ HS.member k es
            appendTailSection =<< handleToFd fwh

        result <- fileSize mfile

        if result == 0 then do
          atomically $ modifyTVar ncqTrackedFiles (HPSQ.delete fk)
        else do

          fossil <- ncqGetNewFossilName ncq
          mv mfile fossil

          statA <- getFileStatus fDataNameA

          let ts = modificationTimeHiRes statA
          setFileTimesHiRes fossil ts ts

          fname <- ncqIndexFile ncq (DataFile (fromString fossil))

          atomically do
            let fp = fromString fname
            modifyTVar ncqTrackedFiles (HPSQ.delete fk)
            ncqAddTrackedFileSTM ncq fp (posixToTimeSpec ts)

        mapM_ rm [fDataNameA, fIndexNameA]

  debug $ "compact done" <+> pretty (HM.size state0)


-- NOTE: incremental
--   now it may became incremental if we'll
--   limit amount of tombs per one pass
--   then remove all dead entries,
--   then call again to remove tombs. etc
--   as for now, seems it should work up to 10TB
--   of storage
ncqLinearScanForCompact :: MonadUnliftIO m
                        => NCQStorage2
                        -> ( FileKey -> HashRef -> m () )
                        -> m Int
ncqLinearScanForCompact ncq@NCQStorage2{..} action = flip runContT pure do


  tracked <- readTVarIO ncqTrackedFiles <&> HPSQ.toList

  let state0 = mempty :: HashMap HashRef TimeSpec

  profit <- newTVarIO 0
  tombUse   <- newTVarIO (mempty :: HashMap HashRef (FileKey, Int))

  -- TODO: explicit-unmap-files

  flip fix  (tracked, state0) $ \next -> \case
    ([], s) -> none
    ((fk,p,_):rest, state) -> do

      let cqFile = ncqGetFileName ncq (toFileName (IndexFile fk))
      let dataFile = ncqGetFileName ncq (toFileName (DataFile fk))

      (mmaped,meta@NWayHash{..}) <- liftIO $ nwayHashMMapReadOnly cqFile
               >>= orThrow (NWayHashInvalidMetaData cqFile)

      let emptyKey = BS.replicate nwayKeySize 0

      found <- S.toList_ do
        nwayHashScanAll meta mmaped $ \o k entryBs -> do
          unless (k == emptyKey) do

            let off =  N.word64 (BS.take 8 entryBs)
            let sz   = N.word32 (BS.take 4 (BS.drop 8 entryBs))

            when (sz == ncqPrefixLen || sz ==  ncqPrefixLen + 32) do
                S.yield off

            let kk = coerce k

            case HM.lookup kk state of
              Just ts | ts > timeSpecFromFilePrio p -> do
                notice $ pretty kk <+> pretty (sz + ncqSLen)
                atomically do
                  modifyTVar profit ( + (sz + ncqSLen) )
                  modifyTVar tombUse (HM.adjust (over _2 succ) kk)
                lift $ lift $ action (fromString dataFile) kk

              _ -> none

      newEntries <- S.toList_ do
        unless (List.null found) do
         dataBs <- liftIO $ mmapFileByteString dataFile Nothing
         for_ found $ \o -> do
          let pre = BS.take (fromIntegral ncqPrefixLen) (BS.drop (ncqDataOffset o) dataBs)

          when (pre == ncqRefPrefix || pre == ncqTombPrefix) do
            let keyBs = BS.take ncqKeyLen (BS.drop (fromIntegral o + ncqSLen) dataBs)
            let key = coerce (BS.copy keyBs)
            unless (HM.member key state) do
              S.yield (key, timeSpecFromFilePrio p)
              when ( pre == ncqTombPrefix ) do
                atomically $ modifyTVar tombUse (HM.insert key (fk,0))

      next (rest, state <> HM.fromList newEntries)

  use <- readTVarIO tombUse
  let useless = [ (f,h) | (h, (f,n)) <- HM.toList use, n == 0 ]

  for_ useless $ \(f,h) -> do
    atomically $ modifyTVar profit (+ncqFullTombLen)
    lift $ action f h

  readTVarIO profit <&> fromIntegral


writeFiltered :: forall m . MonadIO m
              => NCQStorage2
              -> FilePath
              -> Handle
              -> ( Integer -> Integer -> HashRef -> ByteString -> m Bool)
              -> m ()

writeFiltered ncq fn out filt = do
  ncqStorageScanDataFile ncq fn $ \o s k v -> do
    skip <- filt o s k v <&> not

    when skip do
      debug $ pretty k <+> pretty "skipped"

    unless skip $ liftIO do
      BS.hPut out (LBS.toStrict (makeEntryLBS k v))

  where

    makeEntryLBS h bs = do
      let b = byteString (coerce @_ @ByteString h)
               <> byteString bs

      let wbs = toLazyByteString b
      let len = LBS.length wbs
      let ws  = byteString (N.bytestring32  (fromIntegral len))

      toLazyByteString (ws <> b)


zeroSyncEntry :: ByteString
zeroSyncEntry = ncqMakeSectionBS (Just B) zeroHash zeroPayload
  where zeroPayload = N.bytestring64 0
        zeroHash    = HashRef (hashObject zeroPayload)
{-# INLINE zeroSyncEntry #-}

zeroSyncEntrySize :: Word64
zeroSyncEntrySize = fromIntegral (BS.length zeroSyncEntry)
{-# INLINE zeroSyncEntrySize #-}

-- 1. It's M-record
-- 2. It's last w64be == fileSize
-- 3. It's hash == hash (bytestring64be fileSize)
-- 4. recovery-strategy: start-to-end, end-to-start
fileTailRecord :: Integral a => a -> ByteString
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

appendSection fh sect = do
  -- off <- liftIO $ fdSeek fh SeekFromEnd 0
  -- pure (fromIntegral off, fromIntegral len)
  liftIO (Posix.fdWrite fh sect) <&> fromIntegral
{-# INLINE appendSection #-}

appendTailSection :: MonadIO m => Fd -> m ()
appendTailSection fh = liftIO do
  s <- Posix.fileSize <$> Posix.getFdStatus fh
  void (appendSection fh (fileTailRecord s))
{-# INLINE appendTailSection #-}

