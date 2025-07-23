{-# Language MultiWayIf #-}
{-# Language RecordWildCards #-}
{-# Language PatternSynonyms #-}
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

import Numeric (showHex)
import Control.Applicative
import Data.ByteString.Builder
import Network.ByteOrder qualified as N
import Data.Bit.ThreadSafe qualified as BV
import Data.HashMap.Strict (HashMap)
import Control.Monad.Except
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Time.Clock.POSIX
import Data.Ord (Down(..),comparing)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TSem
import Data.Hashable (hash)
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

-- FIXME: ASAP-USE-FILE-LOCK
import System.FileLock as FL

type FOff = Word64

data  NCQEntry =
  NCQEntry
  { ncqEntryData   :: !ByteString
  , ncqDumped      :: !(TVar (Maybe FileKey))
  }

type Shard = TVar (HashMap HashRef NCQEntry)

type NCQOffset = Word64
type NCQSize   = Word32

type StateVersion = Word64

data NCQIdxEntry =
  NCQIdxEntry {-# UNPACK#-} !NCQOffset !NCQSize

data StateOP = D FileKey | F TimeSpec FileKey | P FileKey
               deriving (Eq,Ord,Show)

data NCQFlag =
  NCQMergeNow | NCQCompactNow
  deriving (Eq,Ord,Generic)

data Location =
       InFossil {-# UNPACK #-} !FileKey !ByteString !NCQOffset !NCQSize
     | InMemory {-# UNPACK #-} !ByteString

instance Pretty Location where
  pretty = \case
    InFossil k _ o s -> parens $ "in-fossil" <+> pretty k <+> pretty o <+> pretty s
    InMemory _     -> "in-memory"

data TrackedFile =
  TrackedFile
  { tfTime   :: FilePrio
  , tfKey    :: FileKey
  , tfCached :: TVar (Maybe CachedEntry)
  }

type TrackedFiles = Vector TrackedFile

data NCQStorage2 =
  NCQStorage2
  { ncqRoot           :: FilePath
  , ncqGen            :: Int
  , ncqSalt           :: HashRef
  , ncqPostponeMerge  :: Timeout 'Seconds
  , ncqPostponeSweep  :: Timeout 'Seconds
  , ncqLuckyNum       :: Int
  , ncqFsync          :: Int
  , ncqWriteQLen      :: Int
  , ncqWriteBlock     :: Int
  , ncqMinLog         :: Int
  , ncqMaxLog         :: Int
  , ncqMaxCached      :: Int
  , ncqIdleThrsh      :: Double
  , ncqMemTable       :: Vector Shard
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqWriteOps       :: Vector (TQueue (IO ()))
  , ncqReadReq        :: TQueue (HashRef, TMVar (Maybe Location))
  , ncqStorageTasks   :: TVar Int
  , ncqStorageStopReq :: TVar Bool
  , ncqStorageSyncReq :: TVar Bool
  , ncqMergeReq       :: TVar Bool
  , ncqMergeSem       :: TSem
  , ncqSyncNo         :: TVar Int
  , ncqCurrentFiles   :: TVar (HashSet FileKey)
  , ncqTrackedFiles   :: TVar TrackedFiles
  , ncqStateVersion   :: TVar StateVersion
  , ncqStateUsage     :: TVar (IntMap (Int, HashSet FileKey))
  , ncqStateName      :: TVar (Maybe StateFile)
  , ncqStateSem       :: TSem
  , ncqCachedEntries  :: TVar Int
  , ncqWrites         :: TVar Int
  , ncqWriteEMA       :: TVar Double  -- for writes-per-seconds
  , ncqJobQ           :: TQueue (IO ())
  , ncqMiscSem        :: TSem
  , ncqSweepSem       :: TSem
  , ncqMergeTasks     :: TVar Int
  , ncqOnRunWriteIdle :: TVar (IO ())

  }

megabytes :: forall a . Integral a => a
megabytes = 1024 ^ 2

gigabytes :: forall a . Integral a => a
gigabytes = 1024 ^ 3

ncqStorageOpen2 :: MonadIO m => FilePath -> (NCQStorage2 -> NCQStorage2)-> m NCQStorage2
ncqStorageOpen2 fp upd = do
  let ncqRoot       = fp
  let ncqGen        = 0
  let ncqFsync      = 16   * megabytes
  let ncqWriteQLen  = 1024 * 4
  let ncqMinLog     = 512 * megabytes
  let ncqMaxLog     = 16 * gigabytes -- ???
  let ncqWriteBlock = max 128 $ ncqWriteQLen `div` 2
  let ncqMaxCached  = 128
  let ncqIdleThrsh  = 50.00
  let ncqPostponeMerge = 600.00
  let ncqPostponeSweep = 2 * ncqPostponeMerge
  let ncqLuckyNum   = 2

  let shardNum = ncqLuckyNum * 2
  let wopNum   = ncqLuckyNum

  cap                <- getNumCapabilities <&> fromIntegral
  ncqWriteQ          <- newTVarIO mempty
  ncqMemTable        <- V.fromList <$> replicateM shardNum (newTVarIO mempty)
  ncqStorageStopReq  <- newTVarIO False
  ncqStorageSyncReq  <- newTVarIO False
  ncqMergeReq        <- newTVarIO False
  ncqMergeSem        <- atomically (newTSem 1)
  ncqSyncNo          <- newTVarIO 0
  ncqCurrentFiles    <- newTVarIO mempty
  ncqTrackedFiles    <- newTVarIO V.empty
  ncqStateVersion    <- newTVarIO 0
  ncqStateUsage      <- newTVarIO mempty
  ncqStateName       <- newTVarIO Nothing
  ncqStateSem        <- atomically $ newTSem 1
  ncqCachedEntries   <- newTVarIO 0
  ncqStorageTasks    <- newTVarIO 0
  ncqWrites          <- newTVarIO 0
  ncqWriteEMA        <- newTVarIO 0.00
  ncqJobQ            <- newTQueueIO
  ncqMiscSem         <- atomically (newTSem 1)
  ncqSweepSem        <- atomically (newTSem 1)
  ncqMergeTasks      <- newTVarIO 0
  ncqOnRunWriteIdle  <- newTVarIO none

  ncqReadReq <- newTQueueIO

  ncqWriteOps <- replicateM wopNum newTQueueIO <&> V.fromList

  let ncqSalt       = "EstEFasxrCFqsGDxcY4haFcha9e4ZHRzsPbGUmDfdxLk"

  let ncq = NCQStorage2{..} & upd

  mkdir (ncqGetWorkDir ncq)

  liftIO $ withSem ncqMergeSem do
    ncqRepair ncq
    ncqPreloadIndexes ncq
    ncqSweepStates ncq

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

ncqNewUniqFileName :: MonadIO m => NCQStorage2 -> FilePath -> FilePath -> m FilePath
ncqNewUniqFileName  me@NCQStorage2{..} pref suff = liftIO $ withSem ncqMiscSem do
 flip fix 0 $ \next i -> do
   t <- round @_ @Integer . (* 1e9) <$> getPOSIXTime
   let v = show $ pretty (showHex t "") <> "-" <> pretty (showHex i "")
   let n = ncqGetFileName  me (pref <> v <> suff)
   doesFileExist n >>= \case
    False -> pure n
    True  -> next (succ i)

ncqEmptyKey :: ByteString
ncqEmptyKey = BS.replicate ncqKeyLen 0

ncqGetNewFossilName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewFossilName me = ncqNewUniqFileName me "fossil-" ".data"

ncqGetNewStateName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewStateName me = ncqNewUniqFileName me "state-" ""

ncqGetNewCompactName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewCompactName me = ncqNewUniqFileName me "compact-" ".data"

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

ncqAlterEntrySTM :: NCQStorage2
                 -> HashRef
                 -> (Maybe NCQEntry -> Maybe NCQEntry)
                 -> STM ()
ncqAlterEntrySTM ncq h alterFn = do
  let shard = ncqGetShard ncq h
  modifyTVar shard (HM.alter alterFn h)

ncqPutBS :: MonadUnliftIO m
         => NCQStorage2
         -> Maybe NCQSectionType
         -> Maybe HashRef
         -> ByteString
         -> m HashRef
ncqPutBS ncq@NCQStorage2{..} mtp mhref bs' = do

  waiter <- newEmptyTMVarIO

  let work = do

        let h = fromMaybe (HashRef (hashObject @HbSync bs')) mhref
        let bs = ncqMakeSectionBS mtp h bs'
        let shard = ncqGetShard ncq h
        zero <- newTVarIO Nothing

        atomically do

          stop <- readTVar ncqStorageStopReq
          filled <- readTVar ncqWriteQ <&> Seq.length

          when (not stop && filled > ncqWriteQLen) STM.retry

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

ncqEntryUnwrap :: NCQStorage2
               -> ByteString
               -> (ByteString, Either ByteString (NCQSectionType, ByteString))
ncqEntryUnwrap n source = do
  let (k,v) = BS.splitAt ncqKeyLen (BS.drop 4 source)
  (k, ncqEntryUnwrapValue n v)
{-# INLINE ncqEntryUnwrap #-}

ncqEntryUnwrapValue :: NCQStorage2
                    -> ByteString
                    -> Either ByteString (NCQSectionType, ByteString)
ncqEntryUnwrapValue _  v = case ncqIsMeta v of
  Just meta -> Right (meta, BS.drop ncqPrefixLen v)
  Nothing   -> Left v
{-# INLINE ncqEntryUnwrapValue #-}

ncqIdxIsTombSize :: NCQIdxEntry -> Bool
ncqIdxIsTombSize (NCQIdxEntry _ s) = s == ncqSLen + ncqKeyLen + ncqPrefixLen
{-# INLINE ncqIdxIsTombSize #-}

ncqIsTomb :: NCQStorage2 -> Location -> Bool
ncqIsTomb me loc = case ncqEntryUnwrap me (ncqGetEntryBS me loc) of
  (_, Right (T, _)) -> True
  _                 -> False

ncqDelEntry :: MonadUnliftIO m
            => NCQStorage2
            -> HashRef
            -> m ()
ncqDelEntry me href = do
  -- всегда пишем tomb и надеемся на лучшее
  -- merge/compact разберутся
  -- однако не пишем, если записи еще нет
  ncqLocate2 me href >>= \case
    Just loc | not (ncqIsTomb me loc) -> do
      void $ ncqPutBS me (Just T) (Just href) ""

    _ -> none

ncqLookupEntry :: MonadUnliftIO m => NCQStorage2 -> HashRef -> m (Maybe NCQEntry)
ncqLookupEntry sto hash = atomically (ncqLookupEntrySTM sto hash)

ncqGetEntryBS :: NCQStorage2 -> Location -> ByteString
ncqGetEntryBS _ = \case
  InMemory bs -> bs
  InFossil _ mmap off size -> do
    BS.take (fromIntegral size) $ BS.drop (fromIntegral off) mmap

ncqEntrySize :: forall a . Integral a => Location -> a
ncqEntrySize = \case
  InFossil _ _ _ size -> fromIntegral size
  InMemory bs       -> fromIntegral (BS.length bs)

useVersion :: forall m a . MonadUnliftIO m => NCQStorage2 -> (() -> m a) -> m a
useVersion ncq m = bracket succV predV m
  where
    succV = atomically (ncqStateUseSTM ncq)
    predV = const $ atomically (ncqStateUnuseSTM ncq)

ncqListTrackedFilesSTM :: NCQStorage2 -> STM (Vector (FileKey, Maybe CachedEntry, TVar (Maybe CachedEntry)))
ncqListTrackedFilesSTM NCQStorage2{..} = do
  fs <- readTVar ncqTrackedFiles
  for fs $ \TrackedFile{..} -> (tfKey,,) <$> readTVar tfCached <*> pure tfCached

ncqListTrackedFiles :: MonadUnliftIO m => NCQStorage2 -> m (Vector (FileKey, Maybe CachedEntry, TVar (Maybe CachedEntry)))
ncqListTrackedFiles = atomically . ncqListTrackedFilesSTM


ncqPreloadIndexes :: MonadUnliftIO m
                  => NCQStorage2
                  -> m ()
ncqPreloadIndexes me@NCQStorage2{..} = useVersion me $ const do
  fs <- readTVarIO ncqTrackedFiles  <&> take ncqMaxCached . V.toList
  flip fix (fs, ncqMaxCached)  $ \next (files,lim) -> do
    case files of
      (t@TrackedFile{..}:rest) | lim > 0 -> do
        readTVarIO tfCached >>= \case
          Nothing -> do
            void $ ncqLoadTrackedFile me t
            next (rest, pred lim)
          _ -> next (rest, lim)

      _ -> none

ncqLoadTrackedFile :: MonadUnliftIO m
                   => NCQStorage2
                   -> TrackedFile
                   -> m (Maybe CachedEntry)
ncqLoadTrackedFile ncq@NCQStorage2{..} TrackedFile{..} = runMaybeT do

  let indexFile = ncqGetFileName ncq (toFileName (IndexFile tfKey))
  let dataFile  = ncqGetFileName ncq (toFileName (DataFile tfKey))

  idxHere <- liftIO $ doesFileExist indexFile
  unless idxHere do
    liftIO $ err $ red "missed index" <+> "in loadIndex" <+> pretty tfKey
    mzero

  (idxBs, idxNway) <- MaybeT $
    liftIO (nwayHashMMapReadOnly indexFile)

  datBs <- liftIO $ mmapFileByteString dataFile Nothing

  tnow <- liftIO $ newTVarIO =<< getTimeCoarse
  let ce = CachedEntry idxBs datBs idxNway tnow

  atomically do
    writeTVar tfCached (Just ce)
    modifyTVar ncqCachedEntries (+1)
    evictIfNeededSTM ncq (Just 1)

  pure ce

data Seek a = SeekStop !a | SeekNext !a


---

ncqSeekInFossils :: forall a f m . (MonadUnliftIO m, Monoid (f a))
                 => NCQStorage2
                 -> HashRef
                 -> (Location -> m (Seek (f a)))
                 -> m (f a)
ncqSeekInFossils ncq@NCQStorage2{..} href action = useVersion ncq $ const do
  tracked <- readTVarIO ncqTrackedFiles
  let l = V.length tracked

  let
    go :: Int -> Int -> f a -> m (f a)
    go i a r
      | i >= l = pure r
      | a > 1 = do
          let TrackedFile{..} = tracked ! i
          err $ "unable to load fossil" <+> pretty tfKey
          go (i+1) 0 r
      | otherwise = do
          let TrackedFile{..} = tracked ! i
          readTVarIO tfCached >>= \case

            Just PendingEntry{} ->
              go (i+1) 0 r

            Nothing -> do
              void $ ncqLoadTrackedFile ncq TrackedFile{..}
              go i (a+1) r

            Just CachedEntry{..} -> do
              liftIO (ncqLookupIndex href (cachedMmapedIdx, cachedNway)) >>= \case
                Nothing -> go (i+1) 0 r
                Just (NCQIdxEntry offset size) -> do
                  now <- getTimeCoarse
                  atomically $ writeTVar cachedTs now
                  action (InFossil tfKey cachedMmapedData offset size) >>= \case
                    SeekStop e -> pure (r <> e)
                    SeekNext e -> go (i+1) 0 (r <> e)

  go 0 0 mempty


ncqLookupIndex :: MonadUnliftIO m
               => HashRef
               -> (ByteString, NWayHash)
               -> m (Maybe NCQIdxEntry )
ncqLookupIndex hx (mmaped, nway) = do
  fmap decodeEntry <$> nwayHashLookup nway mmaped (coerce hx)
{-# INLINE ncqLookupIndex #-}

decodeEntry :: ByteString -> NCQIdxEntry
decodeEntry entryBs = do
    let (p,r) = BS.splitAt 8 entryBs
    let off = fromIntegral (N.word64 p)
    let size = fromIntegral (N.word32 (BS.take 4 r))
    NCQIdxEntry off size
{-# INLINE decodeEntry #-}

ncqLocate2 :: MonadUnliftIO m => NCQStorage2 -> HashRef -> m (Maybe Location)
ncqLocate2 NCQStorage2{..} href = do
  answ <- newEmptyTMVarIO

  atomically do
    modifyTVar ncqWrites succ
    writeTQueue ncqReadReq (href, answ)

  atomically $ takeTMVar answ

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
        debug $ red "CLOSE FILE" <+> pretty fk
        closeFd fh
        debug $ yellow "indexing" <+> pretty fk
        idx <- ncqRunTaskNoMatterWhat ncq (ncqIndexFile ncq (DataFile fk))
        ncqRunTaskNoMatterWhat ncq $ ncqStateUpdate ncq [F 0 fk]
        nwayHashMMapReadOnly idx >>= \case
          Nothing -> err $ "can't open index" <+> pretty idx
          Just (bs,nway) -> do
            nwayHashScanAll nway bs $ \_ k _ -> do
              unless (k == emptyKey) $ atomically $ void $ runMaybeT do
                NCQEntry _ tfk <- MaybeT $ ncqLookupEntrySTM ncq (coerce k)
                fk' <- MaybeT $ readTVar tfk
                guard (fk == fk') -- remove only own stuff
                lift $ ncqAlterEntrySTM ncq (coerce k) (const Nothing)

        ncqPreloadIndexes ncq
        atomically (modifyTVar ncqCurrentFiles (HS.delete fk))
        loop

  spawnActivity $ forever (liftIO $ join $ atomically (readTQueue ncqJobQ))

  replicateM_ 2 $ spawnActivity $ fix \next -> do
      (h, answ) <- atomically $ readTQueue ncqReadReq

      let answer l = atomically (putTMVar answ l)

      let lookupCached fk = \case
            PendingEntry{}  -> none
            CachedEntry{..} -> do
              ncqLookupIndex h (cachedMmapedIdx, cachedNway) >>= \case
                Nothing -> none
                Just (NCQIdxEntry offset size) -> do
                  answer (Just (InFossil fk cachedMmapedData offset size))
                  next
          {-# INLINE lookupCached #-}

      ncqLookupEntry ncq h >>= \case
        Nothing  -> none
        Just e -> answer (Just (InMemory (ncqEntryData e))) >> next

      useVersion ncq $ const do

        tracked <- readTVarIO ncqTrackedFiles

        for_ tracked $ \(TrackedFile{..}) -> do
            readTVarIO tfCached >>= \case
              Just ce -> lookupCached tfKey ce
              Nothing -> ncqLoadTrackedFile ncq TrackedFile{..} >>= \case
                Nothing -> err $ "unable to load index" <+> pretty tfKey
                Just ce -> lookupCached tfKey ce

      next

  let shLast = V.length ncqWriteOps - 1
  spawnActivity $  pooledForConcurrentlyN_ (V.length ncqWriteOps) [0..shLast] $ \i -> do
    let q = ncqWriteOps ! i
    forever (liftIO $ join $ atomically (readTQueue q))

  spawnActivity measureWPS

  -- FIXME: bigger-period
  spawnActivity $ postponed ncqPostponeSweep $ forever $ (>> pause @'Seconds 120) $ do
    ema <- readTVarIO ncqWriteEMA
    n <- ncqListStateFiles ncq <&> List.length
    when (ema < ncqIdleThrsh * 1.5 && n > 0) $ withSem ncqMergeSem do
      debug $ yellow "run sweep routine"
      ncqSweepStates ncq
      ncqSweepFossils ncq

  spawnActivity $ postponed ncqPostponeMerge $ fix \again -> (>> again) do
      ema <- readTVarIO ncqWriteEMA
      mergeReq <- atomically $ stateTVar ncqMergeReq (,False)

      debug $ green "MERGE ATTEMPT" <+> pretty ema <+> "~" <+> pretty ncqIdleThrsh

      let notPending x = List.length [ k | (k,e,_) <- V.toList x, isNotPending e  ]

      if ema > ncqIdleThrsh && not mergeReq then do
        pause @'Seconds 10

      else do
        mq <- newEmptyTMVarIO

        spawnJob $ do
          merged <- ncqMergeStep ncq
          atomically $ putTMVar mq merged

        -- TODO: detect-dead-merge
        void $ race (pause @'Seconds 300) (atomically $ readTMVar mq) >>= \case
          Left{}     -> warn $ yellow "MERGE FUCKING STALLED"
          Right True -> none
          Right False -> do

            debug "merge: all done, wait..."
            n0 <- ncqListTrackedFiles ncq <&> notPending

            -- FIXME: bigger-timeout
            void $ race (pause @'Seconds 60) do
              atomically do
                n <- ncqListTrackedFilesSTM ncq <&> notPending
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
        liftIO (ncqStateUpdate ncq [P fk])
        debug $ "openNewDataFile" <+> pretty fk
        loop $ RunWrite (fk,fhx,0,0)

    RunSync (fk, fh, w, total, continue) -> do

      stop <- readTVarIO ncqStorageStopReq
      sync <- readTVarIO ncqStorageSyncReq

      let needClose = total >= ncqMinLog || stop

      rest <- if not (sync || needClose || w > ncqFsync) then
                  pure w
                else do
                  appendTailSection fh >> liftIO (fileSynchronise fh)
                  -- FIXME: slow!
                  -- to make it appear in state, but to ignore until index is done
                  atomically do
                    writeTVar  ncqStorageSyncReq False
                    modifyTVar ncqSyncNo succ

                  pure 0

      if | needClose && continue -> do
              atomically $ writeTQueue closeQ (fk, fh)
              loop RunNew

         | not continue -> loop RunFin

         | otherwise -> loop $ RunWrite (fk, fh, rest, total)

    RunWrite (fk, fh, w, total') -> do

      let timeoutMicro = 10_000_000

      chunk <- liftIO $ timeout timeoutMicro $ atomically do
        stop  <- readTVar ncqStorageStopReq
        sy    <- readTVar ncqStorageSyncReq
        chunk <- stateTVar ncqWriteQ (Seq.splitAt ncqWriteBlock)

        if | Seq.null chunk && stop             -> pure $ Left ()
           | Seq.null chunk && not (stop || sy) -> STM.retry
           | otherwise                          -> pure $ Right chunk

      case chunk of
        Nothing -> do
          liftIO $ join $ readTVarIO ncqOnRunWriteIdle
          if w == 0 then do
            loop $ RunWrite (fk,fh,w,total')
          else do
            atomically $ writeTVar ncqStorageSyncReq True
            loop $ RunSync (fk, fh, w, total', True) -- exit ()

        Just (Left{})  -> loop $ RunSync (fk, fh, w, total', False) -- exit ()

        Just (Right chu) -> do
          ws <- for chu $ \h -> do
                  atomically (ncqLookupEntrySTM ncq h) >>= \case
                    Just (NCQEntry bs w)  -> do
                      atomically (writeTVar w (Just fk))
                      lift (appendSection fh bs)

                    _ -> pure 0

          let written = sum ws
          loop $ RunSync (fk, fh, w + written, total' + written, True)

  where

    emptyKey = ncqEmptyKey

    openNewDataFile :: forall mx . MonadIO mx => mx (FileKey, Fd)
    openNewDataFile = do
      fname <- ncqGetNewFossilName ncq
      atomically $ modifyTVar ncqCurrentFiles (HS.insert (fromString fname))
      touch fname
      let flags = defaultFileFlags { exclusive = False, creat = Just 0o666 }
      (fromString fname,) <$> liftIO (PosixBase.openFd fname  Posix.ReadWrite flags)

    spawnJob = ncqSpawnJob ncq

    postponed n m = liftIO (pause @'Seconds n) >> m

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
        atomically $ modifyTVar ncqWriteEMA \ema -> alpha * (dw/dt) + 0.9 * ema
        pause @'Seconds step >> loop (Just (w1,t1))

      where
        alpha = 0.1
        step  = 1.00

ncqSpawnJob :: forall m . MonadIO m => NCQStorage2 -> IO () -> m ()
ncqSpawnJob NCQStorage2{..} m = atomically $ writeTQueue ncqJobQ m

ncqFileFastCheck :: MonadUnliftIO m => FilePath -> m ()
ncqFileFastCheck fp = do

  -- debug $ "ncqFileFastCheck" <+> pretty fp

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
    ncqStorageScanDataFile n fp $ \o w k s -> case ncqIsMeta s of
      Just M -> none
      _ -> do
        -- we need size in order to return block size faster
        -- w/o search in fossil
        let rs = (w + ncqSLen) & fromIntegral @_ @Word32 & N.bytestring32
        let os = fromIntegral @_ @Word64 o & N.bytestring64
        let record = os <> rs
        S.yield (coerce k, record)

  let (dir,name) = splitFileName fp
  let idxTemp = (dropExtension name <> "-") `addExtension` ".cq$"

  result <- nwayWriteBatch (nwayAllocDef 1.10 32 8 12) dir idxTemp items

  mv result dest

  ncqStateUpdate n [F 0 (coerce fk)]

  pure dest


ncqAddTrackedFiles :: MonadIO m => NCQStorage2 -> [DataFile FileKey] -> m ()
ncqAddTrackedFiles ncq@NCQStorage2{..} files = flip runContT pure do
  valid <- for files \fkey -> callCC \skip -> do
    let fname   = ncqGetFileName ncq (toFileName fkey)
    let idxName = ncqGetFileName ncq (toFileName (IndexFile (coerce @_ @FileKey fkey)))

    idxHere <- doesFileExist idxName
    unless idxHere do
      err $ "Index does not exist" <+> pretty (takeFileName idxName)
      skip Nothing

    stat <- liftIO $ PFS.getFileStatus fname
    let ts = posixToTimeSpec $ PFS.modificationTimeHiRes stat
    let fk = fromString (takeFileName fname)

    pure $ Just (ts, fk)

  atomically $ ncqAddTrackedFilesSTM ncq (catMaybes valid)


ncqAddTrackedFilesSTM :: NCQStorage2 -> [(TimeSpec, FileKey)] -> STM ()
ncqAddTrackedFilesSTM NCQStorage2{..} newFiles = do
  a <- readTVar ncqTrackedFiles <&> V.toList
  let already = HS.fromList (map tfKey a)

  b <- for newFiles \(t, f) ->
    if f `HS.member` already
      then pure Nothing
      else do
        tv <- newTVar Nothing
        pure . Just $ TrackedFile (FilePrio (Down t)) f tv

  let new = V.fromList $ List.sortOn tfTime (a <> catMaybes b)
  writeTVar ncqTrackedFiles new
{-# INLINE ncqAddTrackedFilesSTM #-}

evictIfNeededSTM :: NCQStorage2 -> Maybe Int -> STM ()
evictIfNeededSTM me@NCQStorage2{..} howMany = do
  cur <- readTVar ncqCachedEntries

  let need   = fromMaybe cur howMany
      excess = max 0 (cur + need - ncqMaxCached)

  when (excess > 0) do
    files <- ncqListTrackedFilesSTM me

    oldest <- forM (V.toList files) \case
      (k, Just (CachedEntry{..}) , t) -> do
        ts <- readTVar cachedTs
        pure (Just (ts, k, t))
      _ -> pure Nothing

    let victims = oldest & catMaybes & List.sortOn (view _1) & List.take excess

    for_ victims $ \(_,_,t) -> do
      writeTVar t Nothing
      modifyTVar ncqCachedEntries (subtract 1)

{- HLINT ignore "Functor law" -}

ncqListDirFossils :: MonadIO m => NCQStorage2 -> m [FilePath]
ncqListDirFossils ncq = do
  let wd = ncqGetWorkDir ncq
  dirFiles wd
     >>= mapM (pure . takeFileName)
     <&> List.filter (\f -> List.isPrefixOf "fossil-" f && List.isSuffixOf ".data" f)
     <&> HS.toList . HS.fromList

ncqListStateFiles :: forall m . MonadIO m => NCQStorage2 -> m [(TimeSpec, StateFile)]
ncqListStateFiles ncq = do
  let wd = ncqGetWorkDir ncq
  dirFiles wd
     >>= mapM (pure . takeBaseName)
     <&> List.filter (List.isPrefixOf "state-")
     >>= mapM timespecOf
     <&> fmap (over _2 fromString) . rights
     <&> List.sortOn Down

  where
    timespecOf x = liftIO @m $ try @_ @IOException do
      (,x) . posixToTimeSpec . modificationTimeHiRes <$> getFileStatus (ncqGetFileName ncq x)

ncqRepair :: MonadIO m => NCQStorage2 -> m ()
ncqRepair me@NCQStorage2{..} = do
  states <- ncqListStateFiles me <&> fmap snd

  fossils <- flip fix states $ \next -> \case
    [] -> do
      debug $ yellow "no valid state found; start from scratch"
      ncqListDirFossils me <&> fmap (DataFile . fromString)

    (s:ss) -> tryLoadState s >>= \case
      Just files -> do
        debug $ yellow "used state" <+> pretty s
        atomically $ writeTVar ncqStateName (Just s)
        pure files
      Nothing -> do
        warn $ red "inconsistent state" <+> pretty s
        rm (ncqGetFileName me $ toFileName s)
        next ss

  ncqAddTrackedFiles me fossils

  void $ liftIO (ncqStateUpdate me mempty)

  where

    readState path = ncqReadStateKeys  me path <&> fmap DataFile

    tryLoadState (fk :: StateFile) = liftIO do

      debug $ "tryLoadState" <+> pretty fk

      state <- readState fk

      let checkFile fo = flip fix 0 $ \next i -> do
            let dataFile = ncqGetFileName me (toFileName fo)
            let indexFile = ncqGetFileName me (toFileName (IndexFile (coerce @_ @FileKey fo)))

            here <- doesFileExist dataFile

            if not here then do
              rm indexFile
              pure False

            else do

              try @_ @SomeException (ncqFileFastCheck dataFile) >>= \case

                Left e -> do
                  err (viaShow e)
                  here <- doesFileExist dataFile
                  when here do
                    let broken = dropExtension dataFile `addExtension` ".broken"
                    mv dataFile broken
                    rm indexFile
                    warn $ red "renamed" <+> pretty dataFile <+> pretty broken

                  pure False

                Right{} | i > 1 -> pure False

                Right{} -> do
                  exists <- doesFileExist indexFile
                  if exists then do
                    pure True
                  else do
                    debug $ "indexing" <+> pretty (toFileName fo)
                    r <- ncqIndexFile me fo
                    debug $ "indexed" <+> pretty r
                    next (succ i)

      results <- forM state checkFile
      pure $ if and results then Just state else Nothing


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
  modifyTVar ncqStateUsage (IntMap.update (Just . over _1 succ) k)
{-# INLINE ncqStateUseSTM #-}

ncqStateUnuseSTM :: NCQStorage2 -> STM ()
ncqStateUnuseSTM NCQStorage2{..} = do
  k <- readTVar ncqStateVersion <&> fromIntegral
  -- TODO: remove when n <= 0
  modifyTVar ncqStateUsage (IntMap.update (Just . over _1 pred) k)
{-# INLINE ncqStateUnuseSTM #-}

ncqStateUpdate :: MonadUnliftIO m => NCQStorage2 -> [StateOP] -> m Bool
ncqStateUpdate me@NCQStorage2{..} ops' = withSem ncqStateSem $ flip runContT pure $ callCC \exit -> do

  debug $ "ncqStateUpdate" <+> viaShow ops'

  t1 <- FilePrio . Down <$> liftIO getTimeCoarse

  ops <- checkWithDisk $ \name -> do
            err $ "ncqStateUpdate invariant fail" <+> pretty name
            exit False

  changed <- atomically do
    current' <- readTVar ncqTrackedFiles <&> V.toList

    memStateVersionSTM (HS.fromList (fmap tfKey current'))

    let current = HM.fromList [ (tfKey, e) | e@TrackedFile{..} <- current' ]

    wtf <- flip fix (current, ops) $ \next (s, o) -> case o of
              [] -> pure s

              (D fk : rest) -> next (HM.delete fk s, rest)

              (P fk : rest) | HM.member fk current -> next (s, rest)
                            | otherwise -> do
                                e <- TrackedFile t1 fk <$> newTVar (Just PendingEntry)
                                next (HM.insert fk e s, rest)

              (F t fk : rest) -> do
                case HM.lookup fk s of
                  Nothing -> do
                    new <- TrackedFile (FilePrio (Down t)) fk <$> newTVar Nothing
                    next (HM.insert fk new s, rest)

                  Just TrackedFile{..} -> do
                    pe <- readTVar tfCached
                    if isNotPending pe then
                      next (s, rest)
                    else do
                      writeTVar tfCached Nothing
                      next (s, rest)

    writeTVar ncqTrackedFiles (V.fromList $ List.sortOn tfTime (HM.elems wtf))

    pure (HM.keysSet current /= HM.keysSet wtf)

  -- let fks = HS.fromList [ fk | F _ fk <- ops ]
  -- tra <- lift $ ncqListTrackedFiles me <&> filter (not . isNotPending . view _2)  . V.toList
  -- let tra2 = [ k |  (k,_,_) <- tra, HS.member k fks ]

  -- unless (List.null tra2) do
  --   err $ red "FUCKED" <+> pretty tra2

  when changed $ liftIO do
     name <- ncqDumpCurrentState me
     atomically $ writeTVar ncqStateName (Just name)
     debug $ green "switched state" <+> pretty name

  -- a1 <- V.toList <$> lift (ncqListTrackedFiles me)

  -- let fsz = HS.fromList [ fk | F _ fk <- ops ]

  -- let p1 =  [ fk | (fk, Just PendingEntry{}, _) <- a1, HS.member fk fsz   ]

  -- unless (List.null p1)  do
  --   error $ show $ "PIZDA!" <+> pretty p1 <> line <> viaShow ops'

  pure changed

  where

    memStateVersionSTM currentKeys = do
      k0  <- readTVar ncqStateVersion <&> fromIntegral

      let doAlter = \case
            Nothing    -> Just (0, currentKeys)
            Just (u,f) -> Just (u,f)

      modifyTVar ncqStateUsage (IntMap.alter doAlter k0)

    checkWithDisk onFail = for ops' $ \case  --
      f@(F _ fk) -> do
        let datFile = ncqGetFileName me (toFileName $ DataFile fk)
        e2 <- doesFileExist datFile
        unless e2 (onFail datFile)

        ts <- liftIO (getFileStatus datFile) <&>
                 posixToTimeSpec . PFS.modificationTimeHiRes

        pure (F ts fk)

      d    -> pure d


ncqDumpCurrentState :: MonadUnliftIO m => NCQStorage2 -> m StateFile
ncqDumpCurrentState me@NCQStorage2{..} = do
  files <- ncqListTrackedFiles me
  name <- ncqGetNewStateName me
  writeBinaryFileDurableAtomic name (BS8.unlines [coerce k | (k,_,_) <- V.toList files])
  pure $ fromString name

ncqMergeFull :: forall m . MonadUnliftIO m => NCQStorage2 -> m ()
ncqMergeFull me = fix \next -> ncqMergeStep me >>= \case
  False -> none
  True  -> next

-- FIXME: sometime-causes-no-such-file-or-directory
ncqMergeStep :: MonadUnliftIO m => NCQStorage2 -> m Bool
ncqMergeStep ncq@NCQStorage2{..}  = do
  withSem ncqMergeSem $ ncqRunTask ncq False do

    debug "ncqMergeStep"

    tracked <- ncqListTrackedFiles ncq

    files <- for tracked $ \(f,e,_) ->  do

      let fn = ncqGetFileName ncq (toFileName $ DataFile f)
      let idx = ncqGetFileName ncq (toFileName $ IndexFile f)

      dataHere <- doesFileExist  fn

      sz <- case e of
              Just PendingEntry -> pure (-100)
              _ | dataHere      -> liftIO (fileSize fn)
                | otherwise     -> pure (-3)

      idxHere <- doesFileExist idx

      pure (f, sz, idxHere)

    -- debug $ red "MERGE FILES" <+> viaShow files

    let bothOk (_, sz1, here1) (_, sz2, here2) =
          here1 && here2
                && sz1 > 0 && sz2 > 0
                && (sz1 + sz2) < fromIntegral ncqMaxLog

    found <- flip fix (V.toList files, Nothing, Nothing) $ \next -> \case
          ([], _, r) -> pure r

          (a:b:rest, Nothing, _) | bothOk a b -> do
            next (b:rest, Just (view _2 a + view _2 b), Just (a,b))

          (a:b:rest, Just s, _ ) | bothOk a b && (view _2 a + view _2 b) < s -> do
            next (b:rest, Just (view _2 a + view _2 b), Just (a,b))

          (_:rest, s, r) -> do
            next (rest, s, r)

    case found of
      Just (a,b) -> mergeStep a b >> pure True
      _          -> do
        debug "merge: not found shit"
        pure False

  where

    ncqGetNewMergeName :: MonadIO m => NCQStorage2 -> m FilePath
    ncqGetNewMergeName n@NCQStorage2{} = do
      let (p,tpl) = splitFileName (ncqGetFileName n "merge-.data")
      liftIO $ emptyTempFile p tpl

    mergeStep (a,_,_) (b,_,_) = do
      debug $ "merge" <+> pretty a <+> pretty b

      let fDataNameA  = ncqGetFileName ncq $ toFileName (DataFile a)
      let fIndexNameA = ncqGetFileName ncq $ toFileName (IndexFile a)

      let fDataNameB  = ncqGetFileName ncq $ toFileName (DataFile b)
      let fIndexNameB  = ncqGetFileName ncq $ toFileName (IndexFile b)

      -- TODO: proper-exception-handling
      doesFileExist fDataNameA   `orFail` ("not exist" <+> pretty fDataNameA)
      doesFileExist fDataNameB   `orFail` ("not exist" <+> pretty fDataNameB)
      doesFileExist fIndexNameA  `orFail` ("not exist" <+> pretty fIndexNameA)
      doesFileExist fIndexNameB  `orFail` ("not exist" <+> pretty fIndexNameB)

      flip runContT pure $ callCC \exit -> do

        mfile <- ncqGetNewMergeName ncq

        ContT $ bracket none $ const do
          rm mfile

        liftIO $ withBinaryFileAtomic mfile WriteMode $ \fwh -> do

          debug $ "merge: okay, good to go" <+> pretty (takeFileName mfile)

          idxA <- nwayHashMMapReadOnly fIndexNameA
                            >>= orThrow (NCQMergeInvariantFailed (show $ "can't mmap" <+> pretty fIndexNameA))


          idxB <- nwayHashMMapReadOnly fIndexNameB
                            >>= orThrow (NCQMergeInvariantFailed (show $ "can't mmap" <+> pretty fIndexNameB))

          debug $ "SCAN FILE A" <+> pretty fDataNameA

          -- we write only record from A, that last in index(A) and not meta

          writeFiltered ncq fDataNameA fwh $ \o _ k v -> do
            let meta = Just M == ncqIsMeta v
            liftIO (ncqLookupIndex (coerce k) idxA ) >>= \case
                    Just (NCQIdxEntry o1 _) | o1 == fromIntegral o -> pure $ not meta
                    _                     -> pure $ False

          -- we write only record from B, that last in index(B)
          -- and not meta and not already written 'A' pass

          debug $ "SCAN FILE B" <+> pretty fDataNameA

          writeFiltered ncq fDataNameB fwh $ \o _ k v -> do
            let meta = Just M == ncqIsMeta v
            foundInA <- liftIO (ncqLookupIndex  (coerce k) idxA) <&> isJust
            actual <- liftIO (ncqLookupIndex (coerce k) idxB ) >>= \case
              Just (NCQIdxEntry o1 _) | o1 == fromIntegral o -> pure $ not meta
              _                     -> pure $ False

            pure $ not ( foundInA || meta || not actual )

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

          for_ idx $ \(ts,DataFile fk) -> do
            void $ ncqStateUpdate ncq [D a, D b, F (posixToTimeSpec ts) fk]

    orFail what e = do
      r <- what
      unless r (throwIO (NCQMergeInvariantFailed (show e)))

ncqCompactStep :: forall m .  MonadUnliftIO m => NCQStorage2 -> m ()
ncqCompactStep me@NCQStorage2{..} = withSem ncqMergeSem $ flip runContT pure $ callCC \exit -> do
  ContT $ useVersion me

  files <- lift (ncqListTrackedFiles me)
             <&> filter (isNotPending . view _2) . V.toList
             <&> fmap (view _1)
             <&> zip [0 :: Int ..]
             <&> IntMap.fromList

  (i,fkA,tombsA) <- lift (findFileA files) >>= maybe (exit ()) pure

  let (_,_,rest) = IntMap.splitLookup i files

  garbage0 <- lift $ getGarbageSlow fkA mempty

  -- FIXME: hardcode
  (j,fkB,tombsB) <- lift (findClosestAmongst rest (HM.keysSet garbage0) 0.15)
                       >>= maybe (exit ()) pure

  notice $ "found" <+> pretty fkA <+> pretty fkB


  -- for_ (IntMap.elems rest) $ \fk -> do

  --   let datF = ncqGetFileName me (toFileName (DataFile fk))
  --   dataSize <- liftIO (fileSize datF)
  --   garbage <- lift $ getGarbageSlow fk tombsA

  --   let realProfit = sum (HM.elems garbage)
  --   let kUse = realToFrac realProfit / (1 + realToFrac dataSize)  :: Fixed E3


  --   notice $ "profit" <+> pretty fk <+> pretty dataSize <+> pretty realProfit <+> pretty kUse

  -- (aIdx, fileA, nTombs) <- findFileA files >>= maybe (exit  ()) pure

  -- notice $ green "compact: fileA" <+> pretty fileA <+> pretty aIdx <+> pretty nTombs

  -- idxA  <- lift (viewIndex fileA)
  -- tombs <- lift (getTombsInIndex  idxA)

  -- let (_,self,b) = IntMap.splitLookup aIdx files

  -- notice $ green "pretty" <+> viaShow b

  -- for_ (IntMap.elems b) $ \fk -> callCC \skip -> do
  --   profit <- lift (getProfit fk tombs)

  --   let datF = ncqGetFileName me (toFileName (DataFile fk))
  --   here <- doesFileExist datF

  --   unless here do
  --     throwIO (NCQCompactInvariantFailed (show $ "fossil exists" <+> pretty fk))

  --   dataSize <- liftIO (fileSize datF)

  --   when (dataSize == 0) do
  --     notice $ "skipped" <+> pretty fk <+> pretty dataSize <+> pretty profit
  --     skip ()

  --   garbage <- lift (getGargabeSlow fk mempty)
  --   let realProfit = sum (HM.elems garbage)

  --   let pfl =  (realToFrac realProfit / realToFrac dataSize) & realToFrac @_ @(Fixed E6)

  --   notice $ "profit" <+> pretty fk <+> pretty profit <+> pretty dataSize <+> pretty pfl <+> pretty realProfit

  --   none

  where

    findFileA files = do
      tnums <- for (IntMap.toList files) $ \(i, fk)  -> (i, fk,)  <$> (getTombsInIndex =<< viewIndex fk)
      pure $ listToMaybe ( List.sortOn ( Down . HS.size . view _3 ) tnums )

    findClosestAmongst rest tombs ratio = flip runContT pure $ callCC \exit -> do

      for_ (IntMap.toList rest) $ \(i,fk) -> do

        let datF = ncqGetFileName me (toFileName (DataFile fk))
        dataSize <- liftIO (fileSize datF)
        garbage  <- lift (getGarbageSlow fk tombs)

        let realProfit = sum (HM.elems garbage)
        let kUse = realToFrac realProfit / (1 + realToFrac dataSize)

        when (kUse >= ratio) $ exit $ Just (i, fk, HM.keysSet garbage)

      pure Nothing

    viewIndex fk = do
      let idxf = ncqGetFileName me $ toFileName (IndexFile fk)
      liftIO (nwayHashMMapReadOnly idxf)
                         >>= orThrow (NCQCompactInvariantFailed (show $ "index exists" <+> pretty fk))

    getTombsInIndex :: MonadUnliftIO m => (ByteString, NWayHash) -> m (HashSet HashRef)
    getTombsInIndex (idxBs, nway) =  HS.fromList <$> S.toList_ do
      nwayHashScanAll nway idxBs $ \_ k v -> do
        when (k /= ncqEmptyKey && ncqIdxIsTombSize (decodeEntry v) ) do
            S.yield (coerce @_ @HashRef k)

    getProfit :: MonadIO m => FileKey -> HashSet HashRef -> m NCQSize
    getProfit fk tombs = do
      (bs,nw) <- viewIndex fk
      r <- S.toList_ $ nwayHashScanAll nw bs$ \_ k v -> do
        when (HS.member (coerce k) tombs) $ S.yield $ let (NCQIdxEntry _ s) = decodeEntry v in s
      pure (sum r)

    getGarbageSlow :: MonadUnliftIO m => FileKey -> HashSet HashRef -> m (HashMap HashRef NCQSize)
    getGarbageSlow fk tombs = do
      let datFile = ncqGetFileName me (toFileName $ DataFile fk)
      idx <- viewIndex fk

      r <- newTVarIO mempty

      ncqStorageScanDataFile me datFile $ \o s k v -> do
        case ncqEntryUnwrapValue me v of
          Left bs -> atomically $ modifyTVar' r (HM.insertWith (+) k (fromIntegral s))
          Right (t, bs) -> do
            ncqLookupIndex k idx  >>= \case
              Nothing -> do
                -- notice $ "not found in index" <+> pretty k
                atomically $ modifyTVar' r (HM.insertWith (+) k (fromIntegral s))

              Just (NCQIdxEntry oi _) -> do
                let garbage = HS.member k tombs || oi /= fromIntegral o
                when garbage do
                  -- notice $ "offset mismatch or tomb" <+> pretty o <+> pretty oi <+> pretty k
                  when garbage $ atomically do
                    modifyTVar' r (HM.insertWith (+) k (fromIntegral s))

      readTVarIO r

ncqReadStateKeys :: forall m .  MonadUnliftIO m => NCQStorage2 -> StateFile -> m [FileKey]
ncqReadStateKeys me path = liftIO do
  keys <- BS8.readFile (ncqGetFileName me (toFileName path))
           <&> filter (not . BS8.null) . BS8.lines
  pure $ fmap (coerce @_ @FileKey) keys

ncqSweepFossils :: forall m . MonadUnliftIO m => NCQStorage2 -> m ()
ncqSweepFossils me@NCQStorage2{..} = withSem ncqSweepSem do
  debug $ yellow "sweep fossils"

  -- better be safe than sorry

  current <- readTVarIO ncqCurrentFiles

  sfs <- ncqListStateFiles me

  debug $ "STATE FILES" <+> vcat (fmap pretty sfs)

  mentioned <- mapM (safeRead . ncqReadStateKeys @m me) (fmap snd sfs)
                 <&> HS.fromList . mconcat

  kicked' <- ncqListDirFossils me <&> fmap fromString

  (kicked, used)  <- atomically do

      active <- ncqListTrackedFilesSTM me <&> HS.fromList . fmap  (view _1) . V.toList

      used'   <- readTVar ncqStateUsage <&> IntMap.elems

      let used = current
                 <> active
                 <> mentioned
                 <> HS.unions [ keys | (n, keys) <- used', n > 0 ]

      let k = filter (\x -> not (HS.member x used)) kicked'
      pure (k,HS.fromList $ HS.toList used)

  debug $ "KICK" <+> vcat (fmap pretty kicked)

  debug $ "LIVE SET" <+> vcat (fmap pretty (HS.toList used))

  for_ kicked $ \fo -> do
    debug $ "sweep fossil file" <+> pretty fo
    rm (ncqGetFileName me (toFileName (IndexFile fo)))
    rm (ncqGetFileName me (toFileName (DataFile fo)))

  where
    safeRead m = try @_ @IOException m >>= \case
      Right x -> pure x
      Left  e -> err ("ncqSweepFossils" <+> viaShow e) >> pure mempty

ncqSweepStates :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqSweepStates me@NCQStorage2{..} = withSem ncqSweepSem $ flip runContT pure do

  debug $ yellow "remove unused states"

  current' <- readTVarIO ncqStateName

  current <- ContT $ for_ current'

  debug $ yellow "CURRENT STATE NAME" <+> pretty current

  states <- ncqListStateFiles me <&> fmap snd

  flip fix (Left states) $ \next -> \case
    Left  []   -> none
    Right []   -> none
    Left  (x:xs) | x == current -> next (Right xs)
                 | otherwise    -> next (Left xs)

    Right (x:xs) -> do
      debug $ "Remove obsolete state" <+> pretty x
      rm (ncqGetFileName me (toFileName x))
      next (Right xs)

ncqSetOnRunWriteIdle :: MonadUnliftIO m => NCQStorage2 -> IO () -> m ()
ncqSetOnRunWriteIdle NCQStorage2{..} io = atomically (writeTVar ncqOnRunWriteIdle io)

writeFiltered :: forall m . MonadIO m
              => NCQStorage2
              -> FilePath
              -> Handle
              -> ( Integer -> Integer -> HashRef -> ByteString -> m Bool)
              -> m ()

writeFiltered ncq fn out filt = do
  ncqStorageScanDataFile ncq fn $ \o s k v -> do
    skip <- filt o s k v <&> not

    -- when skip do
    --   debug $ pretty k <+> pretty "skipped"

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


withSem :: forall a m . MonadUnliftIO m => TSem -> m a -> m a
withSem sem m = bracket enter leave (const m)
  where enter = atomically (waitTSem sem)
        leave = const $ atomically (signalTSem sem)

isNotPending :: Maybe CachedEntry -> Bool
isNotPending = \case
  Just (PendingEntry {}) -> False
  _ -> True


