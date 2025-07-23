{-# Language MultiWayIf #-}
{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ
  ( module HBS2.Storage.NCQ
  , module HBS2.Storage.NCQ.Types
  ) where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Storage
import HBS2.Storage.NCQ.Types
import HBS2.Misc.PrettyStuff
import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.NCQ
import HBS2.Data.Log.Structured.SD

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
import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.IntSet qualified as IntSet
import Data.IntSet (IntSet)
import Data.Sequence as Seq
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

{- HLINT ignore "Functor law" -}

type NCQPerks m = MonadIO m




data WQItem =
    WQItem { wqNew :: Bool
           , wqData :: Maybe LBS.ByteString
           }

newtype RFd = RFd { unRfd :: Fd }

newtype WFd = WFd { unWfd :: Fd }

data NCQStorage =
  NCQStorage
  { ncqRoot            :: FilePath
  , ncqGen             :: Int
  , ncqQLen            :: Int
  , ncqSyncSize        :: Int
  , ncqMinLog          :: Int
  , ncqMaxSegments     :: Int
  , ncqMaxCached       :: Int
  , ncqCompactTreshold :: Int
  , ncqCapabilities    :: Int
  , ncqSalt            :: HashRef
  , ncqWriteQueue      :: TVar (HashPSQ HashRef TimeSpec WQItem)
  , ncqStaged          :: TVar (IntMap (HashPSQ HashRef TimeSpec (Word64,Word64)))
  , ncqIndexed         :: TVar IntSet
  , ncqIndexNow        :: TVar Int
  , ncqTrackedFiles    :: TVar (HashPSQ FileKey FilePrio (Maybe CachedEntry))
  , ncqCachedEntries   :: TVar Int
  , ncqNotWritten      :: TVar Word64
  , ncqLastWritten     :: TVar TimeSpec
  , ncqCurrentFd       :: TVar (Maybe (RFd,WFd))
  , ncqCurrentUsage    :: TVar (IntMap Int)
  , ncqCurrentReadReq  :: TVar (Seq (Fd, Word64, Word64, TMVar ByteString))
  , ncqLock            :: TVar FL.FileLock
  , ncqFsyncNum        :: TVar Int
  , ncqFlushNow        :: TVar [TQueue ()]
  , ncqMergeReq        :: TVar Int
  , ncqCompactReq      :: TVar Int
  , ncqCompactBusy     :: TMVar ()
  , ncqOpenDone        :: TMVar Bool
  , ncqStopped         :: TVar Bool
  }



instance MonadUnliftIO m => Storage NCQStorage HbSync LBS.ByteString  m where
    putBlock ncq lbs = fmap coerce <$> ncqStoragePutBlock ncq lbs
    enqueueBlock ncq lbs  = fmap coerce <$> ncqStoragePutBlock ncq lbs
    getBlock ncq h = ncqStorageGetBlock ncq (coerce h)

    hasBlock ncq = ncqStorageHasBlock ncq . coerce

    delBlock ncq = ncqStorageDel ncq . coerce

    updateRef ncq k v =  do
      ncqStorageSetRef ncq (HashRef $ hashObject k) (HashRef v)

    getRef ncq k =
      ncqStorageGetRef ncq (HashRef $ hashObject k) <&> fmap coerce

    delRef ncq k =
      ncqStorageDelRef ncq (HashRef $ hashObject k)

    getChunk ncq h off size = runMaybeT do
      block <- lift (ncqStorageGetBlock ncq (coerce h)) >>= toMPlus
      let chunk = LBS.take (fromIntegral size) $ LBS.drop (fromIntegral off) block
      pure chunk


data Location =
    InWriteQueue WQItem
  | InCurrent    (Fd,Word64, Word64)
  | InFossil     CachedEntry (Word64, Word64)

instance Pretty Location where
  pretty = \case
    InWriteQueue{}       -> "write-queue"
    InCurrent  (fd,o,l)  -> pretty $ mkForm @C "current" [mkInt fd, mkInt o, mkInt l]
    InFossil _ (o,l)     -> pretty $ mkForm @C "fossil " [mkInt o, mkInt l]

type IsHCQKey h  = ( Eq (Key h)
                   , Hashable (Key h)
                   , IsKey h
                   , Key h ~ Hash h
                   , ToByteString (AsBase58 (Hash h))
                   , FromByteString (AsBase58 (Hash h))
                   )

ncqGetCurrentName_ :: FilePath -> Int -> FilePath
ncqGetCurrentName_ root gen = root </> show (pretty gen) </> "current.data"

ncqGetFileName :: NCQStorage -> FilePath -> FilePath
ncqGetFileName NCQStorage{..} f = ncqRoot </> show (pretty ncqGen) </> takeFileName f

ncqGetCurrentName :: NCQStorage -> FilePath
ncqGetCurrentName NCQStorage{..} = ncqGetCurrentName_ ncqRoot ncqGen

ncqGetCurrentDir :: NCQStorage -> FilePath
ncqGetCurrentDir ncq = takeDirectory (ncqGetCurrentName ncq)

ncqGetCurrentSizeName_ :: FilePath -> Int -> FilePath
ncqGetCurrentSizeName_ root gen = dropExtension (ncqGetCurrentName_ root gen) <> ".size"

ncqGetCurrentSizeName :: NCQStorage ->  FilePath
ncqGetCurrentSizeName NCQStorage{..} = dropExtension (ncqGetCurrentName_ ncqRoot ncqGen) <> ".size"

ncqGetNewFossilName :: MonadIO m => NCQStorage -> m FilePath
ncqGetNewFossilName n@NCQStorage{} = do
  let fn = ncqGetFileName n "fossil-.data"
  let (p,tpl)  = splitFileName fn
  liftIO $ emptyTempFile p tpl

ncqGetNewMergeName :: MonadIO m => NCQStorage -> m FilePath
ncqGetNewMergeName n@NCQStorage{} = do
  let fn = ncqGetFileName n "merge-.data"
  let (p,tpl)  = splitFileName fn
  liftIO $ emptyTempFile p tpl


ncqGetNewCompactName :: MonadIO m => NCQStorage -> m FilePath
ncqGetNewCompactName n@NCQStorage{} = do
  let fn = ncqGetFileName n "compact-.data"
  let (p,tpl)  = splitFileName fn
  liftIO $ emptyTempFile p tpl

ncqGetIndexFileName :: NCQStorage -> FileKey -> FilePath
ncqGetIndexFileName ncq fk = do
  ncqGetFileName ncq (addExtension (dropExtension (BS8.unpack (coerce fk))) ".cq")

ncqGetDataFileName :: NCQStorage -> FileKey -> FilePath
ncqGetDataFileName ncq fk = do
  ncqGetFileName ncq (addExtension (dropExtension (BS8.unpack (coerce fk))) ".data")

ncqGetErrorLogName :: NCQStorage -> FilePath
ncqGetErrorLogName ncq = do
  ncqGetFileName ncq "errors.log"

ncqEmptyDataHash :: HashRef
ncqEmptyDataHash = HashRef $ hashObject @HbSync (mempty :: ByteString)

ncqAddCachedSTM :: TimeSpec                          -- ^ now
                -> Int                               -- ^ limit
                -> TVar (HashPSQ FileKey TimeSpec a) -- ^ entry
                -> FileKey                           -- ^ key
                -> a                                 -- ^ value
                -> STM ()
ncqAddCachedSTM now limit tv k v = do

  cache <- readTVar tv

  unless (HPSQ.member k cache) do

    let dst = if HPSQ.size cache + 1 > limit then
                maybe cache (view _4) (HPSQ.minView cache)
              else
                cache

    writeTVar tv (HPSQ.insert k now v dst)

ncqAddTrackedFilesIO :: MonadIO m => NCQStorage -> [FilePath] -> m ()
ncqAddTrackedFilesIO ncq fps = do
  tsFiles <- catMaybes <$> forM fps \fp' -> liftIO $ do
    catchIOError
      (do
          let fp = fromString fp'
          let dataFile = ncqGetDataFileName ncq fp
          stat <- getFileStatus dataFile
          let ts = modificationTimeHiRes stat
          pure $ Just (fp, posixToTimeSpec ts))
      (\e -> do
          err $ "ncqAddTrackedFilesIO: failed to stat " <+> viaShow e
          pure Nothing)

  atomically $ ncqAddTrackedFilesSTM ncq tsFiles


ncqAddTrackedFilesSTM :: NCQStorage -> [(FileKey, TimeSpec)] -> STM ()
ncqAddTrackedFilesSTM NCQStorage{..} keys = do
  old <- readTVar ncqTrackedFiles
  let new = flip fix (old, keys) \next -> \case
       (s, []) -> s
       (s, (k,ts):xs) -> next (HPSQ.insert k (mkFilePrio ts) Nothing s, xs)

  writeTVar ncqTrackedFiles new

ncqWaitForSlotSTM :: NCQStorage -> STM ()
ncqWaitForSlotSTM NCQStorage{..} = do
  s <- readTVar ncqWriteQueue <&> HPSQ.size
  when ( s >= ncqQLen ) STM.retry

ncqListTrackedFiles :: MonadIO m => NCQStorage -> m [FilePath]
ncqListTrackedFiles ncq = do
  let wd = ncqGetCurrentDir ncq
  dirFiles wd
     >>= mapM (pure . takeBaseName)
     <&> List.filter (List.isPrefixOf "fossil-")

ncqReadTrackedFiles :: MonadIO m => NCQStorage -> m ()
ncqReadTrackedFiles ncq@NCQStorage{} = do
  files <- ncqListTrackedFiles ncq
  ncqAddTrackedFilesIO ncq files

ncqWriteError :: (MonadIO m) => NCQStorage -> Doc AnsiStyle -> m ()
ncqWriteError ncq txt = liftIO do
  p <- getPOSIXTime <&> round @_ @Integer
  let msg = "error" <+> fill 12 (pretty p) <+> txt
  err msg
  let msgTxt = fromString $ show (msg <> line)
  Text.appendFile (ncqGetErrorLogName ncq) msgTxt

ncqIndexFile :: MonadUnliftIO m => NCQStorage -> FilePath -> m FilePath
ncqIndexFile n@NCQStorage{}  fp' = do

  let fp = ncqGetFileName n fp'
            & takeBaseName
            & (`addExtension` ".cq")
            & ncqGetFileName n

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

ncqFsync :: MonadUnliftIO m => NCQStorage -> Fd -> m ()
ncqFsync NCQStorage{..} fh = liftIO do
  fileSynchronise fh
  atomically $ modifyTVar ncqFsyncNum succ

ncqStorageStop :: MonadUnliftIO m => NCQStorage -> m ()
ncqStorageStop ncq@NCQStorage{..} = do
  debug "ncqStorageStop"
  ncqStorageSync ncq
  atomically $ writeTVar ncqStopped True
  atomically do
    done <- readTVar ncqWriteQueue <&> HPSQ.null
    unless done STM.retry
  debug "ncqStorageStop DONE"

ncqStorageRun :: MonadUnliftIO m => NCQStorage -> m ()
ncqStorageRun ncq@NCQStorage{..} = flip runContT pure do

  indexQ <- newTQueueIO

  ContT $ bracket none $ const $ liftIO do
    ncqFinalize ncq

  debug "RUNNING STORAGE!"

  reader       <- makeReader
  writer       <- makeWriter indexQ
  indexer      <- makeIndexer writer indexQ
  merge        <- makeMerge
  compact      <- makeCompact
  checkCompact <- makeCheckCompact
  checkMerge   <- makeCheckMerge
  flagWatcher  <- makeFlagWatcher
  sweep        <- makeSweep

  mapM_ waitCatch [writer,indexer,merge,compact]
  mapM_ cancel  [reader,flagWatcher,checkCompact,checkMerge,sweep]

  where

    untilStopped m = fix \loop -> do
        m >> readTVarIO ncqStopped >>= \case
          False -> loop
          _     -> debug "STOPPING THREAD"

    micropause :: forall a m . (IsTimeout a, MonadUnliftIO m) => Timeout a -> m ()
    micropause p = do
      void $ race @m (pause p) $
        atomically do
        s <- readTVar ncqStopped
        unless s STM.retry


    makeFlagWatcher = do
      let flags = ncqGetFileName ncq ".flags"
      let needIndexFlag = flags   </> "index:now"
      let needMergeFlag = flags   </> "merge:now"
      let needCompactFlag = flags </> "compact:now"

      ContT $ withAsync $ fix \again -> do
        pause @'Seconds 1
        needIndex <- doesPathExist needIndexFlag
        needMerge <- doesPathExist needMergeFlag
        needCompact <- doesPathExist needCompactFlag

        when needIndex do
          rm needIndexFlag
          ncqIndexRightNow ncq

        when needMerge do
          rm needMergeFlag
          ncqStorageMerge ncq

        when needCompact do
          rm needCompactFlag
          ncqStorageCompact ncq


        again

    makeSweep = do
      ContT $ withAsync $ liftIO $ fix \next -> do
        pause @'Seconds 10

        toClose <- atomically do
          usage   <- readTVar ncqCurrentUsage
          staged  <- readTVar ncqStaged
          indexed <- readTVar ncqIndexed

          let (alive, dead) = List.partition (\(_, u) -> u > 0) (IntMap.toList usage)

          let closable = do
                (f, _) <- dead
                guard (IntSet.member f indexed)
                guard (maybe True HPSQ.null (IntMap.lookup f staged))
                pure f

          writeTVar ncqCurrentUsage (IntMap.fromList alive)
          writeTVar ncqIndexed      (indexed  `IntSet.difference` IntSet.fromList closable)
          writeTVar ncqStaged       (foldr IntMap.delete staged closable)

          pure closable

        for_ toClose $ \f -> do
          debug $ "CLOSE FD" <+> pretty f
          closeFd (fromIntegral f)

        next

    makeReader = do
      cap <- getNumCapabilities
      reader <- ContT $ withAsync $ untilStopped do

          trace "I'm READER THREAD"

          reqs <- atomically do
                    xs <- stateTVar ncqCurrentReadReq (Seq.splitAt cap)
                    when (List.null xs) STM.retry
                    pure xs


          for_ reqs $ \(fd,off,l,answ) -> liftIO do
            -- FIXME: probe-requests-count
            trace $ "READER: PROCEED REQUEST" <+> viaShow fd <+> pretty off
            atomically $ modifyTVar ncqCurrentUsage (IntMap.adjust pred (fromIntegral fd))
            fdSeek fd AbsoluteSeek (fromIntegral $ 4 + 32 + off)
            bs <- Posix.fdRead fd (fromIntegral l)

            unless (BS.length bs == fromIntegral l) do
              err $ "READ MISMATCH" <+> pretty l <+> pretty (BS.length bs)

            atomically $ putTMVar answ bs

      link reader
      pure reader

    makeCheckCompact = do
      ContT $ withAsync $ untilStopped do
        pause @'Seconds 600
        debug "SCAN/CHECK FOR COMPACT"
        profit <- ncqLinearScanForCompact ncq (\_ _ -> none)
        -- FIXME: profit-hardcode
        when ( profit >= ncqCompactTreshold ) do
          atomically $ modifyTVar ncqCompactReq succ

    makeCompact = do

      me <- ContT $ withAsync $ untilStopped do

        req <- atomically do
                 stop <- readTVar ncqStopped
                 req  <- readTVar ncqCompactReq

                 if | stop      -> pure 0
                    | req  > 0  -> pure req
                    | otherwise -> STM.retry

        when (req > 0) do
          atomically $ writeTVar ncqCompactReq 0
          debug $ "STARTED COMPACT" <+> pretty req

          try @_ @SomeException (ncqCompact ncq) >>= \case
            Right{} -> none
            Left e -> do
              err ("COMPACT ERROR:" <+> viaShow e)
              pause @'Seconds 5


      link me
      pure me

    makeCheckMerge = do
      ContT $ withAsync $ untilStopped do
        pause @'Seconds 600
        debug "CHECK FOR MERGE"
        num <- readTVarIO ncqTrackedFiles  <&> HPSQ.size
        when (num > ncqMaxSegments) do
          atomically $ modifyTVar ncqMergeReq succ

    makeMerge = do
      me <- ContT $ withAsync $ untilStopped do
        micropause @'Seconds 10
        req <- readTVarIO ncqMergeReq

        when (req > 0) do
          debug $ "STARTED MERGE" <+> pretty req

          try @_ @SomeException (ncqStorageMergeStep ncq) >>= \case
            Right{} -> none
            Left e -> err ("MERGE ERROR:" <+> viaShow e)

          atomically $ writeTVar ncqMergeReq 0

      link me
      pure me

    makeWriter indexQ = do

      let dumpTimeout = TimeoutSec 10
      let dumpData    = fromIntegral ncqSyncSize
      let syncData    = fromIntegral ncqSyncSize

      writer <- ContT $ withAsync do

        myFlushQ <- newTQueueIO
        atomically $ modifyTVar ncqFlushNow (myFlushQ:)

        fix \next -> do

          liftIO $ race (pause dumpTimeout) $ atomically do
            flush <- isEmptyTQueue myFlushQ <&> not
            stop  <- readTVar ncqStopped
            bytes <- readTVar ncqNotWritten
            now   <- readTVar ncqIndexNow <&> (>0)
            if bytes > dumpData || flush || now || stop then none else STM.retry

          void $ atomically (STM.flushTQueue myFlushQ)

          liftIO $ writeJournal indexQ syncData

          done <- atomically $ readTVar ncqWriteQueue <&> HPSQ.null
          stopped <- readTVarIO ncqStopped

          if done && stopped then none else next

      link writer
      pure writer


    makeIndexer w indexQ = do
      indexer <- ContT $ withAsync $ fix \next ->  do

         what' <- race (pause @'Seconds 1) $ atomically do
            stop  <- readTVar ncqStopped
            q <- tryPeekTQueue indexQ
            if not (stop  || isJust q) then
              STM.retry
            else do
              STM.flushTQueue indexQ

         let what = fromRight mempty what'

         for_ what $ \(fd,fn) -> do

           debug $ "FUCKING WRITE INDEX" <+> pretty fn

           key <- ncqIndexFile ncq fn

           ncqAddTrackedFilesIO ncq [key]
           ncqLoadSomeIndexes ncq [fromString key]

           atomically do
             modifyTVar ncqCurrentUsage (IntMap.adjust pred (fromIntegral fd))
             modifyTVar ncqIndexed (IntSet.insert (fromIntegral fd))

         down <- atomically do
           writerDown <- pollSTM w <&> isJust
           stopped <- readTVar ncqStopped
           pure (stopped && writerDown)

         unless down next

      link indexer
      pure indexer

    writeJournal indexQ syncData = ncqWithCurrent ncq $ \(RFd fdr, WFd fh) -> liftIO do

      trace $ "writeJournal" <+> pretty syncData

      fdSeek fh SeekFromEnd 0

      initQ <- readTVarIO ncqWriteQueue

      wResult <- flip runContT pure $ callCC \exit -> do

        flip fix (0,initQ,mempty) \next (written,q,rq) -> do

         when (written >= syncData) $ exit rq

         -- when (HPSQ.null q) $ exit rq

         case HPSQ.minView q of
          Nothing -> pure rq
          Just (h,_,WQItem{..},rest) -> do

            let b = byteString (coerce @_ @ByteString h)
                     <> lazyByteString (fromMaybe (LBS.fromStrict ncqTombPrefix) wqData)

            let wbs = toLazyByteString b
            let len = LBS.length wbs
            let ws  = N.bytestring32  (fromIntegral len)
            let w = ncqSLen + len

            off <- liftIO $ fdSeek fh SeekFromEnd 0

            ww <- if isNothing wqData && wqNew then
                    pure 0
                  else do
                    liftIO (Posix.fdWrite fh (ws <> LBS.toStrict wbs))
                      <&> fromIntegral

            let item = (h, (fromIntegral off, fromIntegral len))
            next (written + ww, rest, item : rq )

      ncqFsync ncq fh
      size <- fdSeek fh SeekFromEnd 0
      writeBinaryFileDurableAtomic (ncqGetCurrentSizeName ncq) (N.bytestring64 (fromIntegral size))

      now1 <- getTimeCoarse
      atomically do
        q0 <- readTVar ncqWriteQueue
        w0 <- readTVar ncqStaged <&> fromMaybe HPSQ.empty . IntMap.lookup (fromIntegral fdr)
        b0 <- readTVar ncqNotWritten

        wbytes <- newTVar 0

        (rq,rw) <- flip fix (q0,w0,wResult) \next (q,w,r) -> do
                     case r of
                       [] -> pure (q,w)
                       ((h,(o,l)):xs) -> do
                         modifyTVar wbytes (+l)
                         let recLen = ncqFullDataLen (NCQFullRecordLen l)
                         next (HPSQ.delete h q, HPSQ.insert h now1 (o,recLen) w,xs)

        writeTVar  ncqWriteQueue rq
        modifyTVar ncqStaged (IntMap.insert (fromIntegral fdr) rw)
        bw <- readTVar wbytes
        writeTVar ncqNotWritten (max 0 (b0 - bw))

      indexNow <- readTVarIO ncqIndexNow

      when (fromIntegral size >= ncqMinLog || indexNow > 0) do

        fsize <- getFdStatus fdr <&> PFS.fileSize

        unless (fsize == 0) do

          (n,u) <- atomically do
                        let r = fromIntegral fdr
                        u <- readTVar ncqCurrentUsage <&> fromMaybe 0 . IntMap.lookup r
                        pure (fromIntegral @_ @Word32 r, u)

          let current = ncqGetCurrentName ncq

          fossilized <- ncqGetNewFossilName ncq

          debug $ "NEED TRUNCATE" <+> pretty current <+> viaShow size <+> pretty n <+> pretty u

          mv current fossilized

          atomically do
            -- NOTE: extra-use
            --   добавляем лишний 1 для индексации.
            --   исходный файл закрываем, только когда проиндексировано.
            --   то есть должны отнять 1 после индексации.
            modifyTVar ncqCurrentUsage (IntMap.insertWith (+) (fromIntegral fdr) 1)
            writeTQueue indexQ (fdr, fossilized)
            writeTVar ncqIndexNow 0

          closeFd fh
          writeBinaryFileDurable (ncqGetCurrentSizeName ncq) (N.bytestring64 0)
          ncqOpenCurrent ncq

          debug $ "TRUNCATED, moved to" <+> pretty fossilized

          -- ncqSweep
--
ncqStoragePut_ :: MonadUnliftIO m
               => Bool
               -> NCQStorage
               -> HashRef
               -> LBS.ByteString
               -> m (Maybe HashRef)

ncqStoragePut_ check ncq@NCQStorage{..} h lbs = flip runContT pure $ callCC \exit -> do

  when check do
    lift (ncqLocate  ncq h) >>= \case
      Nothing -> none
      Just loc -> do
        what <- lift $ ncqStorageGet_ ncq loc
        let tomb  = maybe True ncqIsTomb what -- continue if no record found || tomb
        unless tomb $ exit (Just h)

  now <- getTimeCoarse
  atomically do
    ncqWaitForSlotSTM ncq
    let wqi = WQItem True (Just lbs)
    modifyTVar ncqWriteQueue (HPSQ.insert h now wqi)
    modifyTVar ncqNotWritten (+ (fromIntegral $ 4 + 32 + LBS.length lbs))
    pure (Just h)

ncqStoragePutBlock :: MonadUnliftIO m => NCQStorage -> LBS.ByteString -> m (Maybe HashRef)
ncqStoragePutBlock ncq lbs = ncqStoragePut_ True ncq h (LBS.fromStrict ncqBlockPrefix <> lbs)
  where h = HashRef (hashObject lbs)

ncqIsTomb :: LBS.ByteString -> Bool
ncqIsTomb lbs = do
  let (pre,_) = LBS.splitAt ncqPrefixLen lbs
  LBS.isPrefixOf "T" pre
{-# INLINE ncqIsTomb #-}


data HasBlockError =
    LocationNotFound
  | DataNotRead
  | BlockIsTomb
  deriving stock (Eq,Show,Typeable)


instance Exception HasBlockError

ncqStorageHasBlockEither :: MonadUnliftIO m => NCQStorage -> HashRef -> m (Either HasBlockError Integer)
ncqStorageHasBlockEither ncq h = runExceptT do
  location <- ncqLocate ncq h >>= orThrow LocationNotFound
  let s  = ncqLocatedSize location
  if s > ncqPrefixLen then
    pure (s - ncqPrefixLen)
  else do
    what <- lift (ncqStorageGet_ ncq location) >>= orThrow DataNotRead
    when (ncqIsTomb what) $ throwIO BlockIsTomb
    pure 0

ncqStorageHasBlock :: MonadUnliftIO m => NCQStorage -> HashRef -> m (Maybe Integer)
ncqStorageHasBlock ncq h = runMaybeT do
  location <- ncqLocate ncq h >>= toMPlus
  let s  = ncqLocatedSize location
  if s > ncqPrefixLen then
    pure (s - ncqPrefixLen)
  else do
    what <- lift (ncqStorageGet_ ncq location) >>= toMPlus
    guard (not $ ncqIsTomb what)
    pure 0

ncqStorageGetBlock :: MonadUnliftIO m
                   => NCQStorage
                   -> HashRef
                   -> m (Maybe LBS.ByteString)

ncqStorageGetBlock ncq h = do
  ncqStorageGet ncq h >>= \case
    Just lbs | not (ncqIsTomb lbs) -> pure (Just $ LBS.drop ncqPrefixLen lbs)
    _ -> pure Nothing


ncqLocatedSize :: Location -> Integer
ncqLocatedSize = \case
  InWriteQueue WQItem{..} -> fromIntegral $ maybe 0 LBS.length wqData
  InCurrent (_,_,s)       -> fromIntegral s
  InFossil _ (_,s)        -> fromIntegral s

-- ncqFsync :: MonadUnliftIO m => NCQStorage{..} -> FilePath

evictIfNeededSTM :: NCQStorage -> Maybe Int -> STM ()
evictIfNeededSTM NCQStorage{..} howMany = do
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


ncqLocate :: MonadIO m => NCQStorage -> HashRef -> m (Maybe Location)
ncqLocate ncq@NCQStorage{..} h = flip runContT pure $ callCC \exit -> do

  inQ <- atomically $ readTVar ncqWriteQueue
          <&> (fmap snd . HPSQ.lookup h)
          <&> \case
              Just wq  -> Just (InWriteQueue wq)
              _        -> Nothing

  for_ inQ $ exit . Just

  inC <- atomically $ do
    s <- readTVar ncqStaged <&> IntMap.toList
    let found = lastMay $ catMaybes [ (fd,) <$> HPSQ.lookup h hpsq | (fd, hpsq)  <- s ]
    case found of
      Just (f, (_,(off,size))) -> pure (Just (InCurrent (fromIntegral f,off,size)))
      Nothing -> pure Nothing

  for_ inC $ exit . Just

  now <- getTimeCoarse
  tracked <- readTVarIO ncqTrackedFiles

  for_ (HPSQ.toList tracked) $ \(fk, prio, mCached) -> do
    case mCached of

      Just ce@CachedEntry{..} -> do
        lookupEntry h (cachedMmapedIdx, cachedNway) <&> fmap (InFossil ce) >>= \case
          Just loc -> do
            atomically $ writeTVar cachedTs now

            exit (Just loc)

          Nothing -> pure ()

      Nothing -> void $ runMaybeT do
        let indexFile = ncqGetIndexFileName ncq fk
        let dataFile  = ncqGetDataFileName  ncq fk

        (idxBs, idxNway) <- liftIO (nwayHashMMapReadOnly indexFile) >>= toMPlus
        datBs <- liftIO $ mmapFileByteString dataFile Nothing

        ce <- CachedEntry idxBs datBs idxNway <$> newTVarIO now
        e <- lookupEntry h (idxBs, idxNway) <&> fmap (InFossil ce) >>= toMPlus

        liftIO $ atomically do
          files <- readTVar ncqTrackedFiles
          case HPSQ.lookup fk files of
            Just (p, _) -> do
              modifyTVar ncqTrackedFiles (HPSQ.insert fk p (Just ce))
              modifyTVar ncqCachedEntries (+1)
              evictIfNeededSTM ncq (Just 1)
            Nothing -> pure ()

        lift (exit (Just e))

  pure Nothing

  where
    lookupEntry (hx :: HashRef) (mmaped, nway) = runMaybeT do
      entryBs <- liftIO (nwayHashLookup nway mmaped (coerce hx)) >>= toMPlus
      pure
        ( fromIntegral $ N.word64 (BS.take 8 entryBs)
        , fromIntegral $ N.word32 (BS.take 4 (BS.drop 8 entryBs)) )


ncqStorageScanDataFile :: MonadIO m
                       => NCQStorage
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

ncqStorageGet :: MonadUnliftIO m => NCQStorage -> HashRef -> m (Maybe LBS.ByteString)
ncqStorageGet ncq h = runMaybeT do
  location <- ncqLocate ncq h >>= toMPlus
  lift (ncqStorageGet_ ncq location) >>= toMPlus

ncqStorageGet_ :: MonadUnliftIO m => NCQStorage -> Location -> m (Maybe LBS.ByteString)
ncqStorageGet_ ncq@NCQStorage{..} = \case
    InWriteQueue WQItem{ wqData = Just lbs } -> do
      pure $ Just lbs

    InCurrent (fd,o,l) -> do
      r <- atomically do
        a <- newEmptyTMVar
        inRQ <- readTVar ncqCurrentReadReq <&> Seq.length
        when (inRQ > 1024 * ncqCapabilities) STM.retry
        modifyTVar ncqCurrentUsage (IntMap.insertWith (+) (fromIntegral fd) 1)
        modifyTVar ncqCurrentReadReq (|> (fd, o, l, a))
        pure a

      atomically (takeTMVar r) <&> Just . LBS.fromStrict

    InFossil ce (o,l) -> do
      now <- getTimeCoarse
      atomically $ writeTVar (cachedTs ce) now
      let chunk = BS.take (fromIntegral l) (BS.drop (ncqDataOffset o) (cachedMmapedData ce))
      pure $ Just $ LBS.fromStrict chunk

    _ -> pure Nothing

{-# INLINE ncqStorageGet_ #-}

ncqRefHash :: NCQStorage -> HashRef -> HashRef
ncqRefHash NCQStorage{..} h = HashRef (hashObject (coerce @_ @ByteString h <> coerce ncqSalt))

ncqStorageGetRef :: MonadUnliftIO m => NCQStorage -> HashRef -> m (Maybe HashRef)
ncqStorageGetRef ncq ref = runMaybeT do
  lbs <- lift (ncqStorageGet ncq h) >>= toMPlus
  guard (not $ ncqIsTomb lbs)
  let hbs = LBS.toStrict (LBS.drop ncqPrefixLen lbs)
  guard (BS.length hbs == ncqKeyLen)
  pure $ coerce hbs
  where h = ncqRefHash ncq ref

ncqStorageSetRef :: MonadUnliftIO m => NCQStorage -> HashRef -> HashRef -> m ()
ncqStorageSetRef ncq ref val = do
  current <- ncqStorageGetRef ncq ref
  unless (current == Just val) do
    void $ ncqStoragePut_ False ncq h (LBS.fromStrict $ ncqRefPrefix <> coerce val)
  where h = ncqRefHash ncq ref

ncqStorageDelRef :: MonadUnliftIO m => NCQStorage -> HashRef -> m ()
ncqStorageDelRef ncq ref = ncqStorageDel ncq h
  where h = ncqRefHash ncq ref

ncqStorageDel :: MonadUnliftIO m => NCQStorage -> HashRef -> m ()
ncqStorageDel ncq@NCQStorage{..} h = flip runContT pure $ callCC \exit -> do
  readTVarIO ncqStopped >>= \case
    True -> exit ()
    _    -> none

  now <- getTimeCoarse
  let writeTombstone wq = do
          ncqWaitForSlotSTM ncq
          let recordPrefixLen = ncqSLen + ncqKeyLen + ncqPrefixLen
          modifyTVar ncqWriteQueue (HPSQ.insert h now wq)
          modifyTVar ncqNotWritten (+ recordPrefixLen)

  ncqLocate ncq h >>= atomically . \case
    Just (InFossil _ _)   -> writeTombstone (WQItem False Nothing)

    Just (InCurrent  (fd,_,_))   -> do
      modifyTVar ncqStaged (IntMap.adjust (HPSQ.delete h) (fromIntegral fd))
      writeTombstone (WQItem False Nothing)

    Just (InWriteQueue _) -> writeTombstone (WQItem True Nothing)
    _ -> pure ()

ncqStorageSync :: MonadUnliftIO m => NCQStorage -> m ()
ncqStorageSync NCQStorage{..} = do
  atomically $ readTVar ncqFlushNow >>= mapM_ (`writeTQueue` ())


ncqLoadSomeIndexes :: MonadIO m => NCQStorage -> [FileKey] -> m ()
ncqLoadSomeIndexes ncq@NCQStorage{..} keys = do
  now <- getTimeCoarse

  ncqAddTrackedFilesIO  ncq (fmap (BS8.unpack . coerce) keys)

  loaded <- catMaybes <$> forM keys \key -> runMaybeT do
    mEntry <- liftIO $ readTVarIO ncqTrackedFiles <&> HPSQ.lookup key
    guard (maybe True (\(_, m) -> isNothing m) mEntry)

    let idxFile = ncqGetIndexFileName ncq key
    let datFile = ncqGetDataFileName  ncq key

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


ncqLoadIndexes :: MonadIO m => NCQStorage -> m ()
ncqLoadIndexes ncq@NCQStorage{..} = do
  debug "WIP: ncqStorageLoadIndexes"
  w <- readTVarIO ncqTrackedFiles
          <&> List.take (ncqMaxCached `div` 2) . HPSQ.keys
  ncqLoadSomeIndexes ncq w

ncqFixIndexes :: MonadUnliftIO m => NCQStorage -> m ()
ncqFixIndexes ncq@NCQStorage{..} = do
  debug "ncqFixIndexes"

  keys <- readTVarIO ncqTrackedFiles <&> HPSQ.keys

  for_ keys $ \k -> do
    let idxName = ncqGetIndexFileName ncq k
    here <- doesFileExist idxName

    unless here do
      warn $ "missed-index" <+> pretty k
      let dataName = ncqGetDataFileName ncq k
      newKey <- ncqIndexFile ncq dataName
      ncqAddTrackedFilesIO ncq [newKey]


ncqStorageOpen :: MonadUnliftIO m
               => FilePath
               -> m NCQStorage
ncqStorageOpen fp' = do
    flip fix 0 $ \next i -> do
      fp <- liftIO $ makeAbsolute fp'

      ncq@NCQStorage{..} <- ncqStorageInit_ False fp

      let flagz = ncqGetFileName ncq ".flags"

      mkdir flagz

      ncqReadTrackedFiles ncq
      ncqFixIndexes ncq
      ncqLoadIndexes ncq

      readCurrent ncq `catch`  \case
        NCQStorageBrokenCurrent  | i < 2 -> do
          let fn = ncqGetCurrentName ncq
          let msg = "broken file" <+> pretty (takeFileName fn)
          ncqWriteError ncq msg
          let (p,tpl)  = splitFileName (dropExtension fn `addExtension` ".broken")
          newFn <- liftIO $ emptyTempFile p tpl
          mv fn newFn
          rm (ncqGetCurrentSizeName ncq)
          void $ next (succ i)

        e -> throwIO e

      atomically $ putTMVar ncqOpenDone True
      pure ncq

  where

    readCurrent ncq@NCQStorage{..} = ncqWithCurrent ncq \(RFd fd, _) -> do
      let fn = ncqGetCurrentName ncq
      -- liftIO $ print $ pretty "FILE" <+> pretty fn
      bs0 <- liftIO $ mmapFileByteString fn Nothing

      now <- getTimeCoarse

      items <- S.toList_ <$>
        flip runContT pure $ callCC \exit ->do
          flip fix (0,bs0) $ \next (o,bs) -> do
            when (BS.length bs < ncqSLen) $ exit ()
            let w = BS.take ncqSLen bs & N.word32 & fromIntegral
            let p = BS.take w (BS.drop ncqSLen bs)

            when (BS.length p < w ) do
              throwIO NCQStorageBrokenCurrent

            let k  = BS.take ncqKeyLen p & coerce . BS.copy
            let vs = ncqFullDataLen (NCQFullRecordLen w)

            lift $ S.yield (k,now, (fromIntegral o, fromIntegral vs))

            next (o+w+ncqSLen, BS.drop (w+ncqSLen) bs)

      atomically $ modifyTVar ncqStaged (IntMap.insert (fromIntegral fd) (HPSQ.fromList items))

ncqStorageInit :: MonadUnliftIO m => FilePath -> m NCQStorage
ncqStorageInit = ncqStorageInit_ True

ncqOpenCurrent :: MonadUnliftIO m => NCQStorage -> m ()
ncqOpenCurrent ncq@NCQStorage{..} = do
  let fp = ncqGetCurrentName ncq
  touch fp
  let flags = defaultFileFlags { exclusive = False, creat = Just 0o666 }
  fdw <- liftIO (PosixBase.openFd fp  Posix.ReadWrite flags) <&> WFd
  fdr <- liftIO (PosixBase.openFd fp Posix.ReadOnly flags) <&> RFd
  atomically $ writeTVar ncqCurrentFd (Just (fdr, fdw))

ncqWithCurrent :: MonadUnliftIO m => NCQStorage -> ((RFd, WFd) -> m a) -> m a
ncqWithCurrent ncq@NCQStorage{..} action = do
  flip fix 2 $ \next i -> do
    readTVarIO ncqCurrentFd >>= \case

      Just a -> action a

      Nothing | i >= 0 -> do
        ncqOpenCurrent ncq
        next (pred i)

      Nothing -> do
        throwIO NCQStorageCantOpenCurrent

ncqStorageInit_ :: MonadUnliftIO m => Bool -> FilePath -> m NCQStorage
ncqStorageInit_ check path = do

  let ncqGen = 0

  let lockName = dropFileName (ncqGetCurrentName_ path ncqGen) </>  ".lock"

  here <- doesPathExist path

  when (here && check) $ throwIO (NCQStorageAlreadyExist path)

  mkdir (path </> show ncqGen)

  let seedPath = path </> ".seed"

  ncqLock_ <- liftIO do
    mkdir (takeDirectory lockName)
    l <- tryLockFile lockName Exclusive >>= orThrow (NCQStorageCantLock lockName)
    touch lockName
    pure l

  unless here do
    now <- liftIO $ getPOSIXTime <&> round @_ @Int

    let meta = [ mkForm @C "created" [ mkInt now ] ]
    let metas = show $ vsep (fmap pretty meta)

    liftIO $ appendFile (path </> "metadata") metas

    cred0 <- newCredentials @HBS2Basic
    cred <- addKeyPair Nothing cred0
    let seed = show $    "# storage seed file"  <+> pretty now <> line
                      <> "# NEVER EVER MODIFY OR REMOVE THIS FILE" <> line
                      <> "# or references may be lost and recovery will be prolematic" <> line
                      <> pretty (AsCredFile $ AsBase58 cred)

    liftIO do
      Prelude.writeFile seedPath seed
      PFS.setFileMode seedPath 0o0444

  let ncqRoot = path

  let ncqQLen            = 32000
  let ncqSyncSize        = 32   * (1024 ^ 2)
  let ncqMinLog          = 1024 * (1024 ^ 2)
  let ncqMaxSegments     = 16
  let ncqCompactTreshold = 128  * 1024^2

  let ncqMaxCached = 128

  ncqSalt <- try @_ @IOException (liftIO $ BS.readFile seedPath)
               >>= orThrow NCQStorageSeedMissed
               <&> HashRef . hashObject

  ncqWriteQueue    <- newTVarIO HPSQ.empty

  ncqNotWritten    <- newTVarIO 0
  ncqLastWritten   <- getTimeCoarse >>= newTVarIO
  ncqStaged        <- newTVarIO mempty

  ncqFlushNow       <- newTVarIO mempty
  ncqOpenDone       <- newEmptyTMVarIO
  ncqCurrentReadReq <- newTVarIO mempty
  ncqCurrentUsage   <- newTVarIO mempty
  ncqStopped        <- newTVarIO False
  ncqTrackedFiles   <- newTVarIO HPSQ.empty
  ncqCachedEntries  <- newTVarIO 0
  ncqIndexNow       <- newTVarIO 0
  ncqCurrentFd      <- newTVarIO Nothing
  ncqIndexed        <- newTVarIO mempty
  ncqMergeReq       <- newTVarIO 0
  ncqCompactReq     <- newTVarIO 0
  ncqCompactBusy    <- newTMVarIO ()
  ncqFsyncNum       <- newTVarIO 0
  ncqLock           <- newTVarIO ncqLock_
  ncqCapabilities   <- getNumCapabilities

  let currentName = ncqGetCurrentName_ path ncqGen

  let currentSize = ncqGetCurrentSizeName_ path ncqGen

  hereCurrent <- doesPathExist currentName

  when hereCurrent $ liftIO do
    let ncq0 = NCQStorage{..}

    lastSz <- try @_ @IOException (BS.readFile currentSize)
               <&> either (const 0) N.word64

    currSz <- try @_ @IOException (fileSize currentName)
                <&> fromRight 0
                <&> fromIntegral

    if | currSz > lastSz  -> do
            fossilized <- ncqGetNewFossilName ncq0
            debug $ "NEW FOSSIL FILE" <+> pretty fossilized
            let fn = takeFileName fossilized
            let msg = "wrong-size" <+> pretty lastSz <+> pretty fn
            ncqWriteError ncq0 msg
            mv currentName fossilized
            PFS.setFileSize fossilized (fromIntegral lastSz)
            rm currentSize

       | currSz < lastSz -> do
            err "current log is broken, removing, data loss"
            ncqWriteError ncq0 $ "current log is broken, removing, data loss"
            none

       | otherwise -> none

  debug $ "currentFileName" <+> pretty (ncqGetCurrentName_ path ncqGen)

  let ncq = NCQStorage{..}

  ncqOpenCurrent ncq

  pure ncq



ncqFsck :: MonadUnliftIO m => FilePath -> m [NCQFsckIssue]
ncqFsck fp = do
  isFile <- doesFileExist fp
  if isFile then
    ncqFsckOne fp
  else do
    fs <- dirFiles fp <&> List.filter ((== ".data") . takeExtension)
    concat <$> mapM ncqFsckOne fs

ncqFsckOne :: MonadUnliftIO m => FilePath -> m [NCQFsckIssue]
ncqFsckOne fp = do
  mmaped <- liftIO $ mmapFileByteString fp Nothing

  notice $ "file" <+> pretty (takeFileName fp) <+> pretty (BS.length mmaped)

  toff <- newTVarIO 0
  issuesQ <- newTQueueIO

  ttombs <- newTVarIO 0
  ttotal <- newTVarIO 0

  let
    emit :: forall m . MonadIO m => NCQFsckIssue -> m ()
    emit = atomically . writeTQueue issuesQ

  handle (\(_ :: ReadLogError) -> none) do
    runConsumeBS mmaped do
      readSections $ \size bs -> do
        let ssz  = LBS.length bs
        let (hash,   rest1) = LBS.splitAt 32 bs & over _1 (coerce . LBS.toStrict)
        let (prefix, rest2) = LBS.splitAt ncqPrefixLen  rest1 & over _1 LBS.toStrict

        let (prefixOk,pt) = if | prefix == ncqBlockPrefix -> (True, Just B)
                               | prefix == ncqRefPrefix   -> (True, Just R)
                               | prefix == ncqTombPrefix  -> (True, Just T)
                               | otherwise                -> (False, Nothing)

        atomically do
          when (prefix == ncqTombPrefix) $ modifyTVar ttombs succ
          modifyTVar ttotal succ

        let contentOk = case pt of
                Just B -> hash == hashObject @HbSync rest2
                _      -> True

        off <- readTVarIO toff

        unless prefixOk $ emit (NCQFsckIssue fp off FsckInvalidPrefix)

        unless contentOk $ emit (NCQFsckIssue fp off FsckInvalidContent)

        liftIO $ atomically $ modifyTVar toff (\x -> x + 4 + fromIntegral (LBS.length bs))

        debug $ pretty (takeFileName fp)
                   <+> pretty size
                   <+> pretty ssz
                   <+> brackets (pretty $ maybe "E" show pt)
                   <+> brackets (if contentOk then pretty hash else "invalid hash")

  lastOff <- readTVarIO toff

  unless (fromIntegral (BS.length mmaped) == lastOff) do
    emit (NCQFsckIssue fp lastOff (FsckInvalidFileSize (fromIntegral lastOff)))

  tombs <- readTVarIO ttombs <&> realToFrac
  total <- readTVarIO ttotal <&> realToFrac
  let ttr = if total /= 0 then tombs / total else 0 :: Fixed E3

  notice $ "tombs/total" <+> pretty ttr <+> pretty tombs <> "/" <> pretty total

  atomically $ STM.flushTQueue issuesQ


ncqStorageFlush :: MonadUnliftIO m => NCQStorage -> m ()
ncqStorageFlush = ncqStorageSync

ncqIndexRightNow :: MonadUnliftIO m => NCQStorage -> m ()
ncqIndexRightNow NCQStorage{..} = atomically $ modifyTVar ncqIndexNow succ

ncqFinalize :: MonadUnliftIO m => NCQStorage -> m ()
ncqFinalize NCQStorage{..} = do

  liftIO $ readTVarIO ncqStaged <&> IntMap.keys >>= mapM_ (closeFd . fromIntegral)
  atomically (writeTVar ncqStaged mempty)

  readTVarIO ncqCurrentFd >>= \case
    Just (RFd _, WFd wfd) -> do
      liftIO (closeFd wfd)
      atomically (writeTVar ncqCurrentFd Nothing)

    _ -> none

  liftIO $ unlockFile =<< readTVarIO ncqLock

withNCQ :: forall m a . MonadUnliftIO  m
        => (NCQStorage -> NCQStorage)
        -> FilePath
        -> (NCQStorage -> m a)
        -> m a
withNCQ setopts p action = flip runContT pure do
  ncq <- lift (ncqStorageOpen p) <&> setopts
  writer <- ContT $ withAsync (ncqStorageRun ncq)
  link writer
  e <- lift (action ncq)
  lift (ncqStorageStop ncq)
  wait writer
  pure e


writeFiltered :: forall m . MonadIO m
              => NCQStorage
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


ncqStorageMerge :: MonadUnliftIO m => NCQStorage -> m ()
ncqStorageMerge NCQStorage{..} = atomically $ modifyTVar ncqMergeReq succ

ncqStorageMergeStep :: MonadUnliftIO m => NCQStorage -> m ()
ncqStorageMergeStep ncq@NCQStorage{..}  = flip runContT pure do

  ContT $ bracket ( atomically (takeTMVar ncqCompactBusy) ) $ const do
    atomically $ putTMVar ncqCompactBusy ()

  liftIO do

    tracked <- readTVarIO ncqTrackedFiles <&> HPSQ.toList
    files <- for tracked $ \(f,p,_) -> do
      let fn = ncqGetDataFileName ncq  f
      sz <- liftIO (fileSize fn)
      pure (f, sz, p)

    let found = flip fix (files, Nothing, Nothing) $ \next -> \case
          ([], _, r) -> r
          (a:b:rest, Nothing, _) ->
            next (b:rest, Just (view _2 a + view _2 b), Just (a,b))

          (a:b:rest, Just s, _ ) | view _2 a + view _2 b < s ->
            next (b:rest, Just (view _2 a + view _2 b), Just (a,b))

          (_:rest, s, r) ->
            next (rest, s, r)

    case found of
      Just (a,b) -> mergeStep a b
      _          -> none

  where

    mergeStep (a,_,p1) (b,_,p2) = do
      warn $ "merge" <+> pretty a <+> pretty b

      let fDataNameA  = ncqGetDataFileName ncq a
      let fIndexNameA = ncqGetIndexFileName ncq a

      let fDataNameB  = ncqGetDataFileName ncq b
      let fIndexNameB = ncqGetIndexFileName ncq b

      warn $ "file A" <+> pretty (timeSpecFromFilePrio p1) <+> pretty fDataNameA <+> pretty fIndexNameA
      warn $ "file B" <+> pretty (timeSpecFromFilePrio p2) <+> pretty fDataNameB <+> pretty fIndexNameB

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


        liftIO do

          result <- fileSize mfile

          fp' <- if result == 0 then
                    pure Nothing
                   else do
                    fossil <- ncqGetNewFossilName ncq
                    mv mfile fossil
                    statA <- getFileStatus fDataNameA
                    let ts = modificationTimeHiRes statA
                    setFileTimesHiRes fossil ts ts
                    Just . (ts,) . fromString <$> ncqIndexFile ncq fossil

          atomically do
            modifyTVar ncqTrackedFiles (HPSQ.delete a)
            modifyTVar ncqTrackedFiles (HPSQ.delete b)
            for_ fp' $ \(ts,fp) -> do
              ncqAddTrackedFilesSTM ncq [(fp, posixToTimeSpec ts)]

          mapM_ rm [fDataNameA, fDataNameB, fIndexNameB, fIndexNameA]


    orFail what e = do
      r <- what
      unless r (throwIO (NCQMergeInvariantFailed (show e)))



-- NOTE: incremental
--   now it may became incremental if we'll
--   limit amount of tombs per one pass
--   then remove all dead entries,
--   then call again to remove tombs. etc
--   as for now, seems it should work up to 10TB
--   of storage
ncqLinearScanForCompact :: MonadUnliftIO m
                        => NCQStorage
                        -> ( FileKey -> HashRef -> m () )
                        -> m Int
ncqLinearScanForCompact ncq@NCQStorage{..} action = flip runContT pure do

  ContT $ bracket ( atomically (takeTMVar ncqCompactBusy) ) $ const do
    atomically $ putTMVar ncqCompactBusy ()

  tracked <- readTVarIO ncqTrackedFiles <&> HPSQ.toList

  let state0 = mempty :: HashMap HashRef TimeSpec

  profit <- newTVarIO 0
  tombUse   <- newTVarIO (mempty :: HashMap HashRef (FileKey, Int))

  -- TODO: explicit-unmap-files

  flip fix  (tracked, state0) $ \next -> \case
    ([], s) -> none
    ((fk,p,_):rest, state) -> do

      let cqFile = ncqGetIndexFileName ncq fk
      let dataFile = ncqGetDataFileName ncq fk

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

ncqStorageCompact :: MonadUnliftIO m => NCQStorage -> m ()
ncqStorageCompact NCQStorage{..} = do
  atomically $ modifyTVar ncqCompactReq succ

ncqCompact :: MonadUnliftIO m => NCQStorage -> m ()
ncqCompact ncq@NCQStorage{..} = do

  q <- newTVarIO ( mempty :: HashMap FileKey (HashSet HashRef) )

  ncqLinearScanForCompact ncq $ \fk h -> atomically do
    modifyTVar q (HM.insertWith (<>) fk (HS.singleton h))

  state0 <- readTVarIO q

  for_ (HM.toList state0) $ \(fk, es) -> do
    trace $ "TO DELETE" <+> pretty fk <+> pretty (HS.size es)

    let fDataNameA = ncqGetDataFileName ncq fk
    let fIndexNameA = ncqGetIndexFileName ncq fk

    flip runContT pure do

      mfile <- ncqGetNewCompactName ncq

      ContT $ bracket none $ const do
        rm mfile

      liftIO do
        withBinaryFileAtomic mfile WriteMode $ \fwh -> do
            writeFiltered ncq fDataNameA fwh $ \_ _ k v -> do
              pure $ not $ HS.member k es

        result <- fileSize mfile

        if result == 0 then do
          atomically $ modifyTVar ncqTrackedFiles (HPSQ.delete fk)
        else do

          fossil <- ncqGetNewFossilName ncq
          mv mfile fossil

          statA <- getFileStatus fDataNameA

          let ts = modificationTimeHiRes statA
          setFileTimesHiRes fossil ts ts

          fname <- ncqIndexFile ncq fossil

          atomically do
            let fp = fromString fname
            modifyTVar ncqTrackedFiles (HPSQ.delete fk)
            ncqAddTrackedFilesSTM ncq [(fp, posixToTimeSpec ts)]

        mapM_ rm [fDataNameA, fIndexNameA]

  debug $ "compact done" <+> pretty (HM.size state0)


