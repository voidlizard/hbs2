{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ where

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

import Data.Config.Suckless.System
import Data.Config.Suckless.Script hiding (void)

import Control.Applicative
import Data.ByteString.Builder
import Network.ByteOrder qualified as N
import Data.HashMap.Strict (HashMap)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Ord (Down(..),comparing)
import Control.Concurrent.STM qualified as STM
import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Sequence as Seq
import Data.List qualified as List
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit)
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
                          , getFdStatus
                          , FileStatus(..)
                          , setFileMode
                          )
import System.Posix.Files qualified as PFS
import System.IO.Error (catchIOError)
import System.IO.MMap as MMap
import System.IO.Temp (emptyTempFile)
-- import Foreign.Ptr
-- import Foreign di
import qualified Data.ByteString.Internal as BSI
import Streaming.Prelude qualified as S

import UnliftIO
import UnliftIO.Concurrent(getNumCapabilities)
import UnliftIO.IO.File

{- HLINT ignore "Functor law" -}

type NCQPerks m = MonadIO m

data NCQStorageException =
    NCQStorageAlreadyExist String
  | NCQStorageSeedMissed
  | NCQStorageTimeout
  deriving stock (Show,Typeable)

instance Exception NCQStorageException


newtype FileKey = FileKey ByteString
                  deriving newtype (Eq,Ord,Hashable,Show)

instance IsString FileKey where
  fromString = FileKey . BS8.pack . dropExtension . takeFileName

instance Pretty FileKey where
  pretty (FileKey s) = parens ("file-key" <+> pretty (BS8.unpack s))

newtype FilePrio = FilePrio (Down TimeSpec)
                    deriving newtype (Eq,Ord)
                    deriving stock (Generic,Show)

mkFilePrio :: TimeSpec -> FilePrio
mkFilePrio = FilePrio . Down

data CachedEntry =
  CachedEntry { cachedMmapedIdx  :: ByteString
              , cachedMmapedData :: ByteString
              , cachedNway       :: NWayHash
              , cachedTs         :: TVar TimeSpec
              }

instance Show CachedEntry where
  show _ = "CachedEntry{...}"

data WQItem =
    WQItem { wqNew :: Bool
           , wqData :: Maybe LBS.ByteString
           }

data NCQStorage =
  NCQStorage
  { ncqRoot           :: FilePath
  , ncqGen            :: Int
  , ncqSyncSize       :: Int
  , ncqMinLog         :: Int
  , ncqMaxLog         :: Int
  , ncqMaxCached      :: Int
  , ncqSalt           :: HashRef
  , ncqWriteQueue     :: TVar (HashPSQ HashRef TimeSpec WQItem)
  , ncqWaitIndex      :: TVar (HashPSQ HashRef TimeSpec (Word64,Word64))
  , ncqIndexNow       :: TVar Int
  , ncqTrackedFiles   :: TVar (HashPSQ FileKey FilePrio (Maybe CachedEntry))
  , ncqCachedEntries  :: TVar Int
  , ncqNotWritten     :: TVar Word64
  , ncqLastWritten    :: TVar TimeSpec
  , ncqCurrentHandleW :: TVar Fd
  , ncqCurrentHandleR :: TVar Fd
  , ncqCurrentUsage   :: TVar (IntMap Int)
  , ncqCurrentReadReq :: TVar (Seq (Fd, Word64, Word64, TMVar ByteString))
  , ncqFlushNow       :: TVar [TQueue ()]
  , ncqOpenDone       :: TMVar Bool
  , ncqStopped        :: TVar Bool
  }


data Location =
    InWriteQueue WQItem
  | InCurrent    (Word64, Word64)
  | InFossil     CachedEntry (Word64, Word64)

instance Pretty Location where
  pretty = \case
    InWriteQueue{}   -> "write-queue"
    InCurrent (o,l)  -> pretty $ mkForm @C "current" [mkInt o, mkInt l]
    InFossil _ (o,l) -> pretty $ mkForm @C "fossil " [mkInt o, mkInt l]

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
          pure $ Just (fp, TimeSpec (floor ts) 0))
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

ncqWriteError :: MonadIO m => NCQStorage -> Text -> m ()
ncqWriteError ncq txt = liftIO do
  p <- getPOSIXTime <&> round @_ @Integer
  let msg = Text.pack $ show $ "error" <+> fill 12 (pretty p) <+> pretty txt <> line
  Text.appendFile (ncqGetErrorLogName ncq) msg

ncqIndexFile :: MonadUnliftIO m => NCQStorage -> FilePath -> m (FilePath, [HashRef])
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

  pure (fp, fmap (coerce @_ @HashRef . fst) items)

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
    -- writeJournal syncData
    readTVarIO ncqCurrentHandleW >>= closeFd

  debug "RUNNING STORAGE!"

  reader     <- makeReader
  writer     <- makeWriter indexQ
  indexer    <- makeIndexer writer indexQ

  mapM_ waitCatch [writer,indexer]
  -- mapM_ waitCatch [writer,indexer,refsWriter] -- ,indexer,refsWriter]
  mapM_ cancel  [reader]

  where

    untilStopped m = fix \loop -> do
        m >> readTVarIO ncqStopped >>= \case
          False -> loop
          _     -> debug "STOPPING THREAD"

    makeReader = do
      cap <- getNumCapabilities
      reader <- ContT $ withAsync $ untilStopped do

          debug "I'm READER THREAD"

          reqs <- atomically do
                    xs <- stateTVar ncqCurrentReadReq (Seq.splitAt cap)
                    when (List.null xs) STM.retry
                    pure xs


          for_ reqs $ \(fd,off,l,answ) -> liftIO do
            debug $ "READER: PROCEED REQUEST" <+> viaShow fd <+> pretty off
            atomically $ modifyTVar ncqCurrentUsage (IntMap.adjust pred (fromIntegral fd))
            fdSeek fd AbsoluteSeek (fromIntegral $ 4 + 32 + off)
            bs <- Posix.fdRead fd (fromIntegral l)
            atomically $ putTMVar answ bs

      link reader
      pure reader


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
            if not ( stop  || isJust q) then
              STM.retry
            else do
              STM.flushTQueue indexQ

         let what = fromRight mempty what'

         for_ what $ \(fd,fn) -> do

           debug $ "FUCKING WRITE INDEX" <+> pretty fn

           (key, _) <- ncqIndexFile ncq fn <&> over _2 HS.fromList

           ncqAddTrackedFilesIO ncq [key]
           atomically do
             modifyTVar ncqCurrentUsage (IntMap.adjust pred (fromIntegral fd))

           ncqLoadSomeIndexes ncq [fromString key]

         down <- atomically do
           writerDown <- pollSTM w <&> isJust
           stopped <- readTVar ncqStopped
           pure (stopped && writerDown)

         unless down next

      link indexer
      pure indexer

    writeJournal indexQ syncData = liftIO do

      trace $ "writeJournal" <+> pretty syncData

      fh <- readTVarIO ncqCurrentHandleW

      fdSeek fh SeekFromEnd 0

      initQ <- readTVarIO ncqWriteQueue

      wResult <- flip fix (0,initQ) \next (written,q) -> case HPSQ.minView q of
         Nothing ->  pure mempty
         Just (h,_,WQItem{..},rest) -> do

          off <- fdSeek fh SeekFromEnd 0

          -- we really have to write tomb prefix here
          let b = byteString (coerce @_ @ByteString h)
                   <> lazyByteString (fromMaybe (LBS.fromStrict ncqTombPrefix) wqData)

          let wbs = toLazyByteString b
          let len = LBS.length wbs
          let ws  = N.bytestring32  (fromIntegral len)
          let w = 4 + len

          if isNothing wqData && wqNew then
            pure ()
          else void do
            liftIO (Posix.fdWrite fh (ws <> LBS.toStrict wbs))

          written' <- if written < syncData then do
                        pure (written + w)
                      else do
                        fileSynchronise fh
                        pure 0

          ((h, (fromIntegral off, fromIntegral len)) : ) <$> next (written', rest)

      fileSynchronise fh
      size <- fdSeek fh SeekFromEnd 0

      now1 <- getTimeCoarse
      atomically do
        q0 <- readTVar ncqWriteQueue
        w0 <- readTVar ncqWaitIndex
        b0 <- readTVar ncqNotWritten

        wbytes <- newTVar 0

        (rq,rw) <- flip fix (q0,w0,wResult) \next (q,w,r) -> do
                     case r of
                       [] -> pure (q,w)
                       ((h,(o,l)):xs) -> do
                         modifyTVar wbytes (+l)
                         next (HPSQ.delete h q, HPSQ.insert h now1 (o,l) w,xs)

        writeTVar  ncqWriteQueue rq
        writeTVar  ncqWaitIndex rw
        bw <- readTVar wbytes
        writeTVar ncqNotWritten (max 0 (b0 - bw))

      writeBinaryFileDurable (ncqGetCurrentSizeName ncq) (N.bytestring64 (fromIntegral size))

      indexNow <- readTVarIO ncqIndexNow

      when (fromIntegral size >= ncqMinLog || indexNow > 0) do

        fsize <- readTVarIO ncqCurrentHandleR
                    >>= getFdStatus
                    <&> PFS.fileSize

        unless (fsize == 0) do

          (n,u) <- atomically do
                        r <- readTVar ncqCurrentHandleR <&> fromIntegral
                        u <- readTVar ncqCurrentUsage <&> fromMaybe 0 . IntMap.lookup r
                        pure (fromIntegral @_ @Word32 r, u)

          let current = ncqGetCurrentName ncq

          fossilized <- ncqGetNewFossilName ncq

          debug $ "NEED TRUNCATE" <+> pretty current <+> viaShow size <+> pretty n <+> pretty u

          mv current fossilized

          atomically do
            writeTVar ncqIndexNow 0
            r <- readTVar ncqCurrentHandleR
            -- NOTE: extra-use
            --   добавляем лишний 1 для индексации.
            --   исходный файл закрываем, только когда проиндексировано.
            --   то есть должны отнять 1 после индексации.
            modifyTVar ncqCurrentUsage (IntMap.insertWith (+) (fromIntegral r) 1)
            writeTQueue indexQ (r, fossilized)

          let flags = defaultFileFlags { exclusive = True }

          touch current
          writeBinaryFileDurable (ncqGetCurrentSizeName ncq) (N.bytestring64 0)

          liftIO (PosixBase.openFd current  Posix.ReadWrite flags)
              >>= atomically . writeTVar ncqCurrentHandleW

          liftIO (PosixBase.openFd current  Posix.ReadWrite flags)
              >>= atomically . writeTVar ncqCurrentHandleR

          debug $ "TRUNCATED, moved to" <+> pretty fossilized

          toClose <- atomically do
            w <- readTVar ncqCurrentUsage <&> IntMap.toList
            let (alive,dead) = List.partition( (>0) . snd) w
            writeTVar ncqCurrentUsage (IntMap.fromList alive)
            pure dead

          for_ toClose $ \(f,_) -> do
            when (f > 0) do
              debug $ "CLOSE FD" <+> pretty f
              Posix.closeFd (fromIntegral f)

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
    let wqi = WQItem True (Just lbs)
    modifyTVar ncqWriteQueue (HPSQ.insert h now wqi)
    modifyTVar ncqNotWritten (+ (fromIntegral $ 4 + 32 + LBS.length lbs))
    pure (Just h)

ncqStoragePutBlock :: MonadUnliftIO m => NCQStorage -> LBS.ByteString -> m (Maybe HashRef)
ncqStoragePutBlock ncq lbs = ncqStoragePut_ True ncq h (LBS.fromStrict ncqBlockPrefix <> lbs)
  where h = HashRef (hashObject lbs)

ncqIsTomb :: LBS.ByteString -> Bool
ncqIsTomb lbs = do
  let (pre,_) = LBS.splitAt (fromIntegral ncqPrefixLen) lbs
  LBS.isPrefixOf "T" pre
{-# INLINE ncqIsTomb #-}

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

ncqStorageGetBlock ncq h = runMaybeT do
  lbs <- lift (ncqStorageGet ncq h) >>= toMPlus
  guard (not $ ncqIsTomb lbs)
  pure $ LBS.drop (fromIntegral ncqPrefixLen) lbs

ncqPrefixLen :: Integer
ncqPrefixLen = 4

ncqRefPrefix :: ByteString
ncqRefPrefix = "R;;\x00"

ncqBlockPrefix :: ByteString
ncqBlockPrefix = "B;;\x00"

ncqTombPrefix :: ByteString
ncqTombPrefix = "T;;\x00"

ncqLocatedSize :: Location -> Integer
ncqLocatedSize = \case
  InWriteQueue WQItem{..} -> fromIntegral $ maybe 0 LBS.length wqData
  InCurrent (_,s)         -> fromIntegral s
  InFossil _ (_,s)        -> fromIntegral s

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
  l1 <- atomically do

    inQ <- readTVar ncqWriteQueue
            <&> (fmap snd . HPSQ.lookup h)
            <&> \case
                Just wq  -> Just (InWriteQueue wq)
                _        -> Nothing

    inC <- readTVar ncqWaitIndex  <&> (fmap snd . HPSQ.lookup h) <&> fmap InCurrent
    pure (inQ <|> inC)

  for_ l1 $ exit . Just

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

     when (BS.length bs < 4) $ exit ()

     let w = BS.take 4 bs & N.word32 & fromIntegral

     when (BS.length bs < 4 + w) $ exit ()

     let kv = BS.drop 4 bs

     let k = BS.take 32 kv & coerce @_ @HashRef
     let v = BS.take (w-32) $ BS.drop 32 kv

     lift (action o (fromIntegral w) k v)

     next (4 + o + fromIntegral w, BS.drop (w+4) bs)

ncqStorageGet :: MonadUnliftIO m => NCQStorage -> HashRef -> m (Maybe LBS.ByteString)
ncqStorageGet ncq h = runMaybeT do
  location <- ncqLocate ncq h >>= toMPlus
  lift (ncqStorageGet_ ncq location) >>= toMPlus

ncqStorageGet_ :: MonadUnliftIO m => NCQStorage -> Location -> m (Maybe LBS.ByteString)
ncqStorageGet_ NCQStorage{..} = \case
    InWriteQueue WQItem{ wqData = Just lbs } -> do
      pure $ Just lbs

    InCurrent (o,l) -> do
      r <- atomically do
        a <- newEmptyTMVar
        fd <- readTVar ncqCurrentHandleR
        modifyTVar ncqCurrentUsage (IntMap.insertWith (+) (fromIntegral fd) 1)
        modifyTVar ncqCurrentReadReq (|> (fd, o, l, a))
        pure a

      atomically (takeTMVar r) <&> Just . LBS.fromStrict

    InFossil ce (o,l) -> do
      now <- getTimeCoarse
      atomically $ writeTVar (cachedTs ce) now
      let chunk = BS.take (fromIntegral l) (BS.drop (fromIntegral o + 4 + 32) (cachedMmapedData ce))
      pure $ Just $ LBS.fromStrict chunk

    _ -> pure Nothing

{-# INLINE ncqStorageGet_ #-}

ncqRefHash :: NCQStorage -> HashRef -> HashRef
ncqRefHash NCQStorage{..} h = HashRef (hashObject (coerce @_ @ByteString h <> coerce ncqSalt))

ncqStorageGetRef :: MonadUnliftIO m => NCQStorage -> HashRef -> m (Maybe HashRef)
ncqStorageGetRef ncq ref = runMaybeT do
  lbs <- lift (ncqStorageGet ncq h) >>= toMPlus
  guard (not $ ncqIsTomb lbs)
  let hbs = LBS.toStrict (LBS.drop (fromIntegral ncqPrefixLen) lbs)
  guard (BS.length hbs == 32)
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
          modifyTVar ncqWriteQueue (HPSQ.insert h now wq)
          modifyTVar ncqNotWritten (+ fromIntegral (4 + 32 + ncqPrefixLen))

  ncqLocate ncq h >>= atomically . \case
    Just (InFossil _ _)   -> writeTombstone (WQItem False Nothing)
    Just (InCurrent  _)   -> do
      modifyTVar ncqWaitIndex (HPSQ.delete h)
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
      (newKey,_) <- ncqIndexFile ncq dataName
      ncqAddTrackedFilesIO ncq [newKey]


ncqStorageOpen :: MonadUnliftIO m => FilePath -> m NCQStorage
ncqStorageOpen fp' = do
  fp <- liftIO $ makeAbsolute fp'
  ncq@NCQStorage{..} <- ncqStorageInit_ False fp
  ncqReadTrackedFiles ncq
  ncqFixIndexes ncq
  ncqLoadIndexes ncq
  readCurrent ncq
  atomically $ putTMVar ncqOpenDone True
  pure ncq

  where

    readCurrent ncq@NCQStorage{..} = do
      let fn = ncqGetCurrentName ncq
      -- liftIO $ print $ pretty "FILE" <+> pretty fn
      bs0 <- liftIO $ mmapFileByteString fn Nothing

      now <- getTimeCoarse

      items <- S.toList_ <$>
        flip runContT pure $ callCC \exit ->do
          flip fix (0,bs0) $ \next (o,bs) -> do
            when (BS.length bs < 4) $ exit ()
            let w = BS.take 4 bs & N.word32 & fromIntegral
            let p = BS.take w (BS.drop 4 bs)

            when (BS.length p < w ) do
              err $ "broken file" <+> pretty fn
              exit ()

            let k  = BS.take 32 p & coerce . BS.copy
            let vs = w - 32

            lift $ S.yield (k,now, (fromIntegral o, fromIntegral vs))

            next (o+w+4, BS.drop (w+4) bs)

      atomically $ writeTVar ncqWaitIndex (HPSQ.fromList items)

ncqStorageInit :: MonadUnliftIO m => FilePath -> m NCQStorage
ncqStorageInit = ncqStorageInit_ True


ncqStorageInit_ :: MonadUnliftIO m => Bool -> FilePath -> m NCQStorage
ncqStorageInit_ check path = do

  let ncqGen = 0

  here <- doesPathExist path

  when (here && check) $ throwIO (NCQStorageAlreadyExist path)

  mkdir (path </> show ncqGen)

  let seedPath = path </> ".seed"

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

  let ncqSyncSize  = 64 * (1024 ^ 2)
  let ncqMinLog    =  2 * (1024 ^ 3)
  let ncqMaxLog    = 10 * (1024 ^ 3)

  let ncqMaxCached = 64

  ncqSalt <- try @_ @IOException (liftIO $ BS.readFile seedPath)
               >>= orThrow NCQStorageSeedMissed
               <&> HashRef . hashObject

  ncqWriteQueue    <- newTVarIO HPSQ.empty

  ncqNotWritten    <- newTVarIO 0
  ncqLastWritten   <- getTimeCoarse >>= newTVarIO
  ncqWaitIndex     <- newTVarIO HPSQ.empty

  ncqFlushNow       <- newTVarIO mempty
  ncqOpenDone       <- newEmptyTMVarIO
  ncqCurrentReadReq <- newTVarIO mempty
  ncqCurrentUsage   <- newTVarIO mempty
  ncqStopped        <- newTVarIO False
  ncqTrackedFiles   <- newTVarIO HPSQ.empty
  ncqCachedEntries  <- newTVarIO 0
  ncqIndexNow       <- newTVarIO 0

  let currentName = ncqGetCurrentName_ path ncqGen

  let currentSize = ncqGetCurrentSizeName_ path ncqGen

  hereCurrent <- doesPathExist currentName

  when hereCurrent $ liftIO do
    let ncqCurrentHandleW = undefined
    let ncqCurrentHandleR = undefined
    let ncq0 = NCQStorage{..}

    lastSz <- try @_ @IOException (BS.readFile currentSize)
               <&> either (const 0) N.word64

    currSz <- try @_ @IOException (fileSize currentName)
                <&> fromRight 0
                <&> fromIntegral

    when (lastSz /= currSz ) do
      fossilized <- ncqGetNewFossilName ncq0
      debug $ "NEW FOSSIL FILE" <+> pretty fossilized
      let fn = takeFileName fossilized
      let msg = fromString $ show $ "wrong-size" <+> pretty lastSz <+> pretty fn
      err $ pretty msg
      ncqWriteError ncq0 msg
      mv currentName fossilized

  touch currentName

  let flags = defaultFileFlags { exclusive = True }

  ncqCurrentHandleW <- liftIO (PosixBase.openFd currentName  Posix.ReadWrite flags)
                          >>= newTVarIO

  ncqCurrentHandleR  <- liftIO (PosixBase.openFd currentName Posix.ReadOnly flags)
                          >>= newTVarIO

  debug $ "currentFileName" <+> pretty (ncqGetCurrentName_ path ncqGen)

  let ncq = NCQStorage{..}

  pure ncq

ncqStorageFlush :: MonadUnliftIO m => NCQStorage -> m ()
ncqStorageFlush = ncqStorageSync

ncqIndexRightNow :: MonadUnliftIO m => NCQStorage -> m ()
ncqIndexRightNow NCQStorage{..} = atomically $ modifyTVar ncqIndexNow succ

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

