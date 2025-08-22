{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ3.Internal.Fossil where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.State

import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Control.Monad.Trans.Cont
import Network.ByteOrder qualified as N
import Data.ByteString.Builder
import System.IO.Temp (emptyTempFile)

import System.Posix.Files qualified as Posix
import System.Posix.IO as PosixBase
import System.Posix.Types as Posix
import System.Posix.IO.ByteString as Posix
import System.Posix.Files qualified as PFS
import Lens.Micro.Platform
import UnliftIO.IO.File

{-HLINT ignore "Functor law"-}

ncqEntryUnwrap :: ByteString
               -> (ByteString, Either ByteString (NCQSectionType, ByteString))
ncqEntryUnwrap source = do
  let (k,v) = BS.splitAt ncqKeyLen (BS.drop 4 source)
  (k, ncqEntryUnwrapValue v)
{-# INLINE ncqEntryUnwrap #-}

ncqEntryUnwrapValue :: ByteString
                    -> Either ByteString (NCQSectionType, ByteString)
ncqEntryUnwrapValue  v = case ncqIsMeta v of
  Just meta -> Right (meta, BS.drop ncqPrefixLen v)
  Nothing   -> Left v
{-# INLINE ncqEntryUnwrapValue #-}

-- FIXME: wrong-algoritm
--
--   контр-пример:
--     индексируем два файла с глобальным индексом, одновременно
--     (но после пробега) значение меняется в памяти и пишется индекс
--     а потом мы пишем свой индекс -- и таким образом, менее актуальное
--     значение всплывает наверх. гонка.
--     При один файл = один индекс порядок был всегда однозначен.
--     теперь же в один индекс попадают значения из разных файлов.
--     а мы какой возьмем?
--     возможно, кстати, timestamp(index) == max(timestamp(idx(a)), timestamp(idx(b)))
--     так как мы: пишем в merged файл значения, отсутствующие в индексе (и памяти -- как нам
--     кажется /т.к гонка/)
--     единственное, что нам нужно -- что бы этот индекс
--     получил таймстемп меньше, чем возможно актуальное значение. вопрос,
--     как этого добиться
--
ncqFossilMergeStep :: forall m . MonadUnliftIO m
               => NCQStorage
               -> m Bool

ncqFossilMergeStep me@NCQStorage{..}  = flip runContT pure $ callCC \exit -> do
  tmax <- liftIO getPOSIXTime >>= newTVarIO

  debug "ncqFossilMergeStep"

  -- TODO: consider-sort-by-timestamps
  files <- readTVarIO ncqState
           <&> fmap DataFile .  HS.toList . ncqStateFiles
           <&> List.sortOn Down

  NCQState{..}  <- readTVarIO ncqState

  let tss = ncqStateIndex & fmap (\(Down x, y) -> (y, realToFrac x :: POSIXTime)) & HM.fromList

  cur <- readTVarIO ncqCurrentFossils
  r' <- lift $ ncqFindMinPairOfBy me (\x -> not (HS.member (coerce x) cur)) files

  r@(sumSize, f1, f2) <- ContT $ maybe1 r' (pure False)

  debug $ yellow "for compacting" <+> pretty f1 <+> pretty f2  <+> pretty r <+> pretty ncqMaxLog

  when (fromIntegral sumSize > ncqMaxLog) $ exit False

  let (p,tpl) = splitFileName (ncqGetFileName me "merge-.merge")

  outFile <- liftIO $ emptyTempFile p tpl

  ContT $ bracket none $ const do
          removeFile outFile


  liftIO $ withBinaryFileAtomic outFile WriteMode $ \fwh -> do
    fd <- handleToFd fwh

    already <- newTVarIO (mempty :: HashSet HashRef )

    for_ [f1, f2] $ \fi -> do
      let fik = coerce fi
      writeFiltered me (ncqGetFileName me fi) fd $ \o _ k _ -> do
        ncqLocate_ False me k >>= \case
          Nothing  -> pure False
          Just (InMemory{}) -> pure False
          Just (InFossil (FileLocation fk oi si)) -> do
            let beWritten = fk == fik && o == fromIntegral oi

            -- let c = if skip then green else id
            -- when (si == ncqTombEntrySize) do
            --   debug $ red "fucking TOMB found!"
            --               <+> pretty k
            --               <+> viaShow (fk, oi, fik, o)
            --               <+> "write" <+> c (pretty beWritten)

            atomically do
              tj <- readTVar tmax
              modifyTVar tmax (max (fromMaybe tj (HM.lookup fk tss)))
              here <- readTVar already <&> HS.member k
              let proceed = not here && beWritten
              when proceed $ modifyTVar already (HS.insert k)
              pure proceed

    appendTailSection fd

  f3 <- DataFile <$> ncqGetNewFileKey me DataFile

  let newFile = ncqGetFileName me f3

  debug $ "MOVED" <+> pretty outFile <+> pretty newFile
  moveFile outFile newFile

  ss <- liftIO (PFS.getFileStatus newFile) <&> fromIntegral . PFS.fileSize

  ncqStateUpdate me do
    ncqStateAddFact (P (PData f3 ss))

  ts <- readTVarIO tmax
  lift $ ncqIndexFile me (Just ts) f3

  ncqStateUpdate me do
    ncqStateDelDataFile (coerce f1)
    ncqStateDelDataFile (coerce f2)

  debug $ "COMPACTED" <+> pretty f1 <+> pretty f2  <+> "=>" <+> pretty f3

  pure True

ncqFileFastCheck :: MonadUnliftIO m => FilePath -> m ()
ncqFileFastCheck fp = do

  -- debug $ "ncqFileFastCheck" <+> pretty fp

  mmaped <- liftIO $ logErr "ncqFileFastCheck" ( mmapFileByteString fp Nothing)
  let size = BS.length mmaped
  let s = BS.drop (size - 8) mmaped & N.word64

  unless ( BS.length mmaped == fromIntegral s ) do
    throwIO $ NCQFsckIssueExt (FsckInvalidFileSize (fromIntegral s))

ncqFileTryRecover :: MonadUnliftIO m => FilePath -> m NCQOffset
ncqFileTryRecover fp = do

  debug $ yellow  "ncqFileTryRecover" <+> pretty fp

  mmaped <- liftIO $ logErr "ncqFileTryRecover" (mmapFileByteString fp Nothing)

  r <- flip runContT pure $ callCC \exit -> do

    flip fix (0,0,mmaped) $ \next (o,r,bs) -> do

      when (BS.length bs < ncqSLen) $ exit r

      let (s0,rest)  = BS.splitAt  ncqSLen bs & over _1 (fromIntegral . N.word32)

      when (BS.length rest < fromIntegral s0 || BS.length rest < ncqKeyLen) $ exit r

      let (entry, rest2) = BS.splitAt (ncqSLen + s0) bs

      let nextOff = o + ncqSLen + s0

      case ncqEntryUnwrap entry of
        (_, Left bs)      -> next (nextOff,r,mempty)

        (k, Right (M, s)) -> do
          let w0 = N.word64 s
          let w1 = w0 - zeroSyncEntrySize
          let hk  = coerce @_ @HashRef k
          let hhs = HashRef $ hashObject @HbSync s

          let thisIsHead = nextOff == fromIntegral w0 && hk == hhs

          -- debug $ yellow "HEAD?" <+> pretty thisIsHead
          --                        <+> pretty nextOff <+> pretty hhs

          if thisIsHead then
            next (nextOff, nextOff, rest2)
          else
            next (nextOff, r, mempty)

        (_, Right (t, _)) -> next (nextOff, r, rest2)

  pure $ fromIntegral r


writeFiltered :: forall m . MonadIO m
              => NCQStorage
              -> FilePath
              -> Fd
              -> ( Integer -> Integer -> HashRef -> ByteString -> m Bool)
              -> m ()

writeFiltered ncq fn out filt = do
  ncqStorageScanDataFile ncq fn $ \o s k v -> do
    skip <- filt o s k v <&> not

    -- when skip do
    --   debug $ pretty k <+> pretty "skipped"

    unless skip $ liftIO do
      void $ appendSection out (LBS.toStrict (makeEntryLBS k v))

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

appendTailSection :: MonadIO m => Fd -> m NCQFileSize
appendTailSection fh = liftIO do
  s <- Posix.fileSize <$> Posix.getFdStatus fh
  appendSection fh (fileTailRecord s) <&> (+ fromIntegral s) . fromIntegral
{-# INLINE appendTailSection #-}



