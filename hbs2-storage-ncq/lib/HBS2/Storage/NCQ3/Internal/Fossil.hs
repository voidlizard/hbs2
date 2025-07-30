module HBS2.Storage.NCQ3.Internal.Fossil where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.State

import HBS2.Data.Types.Refs

import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Control.Monad.Trans.Cont
import Network.ByteOrder qualified as N
import Data.ByteString.Builder
import System.IO.Temp (emptyTempFile)

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

import UnliftIO.IO.File

{-HLINT ignore "Functor law"-}

ncqFossilMergeStep :: forall m . MonadUnliftIO m
               => NCQStorage3
               -> m Bool

ncqFossilMergeStep me@NCQStorage3{..}  = withSem ncqServiceSem $ flip runContT pure $ callCC \exit -> do

  debug "ncqFossilMergeStep"

  -- TODO: consider-sort-by-timestamps
  files <- readTVarIO ncqState
           <&> fmap DataFile .  HS.toList . ncqStateFiles
           <&> List.sortOn Down

  r' <- lift $ ncqFindMinPairOf me files

  r@(sumSize, f1, f2) <- ContT $ maybe1 r' (pure False)

  debug $ "for compacting" <+> pretty f1 <+> pretty f2  <+> pretty r <+> pretty ncqMaxLog

  when (fromIntegral sumSize > ncqMaxLog) $ exit False

  let (p,tpl) = splitFileName (ncqGetFileName me "merge-.merge")

  outFile <- liftIO $ emptyTempFile p tpl

  ContT $ bracket none $ const do
          rm outFile

  liftIO $ withBinaryFileAtomic outFile WriteMode $ \fwh -> do
    fd <- handleToFd fwh

    already <- newTVarIO (mempty :: HashSet HashRef )

    for_ [f1, f2] $ \fi -> do
      let fik = coerce fi
      writeFiltered me (ncqGetFileName me fi) fd $ \_ _ k _ -> do
        ncqLocate_ False me k >>= \case
          Nothing  -> pure True
          Just (InMemory{}) -> pure False
          Just (InFossil fk _ _) -> do
            let beWritten = fik >= fk
            atomically do
              here <- readTVar already <&> HS.member k
              let proceed = not here && beWritten
              when proceed (modifyTVar already (HS.insert k))
              pure proceed

    appendTailSection fd

  f3 <- DataFile <$> ncqGetNewFileKey me DataFile

  let newFile = ncqGetFileName me f3

  mv outFile newFile

  ss <- liftIO (PFS.getFileStatus newFile) <&> fromIntegral . PFS.fileSize

  ncqStateUpdate me do
    ncqStateAddFact (P (PData f3 ss))

  lift $ ncqIndexFile me f3

  ncqStateUpdate me do
    ncqStateDelDataFile (coerce f1)
    ncqStateDelDataFile (coerce f2)

  debug $ "COMPACTED" <+> pretty f1 <+> pretty f2  <+> "=>" <+> pretty f3

  pure True


writeFiltered :: forall m . MonadIO m
              => NCQStorage3
              -> FilePath
              -> Fd
              -> ( Integer -> Integer -> HashRef -> ByteString -> m Bool)
              -> m ()

writeFiltered ncq fn out filt = do
  ncqStorageScanDataFile ncq fn $ \o s k v -> do
    skip <- filt o s k v <&> not

    when skip do
      debug $ pretty k <+> pretty "skipped"

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

appendTailSection :: MonadIO m => Fd -> m ()
appendTailSection fh = liftIO do
  s <- Posix.fileSize <$> Posix.getFdStatus fh
  void (appendSection fh (fileTailRecord s))
{-# INLINE appendTailSection #-}



