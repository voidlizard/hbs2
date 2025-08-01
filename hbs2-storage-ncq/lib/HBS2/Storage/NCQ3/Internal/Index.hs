module HBS2.Storage.NCQ3.Internal.Index where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Memtable
import HBS2.Storage.NCQ3.Internal.Files

import System.Posix.Files qualified as PFS
import Streaming.Prelude qualified as S
import Network.ByteOrder qualified as N
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS
import System.IO.MMap
import System.IO.Temp as Temp
import Streaming.Prelude qualified as S


-- we need size in order to return block size faster
-- w/o search in fossil
data IndexEntry = IndexEntry {-# UNPACK #-} !FileKey !NCQOffset !NCQSize
                  deriving stock (Eq,Show)

ncqIndexEntryPadding :: Int
ncqIndexEntryPadding = 0

ncqIndexPayloadSize :: Int
ncqIndexPayloadSize = fileKey + fileOffset + blockSize + padding
  where
    fileKey    = 8
    fileOffset = 8
    blockSize  = 4
    padding    = ncqIndexEntryPadding
{-# INLINE ncqIndexPayloadSize #-}

unpackIndexEntry :: ByteString -> IndexEntry
unpackIndexEntry  entryBs = do
    let (fks,rest1)  = BS.splitAt 8 entryBs -- FileKey: 8
    let (offs,rest2) = BS.splitAt 8 rest1   -- Offset:  8
    let ss           = BS.take    4 rest2   -- Size:    4
                                            -- padding: 0?
    let fk   = FileKey (N.word64 fks)
    let off  = N.word64 offs
    let size = N.word32 ss
    IndexEntry fk off size
{-# INLINE unpackIndexEntry #-}

packIndexEntryPayload :: IndexEntry -> ByteString
packIndexEntryPayload (IndexEntry fk offset blockSize) = do
    let fks = N.bytestring64 (coerce fk)
    let rs = (blockSize + ncqSLen) & fromIntegral @_ @Word32 & N.bytestring32
    let os = fromIntegral @_ @Word64 offset & N.bytestring64
    let padding = BS.replicate ncqIndexEntryPadding 0
    let record = fks <> os <> rs <>  padding
    record

emptyKey :: ByteString
emptyKey = BS.replicate 32 0

-- FIXME: better-hashtable-params
ncqIndexAlloc :: NWayHashAlloc
ncqIndexAlloc = nwayAllocDef 1.15 32 8 ncqIndexPayloadSize

ncqLookupIndex :: MonadUnliftIO m
               => HashRef
               -> (ByteString, NWayHash)
               -> m (Maybe IndexEntry )
ncqLookupIndex hx (mmaped, nway) = do
  fmap unpackIndexEntry <$> nwayHashLookup nway mmaped (coerce hx)
{-# INLINE ncqLookupIndex #-}



ncqLocate_ :: MonadUnliftIO m => Bool -> NCQStorage -> HashRef -> m (Maybe Location)
ncqLocate_ f me@NCQStorage{..} href = ncqOperation me (pure Nothing) do
  answ <- newEmptyTMVarIO

  atomically do
    when f $ modifyTVar ncqWrites succ
    writeTQueue ncqReadReq (href, answ)

  atomically $ takeTMVar answ

ncqLocate :: MonadUnliftIO m => NCQStorage -> HashRef -> m (Maybe Location)
ncqLocate me href = ncqOperation me (pure Nothing) do
  ncqLocate_ True me href

ncqIndexFile :: MonadUnliftIO m => NCQStorage -> DataFile FileKey -> m (Maybe FilePath)
ncqIndexFile n fk = runMaybeT do

  let fp   = toFileName fk & ncqGetFileName n
  fki <- ncqGetNewFileKey n IndexFile

  let dest  = ncqGetFileName n (IndexFile fki)

  debug $ "INDEX" <+> pretty fp <+> pretty dest

  items <- S.toList_ do
    ncqStorageScanDataFile n fp $ \offset w key s -> case ncqIsMeta s of
      Just M -> none
      _ -> do
        let entry = IndexEntry (coerce fk) (fromIntegral offset) (fromIntegral w)
        let record = packIndexEntryPayload entry
        S.yield (coerce key, record)

  let (dir,name) = splitFileName fp
  let idxTemp = (dropExtension name <> "-") `addExtension` ".cq$"

  result <- lift $ nwayWriteBatch ncqIndexAlloc dir idxTemp items

  mv result dest

  stat <- liftIO $ PFS.getFileStatus dest
  let ts = PFS.modificationTimeHiRes stat

  midx <- liftIO (nwayHashMMapReadOnly dest)

  unless (isJust  midx) do
    err $ "can't mmap index" <+> pretty dest

  ncqStateUpdate n do
    ncqStateAddIndexFile ts fki
    ncqStateAddDataFile (coerce fk)
    ncqStateDelFact (P (PData fk 0))

  (bs,nw) <- toMPlus midx

  nwayHashScanAll nw bs $ \_ k _ -> do
    unless (k == emptyKey) $ atomically $ void $ runMaybeT do
      NCQEntry _ tfk <- MaybeT $ ncqLookupEntrySTM n (coerce k)
      fk' <- MaybeT $ readTVar tfk
      guard (coerce fk == fk') -- remove only own stuff
      lift $ ncqAlterEntrySTM n (coerce k) (const Nothing)

  pure dest

{-HLINT ignore "Functor law"-}


ncqIndexCompactFull :: MonadUnliftIO m
                    => NCQStorage
                    -> m ()

ncqIndexCompactFull ncq = fix \again ->
  ncqIndexCompactStep ncq >>= \case
    True -> again
    False -> none

ncqIndexCompactStep :: MonadUnliftIO m
                    => NCQStorage
                    -> m Bool
ncqIndexCompactStep me@NCQStorage{..} = withSem ncqServiceSem $ flip runContT pure $ callCC \exit -> do

  debug "ncqIndexCompactStep"

  idx <- readTVarIO ncqState
           <&> fmap (IndexFile . snd) . ncqStateIndex

  r' <- lift $ ncqFindMinPairOf me idx

  (_, a, b) <- ContT $ maybe1 r' (pure False)

  let idx1Name = ncqGetFileName me a
  let idx2Name = ncqGetFileName me b

  (bs1, nw1) <- lift (nwayHashMMapReadOnly idx1Name) >>= \case
                  Nothing -> err ("missed file" <+> pretty idx1Name) >> exit False
                  Just e  -> pure e

  (bs2, nw2) <- lift (nwayHashMMapReadOnly idx2Name) >>= \case
                  Nothing -> err ("missed file" <+> pretty idx2Name) >> exit False
                  Just e  -> pure e

  e <- S.toList_ do
         nwayHashScanAll nw1 bs1 $ \_ k v -> unless (k == emptyKey) do
          S.yield (k,v)

         nwayHashScanAll nw2 bs2 \_ k v -> unless (k == emptyKey) do
          r <- liftIO (nwayHashLookup nw1 bs1 k)
          unless (isJust r) do
            S.yield (k,v)


  let dir = ncqGetWorkDir me

  ts <- liftIO (PFS.getFileStatus idx1Name) <&> PFS.modificationTimeHiRes

  -- result <- lift $ nwayWriteBatch ncqIndexAllocForMerge dir "merged-.cq$" e
  result <- lift $ nwayWriteBatch ncqIndexAlloc dir "merged-.cq$" e

  liftIO $ PFS.setFileTimesHiRes result ts ts

  fki <- ncqGetNewFileKey me IndexFile
  mv result (ncqGetFileName me (IndexFile fki))

  debug $ "state update" <+> pretty a <+> pretty b <+> "=>" <+> pretty fki
  ncqStateUpdate me do
    ncqStateDelIndexFile (coerce a)
    ncqStateDelIndexFile (coerce b)
    ncqStateAddIndexFile ts fki

  pure True

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


