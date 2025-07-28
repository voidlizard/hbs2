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


data IndexEntry = IndexEntry {-# UNPACK #-} !FileKey !Word64 !Word32

unpackIndexEntry :: ByteString -> IndexEntry
unpackIndexEntry  entryBs = do
    let (fks,rest1)  = BS.splitAt 4 entryBs
    let (offs,rest2) = BS.splitAt 8 rest1
    let ss           = BS.take 4 rest2
    let fk   = FileKey (N.word32 fks)
    let off  = N.word64 offs
    let size = N.word32 ss
    IndexEntry fk off size
{-# INLINE unpackIndexEntry #-}

emptyKey :: ByteString
emptyKey = BS.replicate 32 0

ncqLookupIndex :: MonadUnliftIO m
               => HashRef
               -> (ByteString, NWayHash)
               -> m (Maybe IndexEntry )
ncqLookupIndex hx (mmaped, nway) = do
  fmap unpackIndexEntry <$> nwayHashLookup nway mmaped (coerce hx)
{-# INLINE ncqLookupIndex #-}


ncqIndexFile :: MonadUnliftIO m => NCQStorage3 -> DataFile FileKey -> m (Maybe FilePath)
ncqIndexFile n@NCQStorage3{..}  fk = runMaybeT do

  let fp   = toFileName fk & ncqGetFileName n
  fki <- ncqGetNewFileKey n IndexFile

  let dest  = ncqGetFileName n (toFileName (IndexFile fki))

  debug $ "INDEX" <+> pretty fp <+> pretty dest

  items <- S.toList_ do
    ncqStorageScanDataFile n fp $ \offset w key s -> case ncqIsMeta s of
      Just M -> none
      _ -> do
        -- we need size in order to return block size faster
        -- w/o search in fossil
        let fks = N.bytestring32 (coerce fk)
        let rs = (w + ncqSLen) & fromIntegral @_ @Word32 & N.bytestring32
        let os = fromIntegral @_ @Word64 offset & N.bytestring64
        let record = fks <> os <> rs
        S.yield (coerce key, record)

  let (dir,name) = splitFileName fp
  let idxTemp = (dropExtension name <> "-") `addExtension` ".cq$"

  result <- lift $ nwayWriteBatch (nwayAllocDef 1.10 32 8 12) dir idxTemp items

  mv result dest

  stat <- liftIO $ PFS.getFileStatus dest
  let ts = PFS.modificationTimeHiRes stat

  midx <- liftIO (nwayHashMMapReadOnly dest)

  unless (isJust  midx) do
    err $ "can't mmap index" <+> pretty dest

  ncqStateUpdate n do
    ncqStateAddIndexFile ts fki
    ncqStateAddDataFile (coerce fk)
    ncqStateAddFact (FI fk (IndexFile fki))

  (bs,nw) <- toMPlus midx

  nwayHashScanAll nw bs $ \_ k _ -> do
    unless (k == emptyKey) $ atomically $ void $ runMaybeT do
      NCQEntry _ tfk <- MaybeT $ ncqLookupEntrySTM n (coerce k)
      fk' <- MaybeT $ readTVar tfk
      guard (coerce fk == fk') -- remove only own stuff
      lift $ ncqAlterEntrySTM n (coerce k) (const Nothing)

  pure dest

ncqStorageScanDataFile :: MonadIO m
                       => NCQStorage3
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


