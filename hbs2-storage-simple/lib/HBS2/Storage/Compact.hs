{-# Language PatternSynonyms #-}
module HBS2.Storage.Compact where


import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder as B
import Data.Binary.Get
import Data.Coerce
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.Map (Map)
import Data.Map qualified as Map
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Foldable
import Data.Traversable
import Codec.Serialise
import GHC.Generics
-- import System.IO
import Lens.Micro.Platform
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Concurrent.STM.TSem
import UnliftIO

import Debug.Trace

-- compact storage
-- for the off-tree data representation
-- may be it will be faster, than Simple storage
-- who knows

newtype EntryOffset = EntryOffset Word64
                      deriving newtype (Ord,Eq,Num,Enum,Real,Integral,Show)
                      deriving stock Generic

newtype EntrySize = EntrySize Word64
                    deriving newtype (Ord,Eq,Num,Enum,Real,Integral,Show)
                    deriving stock Generic


newtype EntryNum = EntryNum Word64
                   deriving newtype (Ord,Eq,Num,Enum,Real,Integral,Show)
                   deriving stock Generic

data IndexEntry =
  IndexEntry
  { idxEntryOffset :: EntryOffset
  , idxEntrySize   :: EntrySize
  , idxEntrySeq    :: Word64
  , idxEntryTomb   :: Bool
  , idxEntryKey    :: ByteString
  }
  deriving stock (Show,Generic)


instance Serialise EntryOffset
instance Serialise EntrySize
instance Serialise EntryNum
instance Serialise IndexEntry

data Header =
  Header
  { hdrMagic        :: Word16
  , hdrVersion      :: Word16
  , hdrIndexOffset  :: EntryOffset
  , hdrIndexEntries :: EntryNum
  , hdrPrev         :: EntryOffset
  }
  deriving stock (Show,Generic)

data CompactStorage =
  CompactStorage
  { csHandle     :: MVar Handle
  , csHandleSem  :: TSem
  , csHeaderOff  :: TVar EntryOffset
  , csKeys       :: TVar (HashMap ByteString (Either IndexEntry ByteString))
  }

type ForCompactStorage m = MonadIO m

data CompactStorageOpenOpt

data CompactStorageOpenError =
         InvalidHeader
       | BrokenIndex
  deriving stock (Typeable,Show)

instance Exception CompactStorageOpenError


compactStorageOpen :: ForCompactStorage m
                   => [CompactStorageOpenOpt]
                   -> FilePath
                   -> m CompactStorage

compactStorageOpen _ fp = do
  ha <- openFile fp ReadWriteMode
  sz <- hFileSize ha
  mha <- newMVar ha

  hoff0 <- newTVarIO 0
  keys0 <- newTVarIO mempty

  sem <- atomically $ newTSem 1

  if sz == 0 then
    pure $ CompactStorage mha sem hoff0 keys0
  else do
    (p,header) <- readHeader mha Nothing >>= maybe (throwIO InvalidHeader) pure
    traceM (show ("HEADER",header))
    hoff <- newTVarIO p
    let sto = CompactStorage mha sem hoff keys0
    readIndex sto (hdrIndexOffset header) (hdrIndexEntries header)

    flip fix (hdrPrev header) $ \next -> \case
      0   -> pure ()
      off ->  do
        (_,pHeader) <- readHeader mha (Just off) >>= maybe (throwIO InvalidHeader) pure
        traceM (show ("PHEADER",pHeader))
        readIndex sto (hdrIndexOffset pHeader) (hdrIndexEntries pHeader)
        next (hdrPrev pHeader)

    pure sto

readIndex :: ForCompactStorage m
          => CompactStorage
          -> EntryOffset
          -> EntryNum
          -> m ()
readIndex sto offset num = liftIO do
  withMVar (csHandle sto) $ \ha -> do
    hSeek ha AbsoluteSeek (fromIntegral offset)
    (rn,entries) <- flip fix (num, mempty, 0) $ \next left -> do
        case left of
          (0,acc,n)   -> pure (n,acc)
          (n,acc,rn) -> do
            what <- runMaybeT do

                      slen <- liftIO (try @_ @IOException (LBS.hGet ha 2))
                               <&> either (const Nothing) Just
                               & MaybeT

                      len <-  either (const Nothing) (Just . view _3) (runGetOrFail getWord16be slen)
                                 & MaybeT . pure

                      sIdx <- liftIO (try @_ @IOException (LBS.hGet ha (fromIntegral len)))
                                >>= either (const mzero) pure

                      deserialiseOrFail @IndexEntry sIdx
                            & either (const mzero) pure

            case what of
              Nothing -> pure (0,mempty :: [IndexEntry])
              Just idx -> next (pred n, idx : acc, succ rn)

    when (rn /= num) do
      throwIO BrokenIndex

    atomically do
      let new = HM.fromList [ (k,Left e) | e@(IndexEntry _ _ _ _ k) <- entries ]
      -- readIndex from newer to older
      -- so we keep only the newer values in map
      modifyTVar (csKeys sto) (HM.unionWith (\_ b -> b) new)

compactStorageCommit :: ForCompactStorage m => CompactStorage -> m ()
compactStorageCommit sto = liftIO do
  withMVar (csHandle sto) $ \ha -> do
    hSeek ha SeekFromEnd 0
    kv <- readTVarIO (csKeys sto) <&> HM.toList

    let items = [ (k, v) | (k, Right v) <- kv ]

    unless (List.null items) do

      off0 <- hTell ha

      idxEntries <- flip fix (off0, items, mempty) $ \next (off, what, idx) -> do
         case what of
          [] -> pure idx
          ((k,v):rest) -> do
            BS.hPut ha v
            let sz = fromIntegral $ BS.length v
            next (off + sz, rest, IndexEntry (fromIntegral off) (fromIntegral sz) 0 False k : idx)

      offIdx0 <- hTell ha <&> fromIntegral

      for_ idxEntries $ \e -> do
        let lbs = serialise e
        LBS.hPut ha (B.toLazyByteString $
                        word16BE (fromIntegral $ LBS.length lbs)
                        <> B.lazyByteString lbs)

      offPrev <- readTVarIO (csHeaderOff sto)

      -- FIXME: maybe-slow-length-calc
      appendHeader ha (Just offPrev) offIdx0 (fromIntegral $ length idxEntries)
      hFlush ha

      hSeek ha SeekFromEnd 0

      offLast <- hTell ha <&> fromIntegral

      let es = HM.fromList [ (idxEntryKey e, Left e) | e <- idxEntries ]

      atomically do
        writeTVar (csHeaderOff sto) (offLast - headerSize 1)
        modifyTVar (csKeys sto) (`mappend` es)

compactStoragePut :: ForCompactStorage m => CompactStorage -> ByteString -> ByteString -> m ()
compactStoragePut sto k v = do
  -- TODO: ASAP-do-not-write-value-if-not-changed
  atomically $ modifyTVar (csKeys sto) (HM.insert k (Right v))

compactStorageGet :: ForCompactStorage m => CompactStorage -> ByteString -> m (Maybe ByteString)
compactStorageGet sto key = do
  val <- readTVarIO (csKeys sto) <&> HM.lookup key
  case val of
    Nothing -> pure Nothing
    Just (Right s) -> pure (Just s)
    Just (Left e)  -> liftIO do
      r <- withMVar (csHandle sto) $ \ha -> do
             try @_ @IOException do
                  hSeek ha AbsoluteSeek (fromIntegral $ idxEntryOffset e)
                  BS.hGet ha (fromIntegral $ idxEntrySize e)
      either throwIO (pure . Just) r

compactStorageClose :: ForCompactStorage m => CompactStorage -> m ()
compactStorageClose sto = do
  compactStorageCommit sto
  -- FIXME: hangs-forever-on-io-exception
  liftIO $ withMVar (csHandle sto) hClose

appendHeader :: ForCompactStorage m
             => Handle
             -> Maybe EntryOffset  -- prev. header
             -> EntryOffset
             -> EntryNum
             -> m ()
appendHeader ha hoffset offset num = do
  let bs =   word16BE headerMagic
          <> word16BE headerVersion
          <> word64BE (coerce offset)
          <> word64BE (coerce num)
          <> word64BE (coerce (fromMaybe 0 hoffset))
          <> byteString (BS.replicate 4 0)
  liftIO $ LBS.hPut ha (B.toLazyByteString bs)

readHeader :: ForCompactStorage m
           => MVar Handle
           -> Maybe EntryOffset
           -> m (Maybe (EntryOffset, Header))

readHeader mha moff = do
  (off,bs) <- liftIO $ withMVar mha $ \ha -> do

    case moff of
      Nothing -> do
        hSeek ha SeekFromEnd (negate $ headerSize 1)
      Just off -> do
        hSeek ha AbsoluteSeek (fromIntegral off)

    p <- hTell ha <&> fromIntegral
    (p,) <$> LBS.hGet ha (headerSize 1)

  let what = flip runGetOrFail bs do
                Header <$> getWord16be
                       <*> getWord16be
                       <*> getOffset
                       <*> getNum
                       <*> getOffset

  pure $ either (const Nothing) (fmap (off,) . Just . view _3) what

  where
    getOffset = EntryOffset <$> getWord64be
    getNum    = EntryNum    <$> getWord64be

headerMagic :: Word16
headerMagic = 32264

headerVersion :: Word16
headerVersion = 1

headerSize :: Integral a => Word16 -> a
headerSize 1 = fromIntegral (32 :: Integer)
headerSize _ = error "unsupported header version"


