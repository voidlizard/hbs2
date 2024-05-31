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
import Data.Vector (Vector,(!))
import Data.Vector qualified as V
import Codec.Serialise
import GHC.Generics
-- import System.IO
import Lens.Micro.Platform
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Concurrent.STM.TSem
import Safe
import UnliftIO

import Debug.Trace

-- compact storage
-- for the off-tree data representation
-- may be it will be faster, than Simple storage
-- who knows

newtype EntryOffset = EntryOffset Word64
                      deriving newtype (Ord,Eq,Num,Enum,Real,Integral,Show)
                      deriving stock Generic

newtype FwdEntryOffset = FwdEntryOffset Word64
                         deriving newtype (Ord,Eq,Num,Enum,Real,Integral,Show)
                         deriving stock Generic

newtype EntrySize = EntrySize Word64
                    deriving newtype (Ord,Eq,Num,Enum,Real,Integral,Show)
                    deriving stock Generic


newtype EntryNum = EntryNum Word32
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
  , hdrFwdOffset    :: FwdEntryOffset
  , hdrIndexOffset  :: EntryOffset
  , hdrIndexEntries :: EntryNum
  , hdrPrev         :: EntryOffset
  }
  deriving stock (Show,Generic)

data CompactStorage =
  CompactStorage
  { csHandle     :: MVar Handle
  , csHeaderOff  :: IORef EntryOffset
  -- , csSeq        :: IORef Integer
  , csSeq        :: TVar Integer
  , csKeys       :: Vector (TVar (HashMap ByteString (Either (IndexEntry,Integer) (ByteString,Integer))))
  }

type ForCompactStorage m = MonadIO m

data CompactStorageOpenOpt

data CompactStorageOpenError =
         InvalidHeader
       | BrokenIndex
       | InvalidFwdSection
  deriving stock (Typeable,Show)

instance Exception CompactStorageOpenError



buckets :: Int
buckets = 8

-- FIXME: buckets-hardcode
getKeyPrefix :: ByteString -> Int
getKeyPrefix bs = maybe 0 (fromIntegral.fst) (BS.uncons bs) `mod` buckets
{-# INLINE getKeyPrefix  #-}

compactStorageOpen :: ForCompactStorage m
                   => [CompactStorageOpenOpt]
                   -> FilePath
                   -> m CompactStorage

compactStorageOpen _ fp = do
  ha <- openFile fp ReadWriteMode
  sz <- hFileSize ha
  mha <- newMVar ha

  hoff0 <- newIORef 0

  keys0 <- replicateM buckets (newTVarIO mempty) <&> V.fromList

  -- ss    <- newIORef 0
  ss   <- newTVarIO 0

  if sz == 0 then
    pure $ CompactStorage mha hoff0 ss keys0
  else do
    (p,header) <- readHeader mha Nothing >>= maybe (throwIO InvalidHeader) pure
    traceM (show ("HEADER",header))
    hoff <- newIORef p
    let sto = CompactStorage mha hoff ss keys0
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

    let new = [ (idxEntryKey e,Left (e,0)) | e <- entries ]
      -- readIndex from newer to older
      -- so we keep only the newer values in map
    atomically do
      for_ new $ \(k,v) -> do
        let tv = csKeys sto ! getKeyPrefix k
        modifyTVar tv (HM.insertWith (\_ o -> o) k v)

compactStorageCommit :: ForCompactStorage m => CompactStorage -> m ()
compactStorageCommit sto = liftIO do
  withMVar (csHandle sto) $ \ha -> do
    hSeek ha SeekFromEnd 0

    kv <- atomically do
            mapM readTVar (csKeys sto) <&> mconcat . V.toList . fmap HM.toList

    let items = [ (k, v) | (k, Right v) <- kv ]

    unless (List.null items) do

      -- write fwd
      offFwd <- hTell ha
      LBS.hPut ha (toLazyByteString $ word64BE 0)

      let off0 = offFwd + 8

      -- write data
      idxEntries <- flip fix (off0, items, mempty) $ \next (off, what, idx) -> do
         case what of
          [] -> pure idx
          ((k,(v,i)):rest) -> do
            BS.hPut ha v
            let sz = fromIntegral $ BS.length v
            next (off + sz, rest, (IndexEntry (fromIntegral off) (fromIntegral sz) 0 False k,i) : idx)

      offIdx0 <- hTell ha <&> fromIntegral

      -- write index
      for_ idxEntries $ \(e,_) -> do
        let lbs = serialise e
        LBS.hPut ha (B.toLazyByteString $
                        word16BE (fromIntegral $ LBS.length lbs)
                        <> B.lazyByteString lbs)

      offPrev <- readIORef (csHeaderOff sto)

      offCommitHead <- hTell ha

      -- FIXME: maybe-slow-length-calc
      appendHeader ha (fromIntegral offFwd) (Just offPrev) offIdx0 (fromIntegral $ length idxEntries)

      hSeek ha AbsoluteSeek offFwd

      LBS.hPut ha (toLazyByteString $ word64BE (fromIntegral offCommitHead))

      hFlush ha

      hSeek ha SeekFromEnd 0

      offLast <- hTell ha <&> fromIntegral

      -- atomically do
      atomicWriteIORef (csHeaderOff sto) (offLast - headerSize 1)

      atomically do
        for_ idxEntries $ \(e,i) -> do
          let k = idxEntryKey e
          let tv = csKeys sto ! getKeyPrefix k
          modifyTVar tv (HM.insertWith merge k (Left (e, i)))

      where
        merge new old = if getSeq new >= getSeq old then new else old
        getSeq = \case
          Left (_,i) -> i
          Right (_,i) -> i


compactStoragePut :: ForCompactStorage m => CompactStorage -> ByteString -> ByteString -> m ()
compactStoragePut sto k v = do
  -- TODO: ASAP-do-not-write-value-if-not-changed

  let tvar = csKeys sto ! getKeyPrefix k

  atomically $ do
    c <- stateTVar (csSeq sto) (\n -> (n+1,n))
    modifyTVar tvar (HM.insert k (Right (v,c)))

compactStorageGet :: ForCompactStorage m => CompactStorage -> ByteString -> m (Maybe ByteString)
compactStorageGet sto key = do
  let tvar = csKeys sto ! getKeyPrefix key

  val <- readTVarIO tvar <&> HM.lookup key

  case val of
    Nothing -> pure Nothing
    Just (Right (s,_)) -> pure (Just s)
    Just (Left (e,_))  -> liftIO do
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

compactStorageFindLiveHeads :: ForCompactStorage m
                             => FilePath
                             -> m [(EntryOffset, Header)]

compactStorageFindLiveHeads path = liftIO do
  withFile path ReadMode $ \ha -> do

    mv <- newMVar ha

    flip fix (mempty :: [(EntryOffset, Header)]) $ \next acc -> do

      what <- runMaybeT do

        fwdOff <- hTell ha

        -- fwd section
        fwd <- lift (LBS.hGet ha 8)
                 <&> runGetOrFail getWord64be
                 >>= either (const mzero) pure
                 <&> view _3

        traceM $ show ("JOPA1", fwdOff, fwd)

        h@(o,header) <- MaybeT $ readHeader mv (Just $ fromIntegral fwd)


        let magicOk = hdrMagic header == headerMagic
        let fwdOk   = hdrFwdOffset header == fromIntegral fwdOff

        if magicOk && fwdOk then
          pure h
        else
          mzero

      maybe (pure acc) (\h -> next ( h : acc) ) what


appendHeader :: ForCompactStorage m
             => Handle
             -> FwdEntryOffset     -- fwd section offset
             -> Maybe EntryOffset  -- prev. header
             -> EntryOffset
             -> EntryNum
             -> m ()
appendHeader ha fwdOff poffset ioffset num = do
  let bs =   word16BE headerMagic                     -- 2
          <> word16BE headerVersion                   -- 4
          <> word64BE (coerce fwdOff)                 -- 12
          <> word64BE (coerce ioffset)                -- 20
          <> word32BE (coerce num)                    -- 24
          <> word64BE (coerce $ fromMaybe 0 poffset)  -- 32
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
                       <*> getFwdOffset
                       <*> getOffset
                       <*> getNum
                       <*> getOffset

  pure $ either (const Nothing) (fmap (off,) . Just . view _3) what

  where
    getOffset = EntryOffset <$> getWord64be
    getNum    = EntryNum    <$> getWord32be
    getFwdOffset = FwdEntryOffset <$> getWord64be

headerMagic :: Word16
headerMagic = 32264

headerVersion :: Word16
headerVersion = 1

headerSize :: Integral a => Word16 -> a
headerSize 1 = fromIntegral (32 :: Integer)
headerSize _ = error "unsupported header version"


