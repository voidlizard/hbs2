{-# LANGUAGE PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# Language UndecidableInstances #-}
module HBS2.Storage.Compact
  ( Storage(..)
  , CompactStorage
  , compactStorageOpen
  , compactStorageClose
  , compactStorageCommit
  , compactStoragePut
  , compactStorageGet
  , compactStorageDel
  , compactStorageSize
  , compactStorageFindLiveHeads
  , compactStorageRun
  , HBS2.Storage.Compact.keys
  , HBS2.Storage.Compact.member
  , HBS2.Storage.Compact.put
  , HBS2.Storage.Compact.get
  , HBS2.Storage.Compact.del
  , HBS2.Storage.Compact.commit
  ) where

import HBS2.Clock
import HBS2.Hash
import HBS2.Storage

import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder as B
import Data.Binary.Get
import Data.Coerce
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Foldable
import Data.Traversable
import Data.Vector (Vector,(!))
import Data.Vector qualified as V
import Codec.Serialise
import GHC.Generics
import Lens.Micro.Platform
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import UnliftIO

import Foreign
import System.IO.MMap

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
  { idxEntryOffset :: !EntryOffset
  , idxEntrySize   :: !EntrySize
  , idxEntrySeq    :: !Word64
  , idxEntryTomb   :: !Bool
  , idxEntryKey    :: !ByteString
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

data E = New ByteString
       | Upd ByteString IndexEntry
       | Off IndexEntry
       | Del IndexEntry

data Entry = Entry Integer E

pattern Fresh :: Entry -> Entry
pattern Fresh e <- e@(Entry _ ( isFresh -> True ))

pattern Tomb :: Entry -> Entry
pattern Tomb e <- e@(Entry _ ( isTomb -> True ))

pattern Existed :: Entry -> IndexEntry -> Entry
pattern Existed e w <- e@(Entry _ (existed -> Just w))

-- {-# COMPLETE Existed #-}

isAlive :: Entry -> Bool
isAlive = \case
  Entry _ New{}     -> True
  Entry _ Upd{}     -> True
  Entry _ e@(Off{}) -> not (isTomb e)
  _                 -> False

isTomb :: E -> Bool
isTomb (Off e) = idxEntryTomb e
isTomb _ = False

existed :: E -> Maybe IndexEntry
existed = \case
  Off e   -> Just e
  Upd _ e -> Just e
  Del e   -> Just e
  _       -> Nothing

isFresh :: E -> Bool
isFresh e = case e of
              New{} -> True
              Del{} -> True
              Upd{} -> True
              _     -> False

type Bucket = TVar (HashMap ByteString Entry)

type MMaped = (ForeignPtr Word8, Int, Int)

data CompactStorage k =
  CompactStorage
  { csBuckets     :: Int
  , csFile        :: FilePath
  , csHandle      :: MVar Handle
  , csHeaderOff   :: TVar EntryOffset
  , csSeq         :: TVar Integer
  , csKeys        :: Vector Bucket
  , csUncommitted :: TVar Integer
  , csMMapped     :: TVar MMaped
  }

type ForCompactStorage m = MonadIO m

data CompactStorageOpenOpt

data CompactStorageOpenError =
         InvalidHeader
       | BrokenIndex
       | InvalidFwdSection
  deriving stock (Typeable,Show)

instance Exception CompactStorageOpenError

getBucket :: CompactStorage k -> ByteString -> Bucket
getBucket sto bs = do
  let i = maybe 0 (fromIntegral.fst) (BS.uncons bs) `mod` csBuckets sto
  csKeys sto ! i
{-# INLINE getBucket #-}


compactStorageOpen :: forall k m . (ForCompactStorage m)
                   => [CompactStorageOpenOpt]
                   -> FilePath
                   -> m (CompactStorage k)

compactStorageOpen _ fp = do

  let buck = 8

  ha <- openFile fp ReadWriteMode

  sz <- hFileSize ha
  mha <- newMVar ha

  hoff0 <- newTVarIO 0

  keys0 <- replicateM buck (newTVarIO mempty) <&> V.fromList
  uncommitted <- newTVarIO 0

  ss   <- newTVarIO 0

  mmapped <- liftIO (mmapFileForeignPtr fp ReadOnly Nothing)
               >>= newTVarIO

  if sz == 0 then
    pure $ CompactStorage buck fp mha hoff0 ss keys0 uncommitted mmapped
  else do
    (p,header) <- readHeader mha Nothing >>= maybe (throwIO InvalidHeader) pure
    hoff <- newTVarIO p
    let sto = CompactStorage buck fp mha hoff ss keys0 uncommitted mmapped
    readIndex sto (hdrIndexOffset header) (hdrIndexEntries header)

    flip fix (hdrPrev header) $ \next -> \case
      0   -> pure ()
      off ->  do
        (_,pHeader) <- readHeader mha (Just off) >>= maybe (throwIO InvalidHeader) pure
        readIndex sto (hdrIndexOffset pHeader) (hdrIndexEntries pHeader)
        next (hdrPrev pHeader)

    pure sto


readIndex :: ForCompactStorage m
          => CompactStorage k
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

                      slen <- liftIO (try @_ @IOException (BS.hGet ha 2))
                               <&> either (const Nothing) Just
                               & MaybeT
                               <&> LBS.fromStrict

                      len <-  either (const Nothing) (Just . view _3) (runGetOrFail getWord16be slen)
                                 & MaybeT . pure

                      sIdx <- liftIO (try @_ @IOException (BS.hGet ha (fromIntegral len)))
                                >>= either (const mzero) pure
                                <&> LBS.fromStrict

                      deserialiseOrFail @IndexEntry sIdx
                            & either (const mzero) pure

            case what of
              Nothing -> pure (0,mempty :: [IndexEntry])
              Just idx -> next (pred n, idx : acc, succ rn)

    when (rn /= num) do
      throwIO BrokenIndex

    let new = [ (idxEntryKey e,Entry 0 (Off e)) | e <- entries ]
      -- readIndex from newer to older
      -- so we keep only the newer values in map
    atomically do
      for_ new $ \(k,v) -> do
        let tv = getBucket sto k
        modifyTVar tv (HM.insertWith (\_ o -> o) k v)

compactStorageCommit :: ForCompactStorage m => CompactStorage k -> m ()
compactStorageCommit sto = liftIO do
  withMVar (csHandle sto) $ \ha -> do
    hSeek ha SeekFromEnd 0

    mma <- readTVarIO (csMMapped sto)

    kv <- atomically do
            mapM readTVar (csKeys sto) <&> mconcat . V.toList . fmap HM.toList

    let items = [ (k, e)
                | (k, e@Fresh{}) <- kv
                , reallyUpdated mma e
                ]

    unless (List.null items) do

      -- write fwd
      offFwd <- hTell ha
      BS.hPut ha (LBS.toStrict $ toLazyByteString $ word64BE 0)

      let off0 = offFwd + 8

      -- write data
      idxEntries <- flip fix (off0, items, mempty) $ \next (off, what, idx) -> do
         case what of
          [] -> pure idx

          ((_,Entry i (Del e)):rest) | not (idxEntryTomb e) -> do
            next (off + 0, rest, (e { idxEntryTomb = True },i) : idx)

          ((k,Entry i (Upd v e)):rest) -> do
              BS.hPut ha v
              let sz = fromIntegral $ BS.length v
              next (off + sz, rest, (IndexEntry (fromIntegral off) (fromIntegral sz) 0 False k,i) : idx)

          ((k,Entry i (New v)):rest) -> do
            BS.hPut ha v
            let sz = fromIntegral $ BS.length v
            next (off + sz, rest, (IndexEntry (fromIntegral off) (fromIntegral sz) 0 False k,i) : idx)

          ((_,Entry _ _):rest) -> do
            next (off + 0, rest, idx)


      offIdx0 <- hTell ha <&> fromIntegral

      -- write index
      for_ idxEntries $ \(e,_) -> do
        let lbs = serialise e
        BS.hPut ha $ LBS.toStrict (B.toLazyByteString $
                        word16BE (fromIntegral $ LBS.length lbs)
                        <> B.lazyByteString lbs)

      offPrev <- readTVarIO (csHeaderOff sto)

      offCommitHead <- hTell ha

      -- FIXME: maybe-slow-length-calc
      appendHeader ha (fromIntegral offFwd) (Just offPrev) offIdx0 (fromIntegral $ length idxEntries)

      hSeek ha AbsoluteSeek offFwd

      BS.hPut ha (LBS.toStrict $ toLazyByteString $ word64BE (fromIntegral offCommitHead))

      hFlush ha

      hSeek ha SeekFromEnd 0

      offLast <- hTell ha <&> fromIntegral

      remapFile

      atomically do
        writeTVar (csHeaderOff sto) (offLast - headerSize 1)
        for_ idxEntries $ \(e,i) -> do
          let k = idxEntryKey e
          let tv = getBucket sto k
          modifyTVar tv (HM.alter (doAlter (Entry i (Off e))) k)
          resetUncommitedSTM sto

      where

        doAlter y@(Entry i (Off e)) v0 = case v0 of
          -- deleted-during-commit
          Nothing -> Just (Entry i (Del e))

          Just x | getSeq x > getSeq y -> Just x
                 | otherwise           -> Just y

        doAlter _ v = v

        getSeq = \case
          Entry i _ -> i

        remapFile :: ForCompactStorage m => m ()
        remapFile = do
          let fp = csFile sto
          unmapFile sto
          mmapped <- liftIO (mmapFileForeignPtr fp ReadOnly Nothing)
          atomically (writeTVar (csMMapped sto) mmapped)

        -- NOTE: this-might-be-slow
        --   но это правильно, поскольку
        --   у нас **compact** storage и мы не хотим,
        --   что бы его раздувало одинаковыми значениями
        --   Можно попробовать использовать siphash
        --   при загрузке (?)... да ну нахрен, капец долго
        --   будет. если только его не хранить (это можно)
        reallyUpdated mma = \case
          Entry _ (Upd v e) -> readValue mma e /= v

          _ -> True


compactStorageDel :: ForCompactStorage m => CompactStorage k -> ByteString -> m ()
compactStorageDel sto key  = do

  let tvar = getBucket sto key
  val <- readTVarIO tvar <&> HM.lookup key

  case val of
    Nothing -> pure ()

    Just (Entry i (Del _)) -> pure ()

    Just (Entry _ (New _)) -> do
      -- FIXME: if-commit-in-progress-then-put-tomb
      atomically do
        modifyTVar tvar (HM.delete key)
        succUncommitedSTM sto 1

    Just (Existed e what) -> do
      atomically do
        j <- newSequenceSTM sto
        modifyTVar tvar (HM.insert key (Entry j (Del what)))
        succUncommitedSTM sto 1

    -- FIXME: fix-incomplete-pattern-warning
    _ -> pure ()

newSequenceSTM :: CompactStorage k -> STM Integer
newSequenceSTM sto = stateTVar (csSeq sto) (\n -> (n+1,n))

succUncommitedSTM :: CompactStorage k -> Integer -> STM ()
succUncommitedSTM sto k = modifyTVar (csUncommitted sto) (+k)

resetUncommitedSTM :: CompactStorage k -> STM ()
resetUncommitedSTM sto  = writeTVar (csUncommitted sto) 0

compactStorageSize :: ForCompactStorage m => CompactStorage k -> m Integer
compactStorageSize sto = liftIO $ withMVar (csHandle sto) hFileSize

compactStoragePut :: ForCompactStorage m => CompactStorage k -> ByteString -> ByteString -> m ()
compactStoragePut sto k v = do
  let tvar = getBucket sto k

  atomically $ do
    c <- newSequenceSTM sto
    modifyTVar tvar (HM.insertWith check k (Entry c (New v)))

  where
    check (Entry i (New v1)) (Entry _ (Off e))    = Entry i (Upd v1 e)
    check (Entry i (New v1)) (Entry _ (Upd v0 e)) = Entry i (Upd v1 e)
    check x _ = x

readValue :: MMaped -> IndexEntry -> ByteString
readValue what e = do
  let ptr = what & view _1
  BS.fromForeignPtr ptr (fromIntegral $ idxEntryOffset e)
                        (fromIntegral $ idxEntrySize e)
{-# INLINE readValue #-}

compactStorageGet :: ForCompactStorage m => CompactStorage k -> ByteString -> m (Maybe ByteString)
compactStorageGet sto key = do
  let tvar = getBucket sto key
  val <- readTVarIO tvar <&> HM.lookup key

  case val of
    Nothing       -> pure Nothing
    Just (Tomb{}) -> pure Nothing
    Just (Entry _ (Del _)) -> pure Nothing
    Just (Entry _ (New s)) -> pure (Just s)
    Just (Entry _ (Upd s _)) -> pure (Just s)
    Just (Entry _ (Off e)) -> Just <$> (readTVarIO (csMMapped sto) <&> flip readValue e)

compactStorageExists :: ForCompactStorage m => CompactStorage k -> ByteString -> m (Maybe Integer)
compactStorageExists sto key = do
  let tvar = getBucket sto key
  val <- readTVarIO tvar <&> HM.lookup key

  case val of
    Just (Entry _ (New s)) -> pure (Just (fromIntegral (BS.length s)))
    Just (Entry _ (Off e)) -> pure (Just (fromIntegral $ idxEntrySize e))
    Just (Entry _ (Upd v e)) -> pure (Just (fromIntegral $ BS.length v))
    _ -> pure Nothing

unmapFile :: ForCompactStorage m => CompactStorage sto -> m ()
unmapFile sto = do
  mmapped <- readTVarIO (csMMapped sto)
  liftIO $ finalizeForeignPtr (view _1 mmapped)
  -- NOTE: mmapped-is-invalid-now
  --   если теперь позвать что-то, что
  --   читает из этого мапинга -- то всё грохнется


compactStorageClose :: ForCompactStorage m => CompactStorage k -> m ()
compactStorageClose sto = do
  compactStorageCommit sto
  -- FIXME: hangs-forever-on-io-exception
  liftIO $ do
    unmapFile sto
    withMVar (csHandle sto) hClose

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
        fwd <- lift (LBS.fromStrict <$> BS.hGet ha 8)
                 <&> runGetOrFail getWord64be
                 >>= either (const mzero) pure
                 <&> view _3

        h@(o,header) <- MaybeT $ readHeader mv (Just $ fromIntegral fwd)

        let magicOk = hdrMagic header == headerMagic
        let fwdOk   = hdrFwdOffset header == fromIntegral fwdOff

        if magicOk && fwdOk then
          pure h
        else
          mzero

      maybe (pure acc) (\h -> next ( h : acc) ) what


compactStorageRun :: ForCompactStorage m => m ()
compactStorageRun = forever do
  pause @'Seconds 1

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
  liftIO $ BS.hPut ha (LBS.toStrict $ B.toLazyByteString bs)

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
    (p,) <$> BS.hGet ha (headerSize 1)

  let what = flip runGetOrFail (LBS.fromStrict bs) do
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


-- Map-like interface

keys :: ForCompactStorage m => CompactStorage k -> m [ ByteString ]
keys sto = do
  what <- atomically $ mapM readTVar (csKeys sto)
  let w = foldMap HM.toList (V.toList what)
  pure [ k | (k,x) <- w, isAlive x ]

member :: ForCompactStorage m
       => CompactStorage k
       -> ByteString
       -> m Bool
member s k = isJust <$> compactStorageExists s k

put :: ForCompactStorage m
     => CompactStorage k
     -> ByteString
     -> ByteString
     -> m ()

put = compactStoragePut

get  :: ForCompactStorage m
     => CompactStorage k
     -> ByteString
     -> m (Maybe ByteString)

get = compactStorageGet

del :: ForCompactStorage m
     => CompactStorage k
     -> ByteString
     -> m ()

del = compactStorageDel

commit :: ForCompactStorage m
       => CompactStorage sto
       -> m ()
commit = compactStorageCommit

-- Storage instance

translateKey :: Coercible (Hash hash) ByteString
             => ByteString
             -> Hash hash
             -> ByteString
translateKey prefix hash  = prefix <> coerce hash

{-# INLINE translateKey #-}

instance ( MonadIO m, IsKey hash
         , Hashed hash LBS.ByteString
         , Coercible (Hash hash) ByteString
         , Serialise (Hash hash)
         , Key hash ~ Hash hash
         , Eq (Key hash)
         )
  => Storage (CompactStorage hash) hash LBS.ByteString m where

  putBlock = enqueueBlock

  enqueueBlock s lbs = do
    let hash = hashObject @hash lbs
    compactStoragePut s (translateKey "V" hash) (LBS.toStrict lbs)
    pure (Just hash)

  getBlock s hash = do
    compactStorageGet s (translateKey "V" hash) <&> fmap LBS.fromStrict

  getChunk s hash off size = runMaybeT do
    bs <- MaybeT $ compactStorageGet s (translateKey "V" hash)
    pure $ LBS.fromStrict $ BS.take (fromIntegral size) $ BS.drop (fromIntegral off) bs

  hasBlock sto k = do
    compactStorageExists sto (translateKey "V" k)

  updateRef sto ref v = do
    let hash = hashObject @hash ref
    -- TODO: figure-out-what-to-do-with-metadata
    compactStoragePut sto (translateKey "R" hash) (LBS.toStrict (serialise v))

  getRef sto ref = do
    let hash = hashObject @hash ref
    runMaybeT do
      v <- MaybeT $ compactStorageGet sto (translateKey "R" hash)
      deserialiseOrFail @(Hash hash) (LBS.fromStrict v)
             & either (const mzero) pure

  delBlock sto h = do
    compactStorageDel sto (translateKey "V" h)

  delRef sto ref = do
    compactStorageDel sto (translateKey "R" (hashObject @hash ref))


