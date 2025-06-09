{-# Language CPP #-}
{-# Language MultiWayIf #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language RecordWildCards #-}
module HBS2.Data.Log.Structured.NCQ where

-- ^ N-way pseudo-cuckoo disk hash tables

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script
import Data.Config.Suckless.System

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Builder
import Data.Maybe
import Data.Word
import Data.List qualified as List
import Data.Vector qualified as V
import Data.Vector ((!))
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Network.ByteOrder qualified as N
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Fixed
import System.Environment
import System.Posix
import System.Posix.Fcntl
import System.Posix.IO
import System.Posix.Files (setFileSize)
import System.FilePath.Posix
import System.IO.MMap
import System.IO.Temp
import System.IO qualified as IO
import Safe
import Lens.Micro.Platform
import Control.Concurrent.STM qualified as STM
import UnliftIO



nextPowerOf2 :: Word64 -> Word64
nextPowerOf2 0 = 1  -- 0 округляем к 1 (минимальная степень двойки)
nextPowerOf2 n =
  let n' = n - 1
  in foldl (\x shift -> x .|. (x `shiftR` shift)) n' [1,2,4,8,16,32,64] + 1

data NWayHashException =
  NWayHashInvalidMetaData String
  deriving stock (Show, Typeable)

instance Exception NWayHashException


type NWayPageOff     = Word64
type NWayPageBuckets = Word64

data NWayHash =
  NWayHash
  { nwayKeySize      :: Int
  , nwayKeyPartSize  :: Int
  , nwayValueSize    :: Int
  , nwayBucketSize   :: Int
  , nwayPages        :: [(NWayPageOff, NWayPageBuckets)]
  }
  deriving stock (Show)

nwayItemSize :: NWayHash -> Int
nwayItemSize NWayHash{..} = nwayKeySize + nwayValueSize

instance IsContext c => MkSyntax c NWayHash where
  mkSyntax NWayHash{..} =
    mkList [ mkForm "keysize"     [mkInt nwayKeySize]
           , mkForm "keypartsize" [mkInt nwayKeyPartSize]
           , mkForm "valuesize"   [mkInt nwayValueSize]
           , mkForm "bucksize"    [mkInt nwayBucketSize]
           , mkForm "buckets"     [mkInt x | x <- fmap snd nwayPages]
           , mkForm "cqfile"      [mkInt 1]
           ]

instance Pretty NWayHash where
  pretty = pretty . mkSyntax @C

nwayHashMMapReadOnly :: MonadUnliftIO m => FilePath -> m (Maybe (ByteString, NWayHash))
nwayHashMMapReadOnly fn = runMaybeT do

  bs0 <- liftIO $ mmapFileByteString fn Nothing

  let size = BS.length bs0
  let (_,metasize) = BS.splitAt (size - 4) bs0 & over _2 (fromIntegral . N.word32)
  let (_,meta) = BS.splitAt (size - metasize - 4) bs0 & over _2 (BS8.unpack . BS.take metasize)

  let bs1 = BS.take (BS.length bs0 - 4 - metasize) bs0

  metaSyn <- parseTop meta & toMPlus

  nwayKeySize <- headMay [ x | MatchOption "keysize"     (LitIntVal x) <- metaSyn ]
                  & orThrow (NWayHashInvalidMetaData "keysize")
                 <&> fromIntegral

  nwayValueSize <- headMay [ x | MatchOption "valuesize"     (LitIntVal x) <- metaSyn ]
                     & orThrow (NWayHashInvalidMetaData "valuesize")
                     <&> fromIntegral

  nwayBucketSize <- headMay [ x | MatchOption "bucksize"     (LitIntVal x) <- metaSyn ]
                      & orThrow (NWayHashInvalidMetaData "bucksize")
                      <&> fromIntegral

  nwayKeyPartSize <- headMay [ x | MatchOption "keypartsize" (LitIntVal x) <- metaSyn ]
                       & orThrow (NWayHashInvalidMetaData "keypartsize")
                       <&> fromIntegral

  let buckets' = [ bsz | ListVal (SymbolVal "buckets" : bsz) <- metaSyn ]
                   & mconcat

  let buckets = [ fromIntegral n :: NWayPageBuckets | LitIntVal n <- buckets' ]

  let isize = fromIntegral nwayKeySize + fromIntegral nwayValueSize

  let nwayPages= List.scanl' (\sz x -> sz + x*isize* fromIntegral nwayBucketSize) 0 buckets
                  & flip zip buckets

  when (List.null nwayPages) do
    throwIO $ NWayHashInvalidMetaData "buckets"

  pure (bs1,NWayHash{..})

bucketSizes :: Int -> [Int]
bucketSizes maxSize = takeWhile (<= maxSize) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

nwayHashLookup :: MonadUnliftIO m
               => NWayHash
               -> ByteString
               -> ByteString
               -> m (Maybe ByteString)

nwayHashLookup nw@NWayHash{..} mmaped  keyBs = do
  let keySize  = fromIntegral nwayKeySize
  let valSize  = fromIntegral nwayValueSize
  let itemSize = fromIntegral $ nwayItemSize nw
  let buckL    = fromIntegral nwayBucketSize :: Word64
  let buckSize = fromIntegral $ fromIntegral buckL * nwayItemSize  nw
  let emptyKey = BS.replicate keySize 0

  let hxs = chunks nwayKeyPartSize (keyBs :: ByteString)

  flip runContT pure $ callCC \exit -> do

    for_ (zip nwayPages hxs) $ \((pageOff,nbuck), hx) -> do
      let ki = N.word64 hx
      let buck = ki `mod` nbuck
      -- let buck = ki .&. (nbuck - 1)
      let baseOff = pageOff + buck * buckSize
      let buckEnd = baseOff + itemSize * buckL
      -- liftIO $ print $ niceHash <+> pretty ki <+> pretty buck <+> pretty baseOff <+> pretty nbuck <+> pretty pageOff

      flip fix (baseOff :: NWayPageOff) $ \nextEntry -> \case
        eOff | eOff >= buckEnd  -> none
             | otherwise -> do
                 let es = BS.drop (fromIntegral eOff) mmaped
                 let ks = BS.take keySize es

                 if | ks == keyBs -> do
                        exit $ Just (BS.take valSize (BS.drop keySize es))

                    | ks == emptyKey -> do
                        exit Nothing

                    | otherwise ->  do
                        nextEntry (eOff + itemSize)

    pure Nothing

data NWayHashAlloc =
  NWayHashAlloc
  { nwayAllocRatio       :: Fixed E3
  , nwayAllocKeySize     :: Int
  , nwayAllocKeyPartSize :: Int
  , nwayAllocValueSize   :: Int
  , nwayAllocBucketSize  :: Int
  , nwayAllocMinBuckets  :: Int
  , nwayAllocBucketNum   :: NWayHashAlloc -> Int -> Int
  , nwayAllocResize      :: NWayHashAlloc -> Int -> Int -> Int -> Maybe Int
  }

nwayAllocDef :: Fixed E3 -> Int -> Int -> Int -> NWayHashAlloc
nwayAllocDef r ks kps vs =
  NWayHashAlloc r ks kps vs 4 512 nwayAllocPow2 nwayAllocResizeDefault

nwayAllocPow2 :: NWayHashAlloc -> Int -> Int
nwayAllocPow2 NWayHashAlloc{..} num = fromIntegral $
  nextPowerOf2 (ceiling (nwayAllocRatio * (realToFrac num / realToFrac nwayAllocBucketSize)))

nwayAllocResizeDefault :: NWayHashAlloc -> Int -> Int -> Int -> Maybe Int
nwayAllocResizeDefault NWayHashAlloc{..} i c num = Nothing


nwayFileAllocate :: Fd -> COff -> COff -> IO ()
#ifdef darwin_HOST_OS
nwayFileAllocate fd offset size = do
  let chunk = BS.replicate (fromIntegral size) 0
  _ <- fdSeek fd AbsoluteSeek (fromIntegral offset)
  void $ fdWrite fd chunk
#else
nwayFileAllocate = fileAllocate
#endif


nwayWriteBatch :: MonadUnliftIO m
               => NWayHashAlloc
               -> FilePath -- ^ dir
               -> FilePath -- ^ template
               -> [(ByteString, ByteString)]
               -> m FilePath

nwayWriteBatch nwa@NWayHashAlloc{..} path tpl items' = do

 let items = HM.fromList items' & HM.toList

 let  ks     = nwayAllocKeySize

 let  vs     = nwayAllocValueSize
 let  kpiece = nwayAllocKeyPartSize

 let  itemsInBuck = nwayAllocBucketSize
 let  itemSize    = fromIntegral $ ks + vs
 let  buckSize    = fromIntegral $ itemSize * itemsInBuck

 let  kparts = ks `div` fromIntegral kpiece

 fn0 <- liftIO (emptyTempFile path tpl)
 fn <- liftIO (emptyTempFile path (takeBaseName fn0 <>".part"))

 h0 <- openFile fn ReadWriteMode
 fd <- liftIO $ handleToFd  h0
 h <- liftIO $ fdToHandle fd

 flip runContT pure do

   buckets <- newTQueueIO
   leftovers <- newTQueueIO

   void $ ContT $ bracket none $ const do
     hClose h

   wq   <- newTQueueIO

   writer <- ContT $ withAsync  do
      fix \next -> do
       ops <-  atomically do
                void (peekTQueue wq)
                STM.flushTQueue wq

       for_ ops $ \case
         Just (_,op) -> op
         Nothing -> none

       unless (any isNothing ops) next

   flip fix (Nothing,0,0,items) \nextPage (numBuck,pageOff,i,es) -> do

     let buckNum = case numBuck of
                     Just x  -> x
                     Nothing -> max nwayAllocMinBuckets (nwayAllocBucketNum nwa (List.length es))

     atomically $ writeTQueue buckets buckNum

     tvx <- replicateM (fromIntegral buckNum) ( newTVarIO 0 )
     let alloc  = V.fromList tvx

     let pageSize = buckNum * buckSize

     liftIO do
       nwayFileAllocate fd pageOff (fromIntegral pageSize)

     for_ es $ \(k,v) -> do
       let ki = BS.take kpiece (BS.drop (i*kpiece) k ) & N.word64
       let bn = ki `mod` fromIntegral buckNum
       let buckOff = fromIntegral pageOff +  bn * fromIntegral buckSize

       eIdx <- atomically do
                 e <- readTVar (alloc ! fromIntegral bn)
                 if e >= itemsInBuck then do
                   writeTQueue leftovers (k,v)
                   pure Nothing
                 else do
                   writeTVar (alloc ! fromIntegral bn) (e+1)
                   pure $ Just e

       for_ eIdx \e -> liftIO do
         let woff = fromIntegral buckOff + fromIntegral (e * itemSize)
         let op = liftIO do
               hSeek h AbsoluteSeek woff
               BS.hPut h (k <> BS.take (fromIntegral vs) v)

         atomically (writeTQueue wq (Just (woff, op)))

     lo <- atomically $ STM.flushTQueue leftovers

     if | List.null lo -> none

        | i + 1 < fromIntegral kparts -> do
           let resize = nwayAllocResize nwa i buckNum (List.length lo)
           nextPage (resize, pageOff + fromIntegral pageSize, succ i, lo)

        | otherwise -> do
            -- TODO: check-how-it-works
            liftIO (setFileSize fn pageOff)
            nextPage (Just (buckNum*2), pageOff, i, lo)

   atomically $ writeTQueue wq Nothing
   wait writer

   -- finalize write
   bucklist <- atomically $ STM.flushTQueue buckets

   let meta = [ mkForm @C "keysize"     [mkInt ks]
              , mkForm    "keypartsize" [mkInt kpiece]
              , mkForm    "valuesize"   [mkInt vs]
              , mkForm    "bucksize"    [mkInt itemsInBuck]
              , mkForm    "buckets"     (fmap mkInt bucklist)
              , mkForm    "cqfile"      [mkInt 1]
              ]

   let metabs = BS8.pack $ show $ vsep (fmap pretty meta)
   let metaSize = fromIntegral $ BS.length metabs

   liftIO do
     hSeek h SeekFromEnd 0
     BS.hPut h metabs
     BS.hPut h (N.bytestring32 metaSize)
     mv fn fn0

   pure fn0

nwayHashScanAll :: MonadIO m
                => NWayHash
                -> ByteString
                -> ( NWayPageOff -> ByteString -> ByteString -> m () )
                -> m ()

nwayHashScanAll n@NWayHash{..} mmaped action = do
  let itemSize = fromIntegral $ nwayItemSize n
  flip fix (0,mmaped) $ \next (o,bs) -> do
    if BS.null bs then
      none
    else do
      let ks = BS.take nwayKeySize bs
      let vs = BS.take nwayValueSize (BS.drop 32 bs)
      action o ks vs
      next (o+itemSize, BS.drop (fromIntegral itemSize) bs)

