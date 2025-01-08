module HBS2.Git3.State.Index where

import HBS2.Git3.Prelude
import HBS2.System.Dir
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Git3.State.Types

import HBS2.Data.Log.Structured

import Data.ByteString qualified as BS
import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as L
import Network.ByteOrder qualified as N
import System.IO.Temp as Temp
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Word
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Kind


import Data.BloomFilter qualified as Bloom
import Data.BloomFilter (Bloom(..))
import Data.BloomFilter.Mutable qualified as MBloom

import Control.Monad.ST

import Control.Concurrent.STM qualified as STM
import Codec.Compression.Zstd.Lazy qualified as ZstdL
import Codec.Serialise
import Streaming.Prelude qualified as S
import Streaming hiding (run,chunksOf)

import UnliftIO
import UnliftIO.IO.File qualified as UIO

import Data.HashPSQ qualified as HPSQ


readLogFileLBS :: forall opts m . ( MonadIO m, ReadLogOpts opts, BytesReader m )
               => opts
               -> ( GitHash -> Int -> ByteString -> m () )
               -> m Int

readLogFileLBS _ action = flip fix 0 \go n -> do
  done <- noBytesLeft
  if done then pure n
    else do
      ssize <- readBytesMaybe 4
                  >>= orThrow SomeReadLogError
                  <&> fromIntegral . N.word32 . LBS.toStrict

      hash  <- readBytesMaybe 20
                  >>= orThrow SomeReadLogError
                  <&> GitHash . BS.copy . LBS.toStrict

      sdata <- readBytesMaybe ( ssize - 20 )
                  >>= orThrow SomeReadLogError

      void $ action hash (fromIntegral ssize) sdata
      go (succ n)

indexPath :: forall m . ( Git3Perks m
                        , MonadReader Git3Env m
                        ) => m FilePath
indexPath = do
  reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet
  getStatePath (AsBase58 reflog) <&> (</> "index")

data IndexEntry =
  IndexEntry
  { entryFile  :: FilePath
  , entryBS    :: N.ByteString
  }

data Index a =
  Index { entries    :: [IndexEntry]
        , bitmap     :: MBloom.MBloom RealWorld GitHash
        }

openIndex :: forall a m . (Git3Perks m, MonadReader Git3Env m)
          => m (Index a)

openIndex = do
  files <- listObjectIndexFiles
  bss <- liftIO $ for files $ \(f,_)  -> (f,) <$> mmapFileByteString f Nothing
  let entries = [ IndexEntry f bs | (f,bs) <- bss ]
  bloom <- liftIO $ stToIO $ MBloom.new bloomHash 10
  pure $ Index entries bloom

indexEntryLookup :: forall a m . (Git3Perks m)
              => Index a
              -> GitHash
              -> m (Maybe N.ByteString)

indexEntryLookup Index{..} h = do
  already_ <- newTVarIO ( mempty :: HashMap GitHash N.ByteString )
  forConcurrently_ entries $ \IndexEntry{..} -> do
    what <- readTVarIO already_ <&> HM.lookup h
    case what of
      Just{} -> none
      Nothing -> do
        offset' <- binarySearchBS 56 ( BS.take 20 . BS.drop 4 ) (coerce h) entryBS
        maybe1 offset' none $ \offset -> do
          let ebs = BS.take 32 $ BS.drop (offset + 4 + 20) entryBS
          atomically $ modifyTVar already_ (HM.insert h ebs)

  readTVarIO already_ <&> headMay . HM.elems

listObjectIndexFiles :: forall m . ( Git3Perks m
                                   , MonadReader Git3Env m
                                   ) => m [(FilePath, Natural)]

listObjectIndexFiles = do
  path <- indexPath
  dirFiles path
    <&> filter ( ("objects*.idx" ?==) .  takeFileName )
    >>= \fs -> for fs $ \f -> do
           z <- fileSize f  <&> fromIntegral
           pure (f,z)


enumEntries :: forall a m . ( Git3Perks m
                          , MonadReader Git3Env m
                          ) => Index a -> ( BS.ByteString -> m () ) -> m ()

enumEntries Index{..} action = do
  forConcurrently_ entries $ \IndexEntry{..} -> do
    scanBS entryBS action

bloomHash :: GitHash  -> [Word32]
bloomHash  gh = [a,b,c,d,e]
    where
      bs = coerce gh
      a = N.word32 (BS.take 4 bs)
      b = N.word32 (BS.take 4 $ BS.drop 4  bs)
      c = N.word32 (BS.take 4 $ BS.drop 8  bs)
      d = N.word32 (BS.take 4 $ BS.drop 12 bs)
      e = N.word32 (BS.take 4 $ BS.drop 16 bs)

startReflogIndexQueryQueue :: forall a m . ( Git3Perks m
                                           , MonadReader Git3Env m
                                           , HasClientAPI PeerAPI UNIX m
                                           , HasClientAPI RefLogAPI UNIX m
                                           , HasStorage m
                                           )
                       => TQueue (BS.ByteString, BS.ByteString -> a, TMVar (Maybe a))
                       -> m  ()

startReflogIndexQueryQueue rq = flip runContT pure do
  files <- lift $ listObjectIndexFiles <&> fmap fst

  -- один файл - не более, чем один поток
  -- мапим файлы
  -- возвращаем функцию запроса?
  -- для каждого файла -- мы создаём отдельную очередь,
  -- нам надо искать во всех файлах

  mmaped <- liftIO $ for files (liftIO . flip mmapFileByteString Nothing)

  answQ <- newTVarIO mempty

  forever $ liftIO do
    requests <- atomically do
                   _ <- peekTQueue rq
                   w <- STM.flushTQueue rq
                   for_ w $ \(k,_,a) -> do
                     modifyTVar answQ (HM.insert k a)
                   pure w

    forConcurrently_ mmaped \bs -> do
      for requests $ \(s,f,answ) -> runMaybeT do

        still <- readTVarIO answQ <&> HM.member s

        guard still

           -- FIXME: size-hardcodes
        w <- binarySearchBS 56 ( BS.take 20 . BS.drop 4 ) s bs
              >>= toMPlus

        let r = f (BS.drop (w * 56) bs)

        atomically do
          writeTMVar answ (Just r)
          modifyTVar answQ (HM.delete bs)

    atomically do
      rest <- readTVar answQ
      for_ rest $ \x -> writeTMVar x Nothing

bloomFilterSize :: Natural -> Natural -> Double -> Natural
bloomFilterSize n k p
    | p <= 0 || p >= 1 = 0
    | otherwise = rnd $ negate (fromIntegral n * fromIntegral k) / log (1 - p ** (1 / fromIntegral k))
  where
    rnd x = 2 ** realToFrac (ceiling (logBase 2 x)) & round

writeReflogIndex :: forall m . ( Git3Perks m
                               , MonadReader Git3Env m
                               , HasClientAPI PeerAPI UNIX m
                               , HasClientAPI RefLogAPI UNIX m
                               , HasStorage m
                               ) => m ()
writeReflogIndex = do

    reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet

    api <- getClientAPI @RefLogAPI @UNIX

    sto <- getStorage

    flip runContT pure do

      what' <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 2) api reflog
                >>= orThrow Git3RpcTimeout

      what <- ContT $ maybe1 what' none

      idxPath <- lift indexPath
      mkdir idxPath

      notice $ "STATE" <+> pretty idxPath

      sink <- S.toList_ do
        walkMerkle (coerce what) (getBlock sto) $ \case
          Left{} -> throwIO MissedBlockError
          Right (hs :: [HashRef]) -> do
            for_ hs $ \h -> void $ runMaybeT do

              tx <- getBlock sto (coerce h)
                       >>= toMPlus

              RefLogUpdate{..} <- deserialiseOrFail @(RefLogUpdate L4Proto) tx
                                    & toMPlus

              AnnotatedHashRef _ href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict _refLogUpdData)
                                           & toMPlus

              -- FIXME: error logging
              lbs <- liftIO (runExceptT (getTreeContents sto href))
                       >>= orThrow MissedBlockError

              pieces <- S.toList_ do
                void $ runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \o s _ -> do
                  lift $ S.yield o

              lift $ S.yield (h, pieces)

      liftIO $ forConcurrently_ sink $ \(tx, pieces) -> do
        idxName <- emptyTempFile idxPath "objects-.idx"
        let ss = L.sort pieces
        UIO.withBinaryFileAtomic idxName WriteMode $ \wh -> do
          for_ ss $ \sha1 -> do
            let key   = coerce @_ @N.ByteString sha1
            let value = coerce @_ @N.ByteString tx
            -- notice $ pretty sha1 <+> pretty tx
            writeSection ( LBS.fromChunks [key,value] ) (LBS.hPutStr wh)

      -- files <- lift listObjectIndexFiles
      -- let num = sum (fmap snd files) `div` 56
      -- let size = bloomFilterSize num 5 0.01

      -- bloom <- liftIO $ stToIO (MBloom.new bloomHash (fromIntegral size))

      -- lift $ enumEntries $ \bs -> do
      --   liftIO $ stToIO $ MBloom.insert bloom (coerce bs)

      -- let bloomIdxName =  idxPath </> "filter"
      -- bytes <- liftIO $ stToIO $ Bloom.freeze bloom

      -- liftIO $ UIO.withBinaryFileAtomic bloomIdxName WriteMode $ \wh -> do
      --   LBS.hPutStr wh "puk"
        -- LBS.hPutStr wh (serialise bytes)
        -- LBS.writeFile (serialise b
        -- for_ ss $ \sha1 -> do
        --   let key   = coerce @_ @N.ByteString sha1
        --   let value = coerce @_ @N.ByteString tx
        --   -- notice $ pretty sha1 <+> pretty tx
        --   writeSection ( LBS.fromChunks [key,value] ) (LBS.hPutStr wh)



