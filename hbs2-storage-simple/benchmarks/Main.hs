{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Compact

import System.TimeIt

import DBPipe.SQLite

import System.Environment
import System.FilePath
import System.IO.Temp

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.ByteString.Lazy  qualified as LBS
import Data.Word (Word8)

import Data.Coerce
import Data.Function
import Text.InterpolatedString.Perl6 (qc)
import Control.Monad
import UnliftIO
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map)
import Data.Map qualified as Map

import Streaming.Prelude (Of,Stream)
import Streaming.Prelude qualified as S

import Control.Concurrent.STM (retry,flushTQueue)

import Codec.Serialise

-- Генерация одного случайного байта
randomByte :: IO Word8
randomByte = randomRIO (0, 255)

-- Генерация одной случайной байтовой строки заданной длины
randomByteString :: Int -> IO LBS.ByteString
randomByteString len = LBS.pack <$> replicateM len randomByte

-- Генерация списка из n случайных байтовых строк заданной длины
-- randomByteStrings :: Int -> Int -> IO (S.Stream (S.Of LBS.ByteString ))
-- randomByteStrings n len = replicateM n (randomByteString len)

randomByteStrings :: MonadIO m => Int -> Int -> Stream (Of LBS.ByteString) m ()
randomByteStrings n len = replicateM_ n $ do
    bs <- liftIO $ randomByteString len
    S.yield bs

-- chunkStream :: Monad m => Int -> Stream (Of LBS.ByteString) m r -> Stream (Of [LBS.ByteString]) m r
-- chunkStream n = S.mapsM (sequence . take n) . S.chunksOf n

main :: IO ()
main = do
  (ns:ss:pref:_) <- getArgs

  let n = readDef @Int 100 ns
  let s = readDef @Int 256 ss
  let p = pref

  let bss = randomByteStrings n s
  let bss2 = randomByteStrings n s
  let bss3 = randomByteStrings n s
  let bss4 = randomByteStrings n s
  -- let bss41 = randomByteStrings (n `div` 2) s
  -- let bss42 = randomByteStrings (n`div` 2) s
  -- let bss43 = randomByteStrings (n`div`4) s
  -- let bss44 = randomByteStrings (n`div`4) s

  let path = pref </> ".test-storage"

  storage <- simpleStorageInit [StoragePrefix path] :: IO (SimpleStorage HbSync)

  workers <- replicateM 4 $ async  (simpleStorageWorker storage)

  env <- newDBPipeEnv dbPipeOptsDef (path </> "bench.db")

  withDB env do
    ddl [qc|
      create table if not exists
      wtf ( hash text not null
          , val text not null
          , primary key (hash)
          )
    |]
    commitAll

  print $ "preparing to write" <+> pretty n <+> "chunks"

  timeItNamed "write chunks to simple storage" do
    S.mapM_ (enqueueBlock storage) bss

  timeItNamed "write chunks to sqlite test" do
    withDB env $ transactional do
      flip S.mapM_  bss2 $ \bs -> do
        let h = hashObject @HbSync bs & pretty & show
        insert [qc|insert into wtf (hash,val) values(?,?)|] (h,bs)

  timeItNamed "write chunks to log" do
    fh <- openFile (path </> "lsm") AppendMode
    flip S.mapM_  bss3 $ \bs -> do
      let h = hashObject @HbSync bs & pretty & show
      LBS.hPut fh (serialise (h,bs))
    hClose fh

  timeItNamed "write chunks to log 2" do
    buf <- newIORef (mempty, 0 :: Int)
    fh <- openFile (path </> "lsm2") AppendMode

    flip S.mapM_  bss3 $ \bs -> do
      let h = hashObject @HbSync bs & pretty & show
      num <- atomicModifyIORef buf (\(chunks,sz) -> ((serialise (h,bs) : chunks,sz+1),sz+1))

      when (num >= 16) do
        w <- atomicModifyIORef buf (\(chunks,_) -> ((mempty,0),chunks))
        LBS.hPut fh (mconcat w)

    (w,_) <- readIORef buf
    LBS.hPut fh (mconcat w)
    hClose fh

  timeItNamed "write chunks to compact-storage" do

    temp <- liftIO $ emptyTempFile "." "compact-storage"

    sto <- compactStorageOpen mempty temp

    flip S.mapM_  bss4 $ \bs -> do
      let h = hashObject @HbSync bs
      compactStoragePut sto (coerce h) (LBS.toStrict bs)

    compactStorageClose sto

  timeItNamed "write chunks to LSM-mock" do

    if n*s > 1073741824 then do
      print "too much"
    else do

      let k = 6
      let batch = 100

      rem  <- newTVarIO n
      quit <- newTVarIO False
      out  <- newTQueueIO
      queue <- newTQueueIO

      w1 <- async do
              fix \next -> do
                r <- readTVarIO rem
                when (r > 0) do
                  b <- S.toList_ (randomByteStrings (min r batch) s)
                  atomically $ do
                    writeTQueue queue b
                    modifyTVar rem (+ (-batch))
                  next
              atomically $ writeTVar quit True

      as <- forM [1..k] \j -> async do
              mem <- newIORef mempty
              -- mem <- newTVarIO mempty
              fix \next -> do

                b <- atomically $ do
                      q <- readTVar quit
                      if q
                        then return Nothing
                        else (Just <$> readTQueue queue)
                                 `orElse` (
                                      readTVar quit >>= \z -> if z then pure Nothing else retry
                                  )
                case b of
                  Nothing -> pure ()
                  Just bss -> do

                    new <- for bss $ \bs -> do
                      let h = hashObject @HbSync bs & HashRef
                      pure $ (h, bs)

                    modifyIORef' mem (HashMap.union (HashMap.fromList new))
                    -- atomically $ modifyTVar mem (Map.union (Map.fromList new))
                    next

              co <- readIORef mem
              -- co <- readTVarIO mem
              atomically $ writeTQueue out co

      mapM_ wait (w1:as)

      result <- atomically $ flushTQueue out
      h <- openFile (path </> "lsm3") WriteMode
      LBS.hPut h (serialise result)
      hClose h


