module Main where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Storage
import HBS2.Storage.Simple

import System.TimeIt

import DBPipe.SQLite

import System.Environment
import System.FilePath

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.ByteString.Lazy  qualified as LBS
import Data.Word (Word8)

import Text.InterpolatedString.Perl6 (qc)
import Control.Monad
import UnliftIO

import Streaming.Prelude (Of,Stream)
import Streaming.Prelude qualified as S

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

main :: IO ()
main = do
  (ns:ss:pref:_) <- getArgs

  let n = readDef @Int 100 ns
  let s = readDef @Int 256 ss
  let p = pref

  let bss = randomByteStrings n s
  let bss2 = randomByteStrings n s

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

  timeItNamed "write chunks test" do
    S.mapM_ (enqueueBlock storage) bss

  timeItNamed "write chunks to sqlite test" do
    withDB env $ transactional do
      flip S.mapM_  bss2 $ \bs -> do
        let h = hashObject @HbSync bs & pretty & show
        insert [qc|insert into wtf (hash,val) values(?,?)|] (h,bs)

