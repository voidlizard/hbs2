module TestSimpleStorage where

import Data.Traversable
import Data.Foldable
import Control.Concurrent.Async
import Control.Concurrent
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO.Temp
import Test.QuickCheck

import Test.Tasty.HUnit

import HBS2.Hash
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Simple


testSimpleStorageInit :: IO ()
testSimpleStorageInit = do

  withSystemTempDirectory "simpleStorageTest" $ \dir -> do

    let opts = [ StoragePrefix (dir </> ".storage")
               ]

    storage <- simpleStorageInit [StoragePrefix (dir </> ".storage")] :: IO (SimpleStorage HbSync)

    exists <- doesDirectoryExist ( storage ^. storageBlocks )

    assertBool "blocks directory exists" exists

    worker <- async  (simpleStorageWorker storage)

    let pieces = shrink [0x00 .. 0xFF] :: [[Word8]]

    forConcurrently_ (take 1000 pieces) $ \piece -> do
    -- for_ (take 1000 pieces) $ \piece -> do

      let str = LBS.pack piece

      key <- putBlock storage str

      -- threadDelay $ 500000
      -- print "ok"

      assertBool "key is Just" (isJust key)

      let hash = fromJust key

      -- print (pretty key)

      s <- getBlock storage hash

      -- print s

      assertBool "data read" (isJust s)

      let result = fromJust s

      assertEqual "written data == read data" str result

      let chuSize = 4

      let chNum =
            let (n,r) = length piece `divMod` chuSize
            in if r == 0 then n else succ n

      chunks' <- forM [0,chuSize ..  (chNum - 1)*chuSize] $ \o -> do
                  getChunk storage hash (fromIntegral o) (fromIntegral chuSize)

      let fromChunks = mconcat $ catMaybes chunks'

      -- print (LBS.length  str, LBS.length fromChunks, chNum)

      assertEqual "bs from chunks == str" str fromChunks

      pure ()

    cancel worker



