module TestSimpleStorage where

import Data.Maybe
import Data.ByteString.Lazy qualified as LBS
import Control.Concurrent.Async
import Lens.Micro.Platform
import System.Directory
import Prettyprinter

import Test.Tasty.HUnit

import HBS2.Hash
import HBS2.Storage
import HBS2.Storage.Simple

testSimpleStorageInit :: IO ()
testSimpleStorageInit = do
  storage <- simpleStorageInit [StoragePrefix ".storage"] :: IO (SimpleStorage HbSync)

  exists <- doesDirectoryExist ( storage ^. storageBlocks )

  assertBool "blocks directory exists" exists

  worker <- async  (simpleStorageWorker storage)

  let str = "AAAAAAAAAA"

  key <- putBlock storage str

  assertBool "key is Just" (isJust key)

  let hash = fromJust key

  print (pretty key)

  s <- getBlock storage hash

  print s

  assertBool "data read" (isJust s)

  let result = fromJust s

  assertEqual "written data == read data" str result

  cancel worker



