{-# LANGUAGE NumericUnderscores #-}
module TestCompactStorage where

import HBS2.Prelude.Plated
import HBS2.Merkle
import HBS2.OrDie
import HBS2.Hash
import HBS2.Clock
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Storage.Compact
import HBS2.Data.Bundle

import Control.Monad.Except
import Control.Monad
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
import System.TimeIt
import System.IO

import Streaming.Prelude qualified as S

import Test.Tasty.HUnit


testCompactStorageBasic :: IO ()
testCompactStorageBasic = do

  let elems = [ 0 .. 100_000 :: Int ]

  let pt = toPTree (MaxSize 1000) (MaxNum 256) elems

  withSystemTempDirectory "simpleStorageTest1" $ \dir -> do
    let db = dir </> "storage"
    sto <- compactStorageOpen @HbSync mempty db

    root <- makeMerkle 0 pt $ \(_,_,bss) -> do
      void $ putBlock sto bss

    compactStorageClose sto

    sto2 <- compactStorageOpen @HbSync mempty db

    elems2 <- S.toList_ $ walkMerkle @[Int] root ( getBlock sto2 ) $ \case
      Left{}   -> error "missed block"
      Right xs -> mapM_ S.yield  xs

    assertEqual "elems-read-from-storage" elems elems2

testCompactStorageNoDupes :: IO ()
testCompactStorageNoDupes = do

  let elems = [ 0 .. 1_000 :: Int ]

  withSystemTempDirectory "simpleStorageTest2" $ \dir -> do
    let db = dir </> "storage"
    sto <- compactStorageOpen @HbSync mempty db

    for_ elems $ \k -> do
      put sto (LBS.toStrict $ serialise k) (LBS.toStrict $ serialise $ show $ pretty k)

    commit sto

    size1 <- compactStorageSize sto

    here <- for elems $ \e -> do
              let k = LBS.toStrict $ serialise e
              member sto k

    assertBool "all-members-here" (and here)

    for_ elems $ \k -> do
      put sto (LBS.toStrict $ serialise k) (LBS.toStrict $ serialise $ show $ pretty k)
      commit sto

    size2 <- compactStorageSize sto

    assertEqual "no-dupes" size1 size2

    here2 <- for elems $ \e -> do
              let k = LBS.toStrict $ serialise e
              member sto k

    assertBool "all-members-here" (and here2)


