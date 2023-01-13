module TestBlockInfoActor where

import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Proto.Actors.BlockInfo

import Test.Tasty.HUnit

import Test.QuickCheck
import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Control.Concurrent.Async

testBlockInfoActor :: IO ()
testBlockInfoActor = do

  a <- createBlockInfoActor
  actor <- async $ runBlockInfoActor a

  let obj = shrink [0x00 .. 0xFF] :: [[Word8]]

  forConcurrently_ obj $ \x -> do
    requestBlockInfo a Nothing (hashObject (B.pack x) :: Hash HbSync)

  pause ( 1 :: Timeout 'Seconds)

  stopBlockInfoActor a

  waitAnyCatchCancel [actor]

  assertBool "testBlockInfoActor" True


