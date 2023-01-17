module TestBlockInfoActor where

import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Proto.Actors.BlockInfo
import HBS2.Net.PeerLocator
import HBS2.Net.PeerLocator.Static

import FakeMessaging
import HasProtocol

import Test.Tasty.HUnit

import Test.QuickCheck
import Data.Word
import Data.ByteString qualified as B
import Control.Concurrent.Async


testBlockInfoActor :: IO ()
testBlockInfoActor = do


  np <- newStaticPeerLocator @Fake [1..10]

  a <- createBlockInfoActor (AnyPeerLocator np)
  actor <- async $ runBlockInfoActor a

  let obj = shrink [0x00 .. 0xFF] :: [[Word8]]

  forConcurrently_ obj $ \x -> do
    requestBlockInfo @Fake a Nothing (hashObject (B.pack x) :: Hash HbSync)

  pause ( 1 :: Timeout 'Seconds)

  stopBlockInfoActor a

  waitAnyCatchCancel [actor]

  assertBool "testBlockInfoActor" True


