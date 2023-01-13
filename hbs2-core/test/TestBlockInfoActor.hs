module TestBlockInfoActor where

import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Proto.Actors.BlockInfo
import HBS2.Net.PeerLocator
import HBS2.Net.PeerLocator.Static

import Test.Tasty.HUnit

import Test.QuickCheck
import Data.Word
import Data.Hashable (Hashable)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Control.Concurrent.Async

data Fake

instance IsPeer Fake where
  newtype instance Peer Fake = FakePeer Int
                               deriving stock (Eq,Ord,Show)
                               deriving newtype (Hashable,Num,Enum,Real,Integral)

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


