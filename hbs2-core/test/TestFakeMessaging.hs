module TestFakeMessaging where

import HBS2.Net.Messaging
import HBS2.Net.Messaging.Fake

import Test.Tasty.HUnit

import Control.Monad
import Data.Tuple
import Data.Functor
import System.Random
import Data.IORef
import Data.Word
import Data.Set qualified as Set
import Data.Map qualified as Map

import FakeMessaging


testFakeMessaging1 :: IO ()
testFakeMessaging1 = do

  gen <- newIORef (mkStdGen 0x4387ddaA10124)

  let peers = fmap FakePeer [1..10]

  bus <- newFakeP2P @Fake @Word16 False

  sent <- forM (zip peers peers) $ \(to,from) -> do
            msg <- replicateM 10 $ atomicModifyIORef' gen (swap . random) :: IO [Word16]
            forM msg $ \m -> do
              sendTo bus (To to) (From from) m
              pure ( to, Set.singleton m )

  received <- forM peers $ \me -> do
                msg <- replicateM 10 $ receive bus (To me) <&> fmap snd
                pure ( me, Set.fromList (mconcat msg) )

  let s1 = Map.fromListWith (<>) (mconcat sent)
  let s2 = Map.fromList received

  -- print ("sent", s1)
  -- print ("receive", s2)

  assertEqual "sent == received" s1 s2


