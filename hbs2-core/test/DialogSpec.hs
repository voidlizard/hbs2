module DialogSpec where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as TastyQ

import Control.Concurrent.Async
import Control.Monad
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Lens.Micro.Platform
import System.IO

import HBS2.Net.Dialog.Core
import HBS2.Net.Dialog.Helpers.List

newtype BSA = BSA { unBSA :: ByteString }
  deriving (Generic, Show)

instance Arbitrary BSA where
  arbitrary = BSA <$> randomSizedByteString

  -- shrink = \case
  --     BSA bs | BS.length bs > 1 ->
  --                 let (bs1, bs2) = BS.splitAt (BS.length bs `div` 2) bs
  --                  in [BSA bs1, BSA bs2]
  --     _ -> []

  shrink = \case
      BSA (BS.uncons -> Just (x, xs)) -> [BSA xs]
      _ -> []

deriving via [BSA] instance Arbitrary Frames

randomByteString :: Int -> Gen ByteString
randomByteString n =
    vectorOf n arbitrary <&> BS.pack
{-# NOINLINE randomByteString #-}

randomSizedByteString :: Gen ByteString
randomSizedByteString =  do
    let low = 0
    let high = 2^13
    size <- choose (low, high)
    randomByteString size
{-# NOINLINE randomSizedByteString #-}

property' name = li . (name, ) . property

testDialog :: TestTree
testDialog = testGroup "dialog" $ buildList do
    li . TastyQ.testProperties "props" $ buildList do

        property' "roundtrip encode Frames" \ xs ->
            (decodeFrames . encodeFrames) xs == Right xs

        property' "encodeFrames is quasidistributive over mappend" \ (xs, ys) ->
            BS.drop (BS.length (encodeFrames xs)) (encodeFrames (xs <> ys))
                == encodeFrames ys

