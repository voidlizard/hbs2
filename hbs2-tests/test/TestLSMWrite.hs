{-# Language NumericUnderscores #-}
module Main where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Storage.Compact

import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Char8 qualified as BS8
import Data.Function
import Streaming.Prelude qualified as S
import System.TimeIt
import Data.HashSet qualified as HS
import Data.HashSet (HashSet)
import Data.List qualified as List
import UnliftIO
import System.Random
import Data.Word
import Control.Monad
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs
  sto <- compactStorageOpen @HbSync mempty f

  for_ [0..10_000_000] $ \i -> do
    let k = B.toLazyByteString (B.word64BE i) & LBS.toStrict
    let v = BS8.pack (show k)
    put sto k v

  compactStorageClose sto

