{-# Language NumericUnderscores #-}
module Main where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Data.Types.Refs

import Data.ByteString.Lazy qualified as LBS
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

rndHash :: IO HashRef
rndHash = do
  w1 <- replicateM 4 $ randomIO @Word64
  pure $ HashRef $ hashObject @HbSync (serialise w1)

main :: IO ()
main = do
  rnd <- openFile "/dev/random" ReadMode

  lbs <- LBS.hGetNonBlocking rnd $ 32_000_000 * 10

  hashList <- S.toList_ do
    flip fix lbs $ \next rest -> do
      let (a,rest') = LBS.splitAt 32 rest
      S.yield $ HashRef $! HbSyncHash (LBS.toStrict a)
      unless (LBS.null rest') $ next rest'

  chunks <- S.toList_ do
    flip fix hashList $ \next rest -> do
      let (c, rest') = List.splitAt 1_000_000 rest
      S.yield c
      unless (List.null rest') $ next rest'

  pieces <- forConcurrently chunks (pure . HS.fromList)

  hs <- timeItNamed "rebuild index" do
    let hashSet = HS.unions pieces
    print $ length hashSet
    pure hashSet

  void $ timeItNamed "calculate hash" do
    let bs = serialise hs
    let hx = hashObject @HbSync bs
    print $ pretty hx

  putStrLn "now we have partially sorted index"

  hashes <- replicateM 100 rndHash

  timeItNamed "add new items" do
    let hs2 = HS.union hs (HS.fromList hashes)
    -- let hx = hashObject @HbSync (serialise hs2)
    print $ pretty (HS.size hs2) -- <+> pretty hx

  pure ()


