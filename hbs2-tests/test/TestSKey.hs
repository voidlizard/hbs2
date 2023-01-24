module Main where

import HBS2.Net.Proto.Sessions
import HBS2.Hash


import Control.Monad
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 as B
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Map qualified as Map
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import System.TimeIt
import Test.QuickCheck
import Data.HashMap.Strict qualified as HashMap

main :: IO ()
main = do
  g <- initialize $ U.fromList [0xFACABAC]



  ss <- replicateM 1000 $ do
          bytes <- replicateM 256 $ uniformM g :: IO [Char]
          pure $ hashObject @HbSync (B.pack bytes)

  timeItNamed "Cache (Hash HbSync)" $ do

    replicateM_ 1000 $ do

      m1 <- Cache.newCache Nothing :: IO (Cache (Hash HbSync) ())

      forM_ ss $ \key -> Cache.insert m1 key ()
      forM_ ss $ \key -> Cache.lookup' m1 key


  timeItNamed "Cache (SKey)" $ do

    let keys = fmap newSKey ss

    replicateM_ 1000 $ do

      m1 <- Cache.newCache Nothing

      forM_ keys $ \key -> Cache.insert m1 key ()
      forM_ keys $ \key -> Cache.lookup' m1 key


  timeItNamed "HashMap (Hash HbSync)" $ do

    let keys = ss

    replicateM_ 1000 $ do

      m1 <- newTVarIO mempty -- .newCache Nothing

      forM_ keys $ \key -> do
        atomically $ modifyTVar' m1 (HashMap.insert key ())
        -- Cache.insert m1 key ()

      forM_ keys $ \key -> do
        m <- readTVarIO m1
        let !x = HashMap.lookup key m
        pure ()

  timeItNamed "HashMap (Skey)" $ do

    let keys = fmap newSKey ss

    replicateM_ 1000 $ do

      m1 <- newTVarIO mempty -- .newCache Nothing

      forM_ keys $ \key -> do
        atomically $ modifyTVar' m1 (HashMap.insert key ())
        -- Cache.insert m1 key ()

      forM_ keys $ \key -> do
        m <- readTVarIO m1
        let !x = HashMap.lookup key m
        pure ()


  timeItNamed "Map (Hash HbSync)" $ do

    let keys = ss

    replicateM_ 1000 $ do

      m1 <- newTVarIO mempty -- .newCache Nothing

      forM_ keys $ \key -> do
        atomically $ modifyTVar' m1 (Map.insert key ())
        -- Cache.insert m1 key ()

      forM_ keys $ \key -> do
        m <- readTVarIO m1
        let !x = Map.lookup key m
        pure ()


  pure ()

