module TestActors where

import HBS2.Actors
import HBS2.Clock

import Test.Tasty.HUnit

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue qualified as Q
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Concurrent.Async
import Control.Concurrent

testActorsBasic :: IO ()
testActorsBasic = do

  sink <- newTQueueIO @Int

  pip <- newPipeline 10

  wpip <- async $ runPipeline pip

  let nums = [1..1000] :: [Int]

  forConcurrently_ nums $ \n -> do
    addJob pip do
      atomically $ Q.writeTQueue sink n

  pause ( 0.25 :: Timeout 'Seconds )

  stopPipeline pip

  void $ waitAnyCatchCancel [wpip]

  ll <- atomically $ Q.flushTQueue sink

  assertEqual "alive" 1000 (length ll)



