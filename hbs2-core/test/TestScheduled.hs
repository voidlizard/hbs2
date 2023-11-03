module TestScheduled where

import HBS2.Prelude
import HBS2.Clock
import HBS2.ScheduledAction

import Test.Tasty.HUnit

import Control.Monad
import Lens.Micro.Platform
import System.IO (hPrint)
import UnliftIO
import Data.List qualified as List

import Control.Monad.Cont

testScheduled :: IO ()
testScheduled = do

  tres <- newTVarIO mempty

  sch <- defScheduled <&> set scheduleRunPeriod 1.5

  s <- async $ runScheduled sch

  let addAction = schedule sch
      addValue values = atomically $ modifyTVar' tres (values ++)

  addAction 1 (addValue [1, 2, 3])
  addAction 2 (addValue [10, 20, 30])
  addAction 3 (addValue [100, 200, 300])
  addAction 2 do
    throwIO $ userError "fail!"

  addAction 2 do
    error "fail 2!"

  -- addAction 2 do
  --   addValue [1 `div` 0]

  pause @'Seconds 3.5

  cancel s

  let expected = [100,200,300,10,20,30,1,2,3] & List.sort
  results <- readTVarIO tres <&> List.sort

  hPrint stderr results

  assertEqual "all-values-calculated" expected results


testAsync :: IO ()
testAsync = do

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  flip runContT pure $ do

    a <- ContT $ withAsync do
      forever do
        pause @'Seconds 1
        print "1"

    b <- ContT $ withAsync do
      forever do
        pause @'Seconds 2
        print "2"

    c <- ContT $ withAsync do
          pause @'Seconds 5
          print "leaving"

    pause @'Seconds 10

    liftIO $ print "now what?"


