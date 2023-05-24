module Main where

import HBS2.Clock

import Control.Concurrent.Async
import UnliftIO.Exception
import System.IO
import Control.Monad

testOne :: IO ()
testOne = do

  t1 <- async $ forever $ do
          pause @'Seconds 1
          print "ONE"

  t2 <- async $ forever $ do
          pause @'Seconds 2
          print "TWO"

  link t1
  link t2

  print "testOne DONE"
  pause @'Seconds 10

main = do
  hSetBuffering stdout LineBuffering

  testOne

  pause @'Seconds 30

  print "WTF?"

  pure ()
