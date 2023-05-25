
module Main where

import HBS2.Clock
import HBS2.Prelude.Plated
-- import Control.Concurrent.Async
import Control.Concurrent (myThreadId)
import UnliftIO.Exception
import UnliftIO.Async
import System.IO
import Control.Monad

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Cont

testOne :: MonadUnliftIO m => m ()
testOne = testOneE `finally` liftIO (print "testOne EXITS")
  where
    testOneE = liftIO $ evalContT $ do
        t1 <- ContT $ withAsync $ forever $ do
                pause @'Seconds 1
                print "ONE"

        t2 <- ContT $ withAsync $ forever $ do
                pause @'Seconds 2
                print "TWO"

        liftIO do
          link t1
          link t2
          print "testOne DONE"
          pause @'Seconds 5
          throwIO MyException


testTwo:: IO ()
testTwo = runResourceT testOneE `finally` print "testTwo EXITS"
  where
    asyncX f = do
      s <- liftIO $ async f
      _ <- register (liftIO $ cancel s)
      pure s

    testOneE = do
        t1 <- asyncX $ forever $ do
                pause @'Seconds 1
                print "ONE"

        t2 <- asyncX $ forever $ do
                pause @'Seconds 2
                print "TWO"

        liftIO $ print "testOne DONE"
        pause @'Seconds 10

        liftIO $ link t1
        liftIO $ link t2

        liftIO do
          print "testOne DONE"
          pause @'Seconds 5
          throwIO MyException


data MyException = MyException deriving (Show)

instance Exception MyException


rootThread :: IO ()
rootThread = do
  testOne `catch` \(e :: SomeException) -> do
    print e
    print "RELOADING!"
    rootThread

main :: IO ()
main = do

  liftIO $ hSetBuffering stdout LineBuffering

  r <- liftIO $ async rootThread

  void $ async $ do
    pause @'Seconds 3.5
    cancel r

  pause @'Seconds 6

  waitCatch r

  print "WTF?"

  pause @'Seconds 30

  pure ()

