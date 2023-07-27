module Main  where

import HBS2.Prelude.Plated
import HBS2.Defaults
import HBS2.Hash
import HBS2.Clock
import HBS2.Storage.Simple

import HBS2.System.Logger.Simple

import Test.QuickCheck
import Test.Tasty.HUnit

import Control.Concurrent.Async
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Lens.Micro.Platform
import System.Directory
import System.FilePath.Posix
import System.IO.Temp
import Control.Concurrent.STM
import System.ProgressBar
import Control.Concurrent
import System.IO

randomByteString :: Int -> Gen ByteString
randomByteString n = vectorOf n arbitrary <&> LBS.pack
{-# NOINLINE randomByteString #-}

{-# NOINLINE randomSizedByteString #-}
randomSizedByteString :: Gen ByteString
randomSizedByteString =  do
  let low = 0
  let high = 256 -- ceiling $ realToFrac defBlockSize  * 1.5
  size <- choose (low, high)
  randomByteString size

waitTime :: Timeout 'Seconds
waitTime = 30

testSimpleStorageRandomReadWrite :: IO ()
testSimpleStorageRandomReadWrite = do

  withTempDirectory "." "simpleStorageTest" $ \dir -> do

    let opts = [ StoragePrefix (dir </> ".storage")
               ]

    storage <- simpleStorageInit [StoragePrefix (dir </> ".storage")] :: IO (SimpleStorage HbSync)

    exists <- doesDirectoryExist ( storage ^. storageBlocks )

    assertBool "blocks directory exists" exists

    workers <- replicateM 8 $ async  (simpleStorageWorker storage)

    blkQ <- newTQueueIO
    err      <- newTVarIO 0
    errHash  <- newTVarIO 0
    done <- newTVarIO 0

    let succErrIO v = atomically $ modifyTVar v succ

    let tot = toMicroSeconds waitTime
    let st = defStyle { styleWidth = ConstantWidth 50 }
    mon1 <- newProgressBar st 10 (Progress 0 tot ())

    prog <- async $ forever do
              let w = 1
              pause @'Seconds w
              incProgress mon1 (toMicroSeconds w)

    producer <- async $ void $ race ( pause @'Seconds (waitTime + 0.25) ) $ do
                  replicateConcurrently 6 do
                    forever do
                      bs <- generate randomSizedByteString
                      times <- generate (elements [1,1,1,1,2])
                      replicateConcurrently times $ do
                        ha <- putBlock storage bs
                        atomically $ writeTQueue blkQ ha

    checker <- async $ forever do
                 bh <- atomically $ readTQueue blkQ

                 case bh of
                   Nothing -> do
                    succErrIO err
                    -- hPrint stderr "error 1"

                   Just h  -> do
                    blk <- getBlock storage h
                    case blk of
                      Nothing -> do
                        succErrIO err
                        -- hPrint stderr "error 2"

                      Just s -> do
                        let hash = hashObject s
                        if hash /= h then do
                          succErrIO errHash
                        else do
                            succErrIO done
                          -- hPrint stderr "error 3"

    wait producer

    void $ waitAnyCatchCancel $ producer : prog : checker : workers

    e1 <- readTVarIO err
    e2 <- readTVarIO errHash
    ok <- readTVarIO done

    notice $ "errors:" <+> pretty e1 <+> pretty e2
    notice $ "blocks done:" <+> pretty ok

    assertEqual  "errors1" e1 0
    assertEqual  "errors2" e2 0

tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix "[notice] "


main :: IO ()
main = do
  -- hSetBuffering stdout LineBuffering
  -- hSetBuffering stderr LineBuffering

  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix
  setLoggingOff @TRACE

  testSimpleStorageRandomReadWrite


  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE

