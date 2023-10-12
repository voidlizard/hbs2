module TestFileLogger where

import HBS2.System.Logger.Simple
import System.Directory
import Test.Tasty.HUnit

logFile :: FilePath
logFile = "/tmp/testFileLogger.log"

debugPrefix :: SetLoggerEntry
debugPrefix = toFile logFile . logPrefix "[debug] "

testFileLogger :: IO ()
testFileLogger = do
  let msg = "Oh hi Mark"

  setLogging @DEBUG debugPrefix
  debug msg
  setLoggingOff @DEBUG

  fileContent <- readFile logFile
  assertEqual "write == read" fileContent ("[debug] " <> msg <> "\n")
  removeFile logFile
