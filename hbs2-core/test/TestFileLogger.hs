module TestFileLogger where

import HBS2.System.Logger.Simple
import System.Directory
import Test.Tasty.HUnit
import Prettyprinter

logFile :: FilePath
logFile = "/tmp/testFileLogger.log"

debugPrefix :: SetLoggerEntry
debugPrefix = toFile logFile . logPrefix "[debug] "

warnPrefix :: SetLoggerEntry
warnPrefix = toFile logFile . logPrefix "[warn] "

testFileLogger :: IO ()
testFileLogger = do
  let msg1 = "I did not!"
  let msg2 = "Oh hi Mark"

  setLogging @DEBUG debugPrefix
  setLogging @WARN warnPrefix

  debug $ pretty msg1
  warn $ pretty msg2

  setLoggingOff @DEBUG
  setLoggingOff @WARN

  fileContent <- readFile logFile
  assertEqual "written == read" fileContent ("[debug] " <> msg1 <> "\n" <> "[warn] " <> msg2 <> "\n")
  removeFile logFile
