module TestCompactStorage where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Hash
import HBS2.Clock
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Storage.Compact
import HBS2.Data.Bundle

import Control.Monad.Except
import Control.Monad
import Data.Traversable
import Data.Foldable
import Control.Concurrent.Async
import Control.Concurrent
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO.Temp
import Test.QuickCheck
import System.TimeIt
import System.IO

import Test.Tasty.HUnit


testCompactStorageBasic :: IO ()
testCompactStorageBasic = do

  withSystemTempFile "simpleStorageTest1" $ \fn ha -> do
    pure ()

  pure ()

