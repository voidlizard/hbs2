module Main where

import HBS2.Clock
import HBS2.Hash
import HBS2.Net.Messaging
import HBS2.Net.Proto
import HBS2.Prelude
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra

import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit hiding (Timeout)

import Control.Concurrent.Async
import Data.Hashable
import Data.Word
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO
import Data.Char
import Data.ByteString.Lazy.Char8 qualified as B8


data Fake

instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Word8
                               deriving newtype (Hashable,Num,Enum,Real,Integral)
                               deriving stock (Eq,Ord,Show)


instance Pretty (Peer Fake) where
  pretty (FakePeer n) = parens ("peer" <+> pretty n)

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p


main :: IO ()
main = do
  hSetBuffering stderr LineBuffering

  defaultMain $
    testGroup "root"
      [
        testCase "test1" test1
      ]


runFakePeer :: Peer Fake -> IO ()
runFakePeer p = do

  let pid = fromIntegral (hash p) :: Word8

  debug $ "I'm" <+> pretty p <+> pretty pid

  dir <- canonicalizePath ( ".peers" </> show pid)

  createDirectoryIfMissing True dir

  let opts = [ StoragePrefix dir
             ]

  storage <- simpleStorageInit opts :: IO (SimpleStorage HbSync)

  w <- async $ simpleStorageWorker storage

  let size = 1024*1024

  let blk = B8.concat [ fromString (show x) | x <- replicate size (fromIntegral p :: Int) ]

  debug $ pretty $ B8.length blk

  root <- putAsMerkle storage blk

  pause ( 0.1 :: Timeout 'Seconds)

  simpleStorageStop storage

  debug $ pretty root

  pure ()

test1 :: IO ()
test1 = do

  let peers = [0..0] :: [Peer Fake]

  peerz <- mapM (async . runFakePeer) peers

  void $ waitAnyCatchCancel peerz


