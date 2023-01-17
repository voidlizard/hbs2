module Main where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Net.Messaging
import HBS2.Storage.Simple

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent.Async
import Data.Hashable
import Data.Word
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO


data Fake

instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Word8
                               deriving newtype (Hashable,Num,Enum)
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


  pure ()

test1 :: IO ()
test1 = do

  let peers = [0..3] :: [Peer Fake]

  peerz <- mapM (async . runFakePeer) peers

  void $ waitAnyCatchCancel peerz


