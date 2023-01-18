module Main where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Hash
-- import HBS2.Net.Messaging
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Messaging.Fake
import HBS2.Net.Peer
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra

-- import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit hiding (Timeout)

import Lens.Micro.Platform
import Data.Traversable
import Control.Concurrent.Async
import Data.Hashable
import Data.Word
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise
import System.Exit


data Fake

instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Word8
                               deriving newtype (Hashable,Num,Enum,Real,Integral)
                               deriving stock (Eq,Ord,Show)


instance Pretty (Peer Fake) where
  pretty (FakePeer n) = parens ("peer" <+> pretty n)

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p

instance HasProtocol Fake (BlockSize Fake) where
  type instance ProtocolId (BlockSize Fake) = 1
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise



dontHandle :: Applicative f => a -> f ()
dontHandle = const $ pure ()

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  test1

  -- defaultMain $
  --   testGroup "root"
  --     [
  --       testCase "test1" test1
  --     ]


runFakePeer :: EngineEnv Fake -> IO ()
runFakePeer env = do

  let pid = fromIntegral (hash (env ^. self)) :: Word8

  dir <- canonicalizePath ( ".peers" </> show pid)

  createDirectoryIfMissing True dir

  let opts = [ StoragePrefix dir
             ]

  storage <- simpleStorageInit opts :: IO (SimpleStorage HbSync)

  w <- async $ simpleStorageWorker storage

  let size = 1024*1024

  let blk = B8.concat [ fromString (take 1 $ show x)
                      | x <- replicate size (fromIntegral pid :: Int)
                      ]

  root <- putAsMerkle storage blk

  debug $ "I'm" <+> pretty pid <+> pretty root

  simpleStorageStop storage

  let handleBlockInfo (p, h, sz) = do
       debug $ pretty p <+> "has block" <+> pretty h <+> pretty sz

  runPeer env
    [ makeResponse (blockSizeProto (hasBlock storage) handleBlockInfo)
    ]

  cancel w

  pure ()


test1 :: IO ()
test1 = do

  hSetBuffering stderr LineBuffering

  fake <- newFakeP2P True

  let peers@[p0,p1] = [0..1] :: [Peer Fake]

  envs@[e0,e1] <- forM peers $ \p ->  newEnv p fake

  void $ race (pause (2 :: Timeout 'Seconds)) $ do

    peerz <- mapM (async . runFakePeer) envs

    runEngineM e0 $ do
      request p1 (GetBlockSize @Fake (fromString "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"))
      request p1 (GetBlockSize @Fake (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

      request p0 (GetBlockSize @Fake (fromString "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"))
      request p0 (GetBlockSize @Fake (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

    pause ( 0.5 :: Timeout 'Seconds)

    mapM_ cancel peerz

    (_, e) <- waitAnyCatchCancel peerz

    debug (pretty $ show e)
    debug "we're done"
    assertBool "sucess" True
    exitSuccess

  assertBool "failed" False

