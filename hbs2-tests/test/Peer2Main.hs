{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Actors.ChunkWriter
import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Hash
import HBS2.Net.Messaging.Fake
import HBS2.Net.Proto
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.BlockChunks
import HBS2.Net.Proto.BlockInfo
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra

import Test.Tasty.HUnit

import Codec.Serialise hiding (encode,decode)
import Control.Concurrent.Async
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word
import Lens.Micro.Platform
import Prettyprinter hiding (pipe)
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p

data Fake

data BlockDownload =
  BlockDownload
  { _sBlockHash      :: Hash HbSync
  , _sBlockSize      :: Size
  , _sBlockChunkSize :: ChunkSize
  , _sBlockOffset    :: Offset
  , _sBlockWritten   :: Size
  }

makeLenses 'BlockDownload


instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Word8
                               deriving newtype (Hashable,Num,Enum,Real,Integral)
                               deriving stock (Eq,Ord,Show)


instance Pretty (Peer Fake) where
  pretty (FakePeer n) = parens ("peer" <+> pretty n)


instance HasProtocol Fake (BlockSize Fake) where
  type instance ProtocolId (BlockSize Fake) = 1
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance HasProtocol Fake (BlockChunks Fake) where
  type instance ProtocolId (BlockChunks Fake) = 2
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


type instance SessionData e (BlockSize e) = BlockSizeSession e
type instance SessionData Fake (BlockChunks Fake) = BlockDownload

newtype instance SessionKey Fake (BlockChunks Fake) =
  DownloadSessionKey (Peer Fake, Cookie Fake)
  deriving newtype (Eq, Hashable)
  deriving stock (Generic)

newtype BlockSizeSession e =
  BlockSizeSession
  { _bsBlockSizes :: Map (Peer e) Size
  }

makeLenses 'BlockSizeSession

instance Ord (Peer e) => Default (BlockSizeSession e) where
  def = BlockSizeSession mempty

deriving stock instance Show (BlockSizeSession Fake)



runTestPeer :: Peer Fake
            -> (SimpleStorage HbSync -> IO ())
            -> IO ()

runTestPeer p zu = do

  dir <- liftIO $ canonicalizePath ( ".peers" </> show p)
  let chDir = dir </> "tmp-chunks"
  liftIO $ createDirectoryIfMissing True dir

  let opts = [ StoragePrefix dir
             ]

  stor <- simpleStorageInit opts
  cww  <- newChunkWriterIO  stor (Just chDir)

  sw <- liftIO $ async $ simpleStorageWorker stor
  cw <- liftIO $ async $ runChunkWriter cww

  zu stor

  simpleStorageStop stor
  stopChunkWriter cww

  mapM_ cancel [sw,cw]


handleBlockInfo :: forall e m . ( MonadIO m
                                , Sessions e (BlockSize e) m
                                , Default (SessionData e (BlockSize e))
                                , Ord (Peer e)
                                , Pretty (Peer e)
                                )

                => (Peer e, Hash HbSync, Maybe Integer)
                -> m ()

handleBlockInfo (p, h, sz') = do
   maybe1 sz' (pure ()) $ \sz -> do
    let bsz = fromIntegral sz
    update @e def (BlockSizeKey h) (over bsBlockSizes (Map.insert p bsz))
    liftIO $ debug $ "got block:" <+> pretty (p, h, sz)
    -- FIXME: turn back on event notification
    -- lift $ runEngineM env $ emitBlockSizeEvent ev h (p, h, Just sz) -- TODO: fix this crazy shit


blockDownloadLoop :: forall e . ( HasProtocol e (BlockSize e)
                                , Request e (BlockSize e) (PeerM e IO)
                                , Num (Peer e)
                                ) =>  PeerM e IO ()
blockDownloadLoop = do

  -- w <- subscribe ???

  request 1 (GetBlockSize @e "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt")
  request 1 (GetBlockSize @e "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ")

  fix \next -> do
    liftIO $ print "piu!"

    pause ( 0.85 :: Timeout 'Seconds )
    next

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering

  void $ race (pause (10 :: Timeout 'Seconds)) $ do

    fake <- newFakeP2P True <&> Fabriq

    let (p0:ps) = [0..1] :: [Peer Fake]

    -- others
    others <- forM ps $ \p -> async $ runTestPeer p $ \s  -> do
                let findBlk = hasBlock s

                let size = 1024*1024

                let blk = B8.concat [ fromString (take 1 $ show x)
                                    | x <- replicate size (fromIntegral p :: Int)
                                    ]

                root <- putAsMerkle s blk

                debug $ "I'm" <+> pretty p <+> pretty root

                runPeerM (AnyStorage s) fake p $ do
                  runProto @Fake
                    [ makeResponse (blockSizeProto findBlk dontHandle)
                    -- , makeResponse (blockChunksProto undefined)
                    ]

    our <- async $ runTestPeer p0 $ \s  -> do
                let blk = hasBlock s
                runPeerM (AnyStorage s) fake p0 $ do
                  env <- ask

                  as <- liftIO $ async $ withPeerM env blockDownloadLoop

                  runProto @Fake
                    [ makeResponse (blockSizeProto blk handleBlockInfo)
                    -- , makeResponse (blockChunksProto undefined)
                    ]

                  liftIO $ cancel as

    pause ( 5 :: Timeout 'Seconds)

    mapM_ cancel (our:others)

    (_, e) <- waitAnyCatchCancel (our:others)

    debug (pretty $ show e)
    debug "we're done"
    assertBool "success" True
    exitSuccess

  assertBool "failed" False



