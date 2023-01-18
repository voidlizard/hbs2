{-# Language FunctionalDependencies #-}
{-# Language RankNTypes #-}
{-# Language PatternSynonyms #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Hash
-- import HBS2.Net.Messaging
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Messaging.Fake
import HBS2.Net.Peer
import HBS2.Defaults

import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra

-- import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

import Codec.Serialise
import Control.Concurrent.Async
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.Foldable
import Data.Hashable
import Data.Maybe
import Data.Traversable
import Data.Word
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import Data.Cache (Cache)
import Data.Cache qualified as Cache

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p

newtype ChunkSize = ChunkSize Word16
                    deriving newtype (Num,Enum,Real,Integral)
                    deriving stock (Eq,Ord,Show,Data,Generic)


newtype ChunkNum = ChunkNum Word16
                   deriving newtype (Num,Enum,Real,Integral)
                   deriving stock (Eq,Ord,Show,Data,Generic)



type GetBlockChunk h = forall m . MonadIO m => Hash h -> Offset -> Size -> m (Maybe ByteString)


data BlockChunksI e m =
  BlockChunksI
  { blkSize         :: GetBlockSize HbSync m
  , blkChunk        :: GetBlockChunk HbSync
  , blkGetHash      :: Cookie e -> m (Maybe (Hash HbSync))
  , blkAcceptChunk  :: (Hash HbSync, ChunkNum, ByteString) -> m ()
  }


instance HasCookie e (BlockChunks e) where
  type instance Cookie e = Word32

pattern BlockGetAllChunks h s <- BlockGetAllChunks_ _ h s
pattern BlockNoChunks         <- BlockNoChunks_ _
pattern BlockChunk n bs       <- BlockChunk_ _ n bs
pattern BlockLost             <- BlockLost_ _

data BlockChunks e = BlockGetAllChunks_ (Cookie e) (Hash HbSync) ChunkSize
                   | BlockNoChunks_     (Cookie e)
                   | BlockChunk_        (Cookie e) ChunkNum ByteString
                   | BlockLost_         (Cookie e)
                   deriving stock (Generic)


instance Serialise ChunkSize
instance Serialise ChunkNum
instance Serialise (BlockChunks e)
-- instance Serialise (MyCookie e)
-- instance Serialise (Cookie e (BlockChunks e))


blockChunksProto :: forall e m  . ( MonadIO m
                                  , Response e (BlockChunks e) m
                                  )
                 => BlockChunksI e m
                 -> BlockChunks e
                 -> m ()

blockChunksProto adapter =
  \case
    BlockGetAllChunks h size -> deferred proto do
      bsz <- blkSize adapter h

      let offsets' = calcChunks (fromJust bsz) (fromIntegral size) :: [(Offset, Size)]
      let offsets = zip offsets' [0..]

      for_ offsets $ \((o,sz),i) -> do
        chunk <- blkChunk adapter h o sz
        maybe (pure ()) (response . BlockChunk_ @e c i) chunk

    BlockChunk n bs -> do
      -- TODO: getHashByCookie c
      h <- blkGetHash adapter c

      maybe1 h (response (BlockLost_ @e c)) $ \hh -> do
        blkAcceptChunk adapter (hh, n, bs)

    BlockNoChunks {} -> do
      -- TODO: notification
      pure ()

    BlockLost{} -> do
      pure ()

  where
    proto = Proxy @(BlockChunks e)

data Fake

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

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  test1

  -- defaultMain $
  --   testGroup "root"
  --     [
  --       testCase "test1" test1
  --     ]


runFakePeer :: forall e . e ~ Fake => EngineEnv e -> IO ()
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

  blkCookies <- Cache.newCache @(Cookie e) @(Hash HbSync) (Just defCookieTimeout)

  let adapter =
        BlockChunksI
        { blkSize        = hasBlock storage
        , blkChunk       = getChunk storage
        , blkGetHash     = liftIO . Cache.lookup blkCookies
        , blkAcceptChunk = dontHandle
        }

  runPeer env
    [ makeResponse (blockSizeProto (hasBlock storage) handleBlockInfo)
    , makeResponse (blockChunksProto adapter)
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

      -- request p1 (BlockGetAllChunks @Fake 0 (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

      -- Я ЗАПРОСИЛ БЛОК
      -- У МЕНЯ НЕТ КУКИ
      -- МНЕ ПРИШЛИ ЧАНКИ
      -- КУКИ НЕТ -> ГОВОРЮ "БЛОК ЛОСТ"
      -- Q1: ЧТО ДЕЛАТЬ
      -- Q1.1: КАК КУКА ПОПАДЁТ в то, где работает "adapter"
      -- Q2: КАК ДЕЛАТЬ ЗАПРОСЫ
      --
      -- ОТСЮДА СЛЕДУЕТ: Cookie должны жить в Engine и быть там доступны
      -- В монаде Response тоже должна быть кука
      --
      -- Как быть с тем, что кука может не поддерживаться подпротоколом?
      -- Требовать HasCookie у всех?

    pause ( 0.5 :: Timeout 'Seconds)

    mapM_ cancel peerz

    (_, e) <- waitAnyCatchCancel peerz

    debug (pretty $ show e)
    debug "we're done"
    assertBool "success" True
    exitSuccess

  assertBool "failed" False

