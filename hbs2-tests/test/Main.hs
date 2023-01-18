{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
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
import HBS2.Actors.ChunkWriter

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
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad.Trans.Maybe

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p

newtype ChunkSize = ChunkSize Word16
                    deriving newtype (Num,Enum,Real,Integral,Pretty)
                    deriving stock (Eq,Ord,Show,Data,Generic)


newtype ChunkNum = ChunkNum Word16
                   deriving newtype (Num,Enum,Real,Integral,Pretty)
                   deriving stock (Eq,Ord,Show,Data,Generic)


data Sessions e =
  Sessions
  { _sBlockHash      :: Cache (Cookie e) (Hash HbSync)
  , _sBlockChunkSize :: Cache (Cookie e) ChunkSize
  , _sBlockSizes     :: Cache (Hash HbSync) (Map (Peer e) Size)
  , _sBlockSize      :: Cache (Hash HbSync) Size
  }

makeLenses 'Sessions

type GetBlockChunk h m = Hash h -> Offset -> Size -> m (Maybe ByteString)


data BlockChunksI e m =
  BlockChunksI
  { blkSize         :: GetBlockSize HbSync m
  , blkChunk        :: GetBlockChunk HbSync m
  , blkGetHash      :: Cookie e -> m (Maybe (Hash HbSync))
  , blkAcceptChunk  :: Response e (BlockChunks e) m => (Cookie e, Peer e, Hash HbSync, ChunkNum, ByteString) -> m ()
  }


instance HasCookie e (BlockChunks e) where
  type instance Cookie e = Word32
  getCookie (BlockChunks c _) = Just c

data BlockChunks e = BlockChunks (Cookie e) (BlockChunksProto e)
                     deriving stock (Generic)

data BlockChunksProto e = BlockGetAllChunks (Hash HbSync) ChunkSize
                        | BlockNoChunks
                        | BlockChunk ChunkNum ByteString
                        | BlockLost
                        deriving stock (Generic)


instance Serialise ChunkSize
instance Serialise ChunkNum
instance Serialise (BlockChunksProto e)
instance Serialise (BlockChunks e)


blockChunksProto :: forall e m  . ( MonadIO m
                                  , Response e (BlockChunks e) m
                                  , Pretty (Peer e)
                                  )
                 => BlockChunksI e m
                 -> BlockChunks e
                 -> m ()

blockChunksProto adapter (BlockChunks c p) =
  case p of
    BlockGetAllChunks h size -> deferred proto do
      bsz <- blkSize adapter h

      debug $ "bzs" <+> pretty bsz

      let offsets' = calcChunks (fromJust bsz) (fromIntegral size) :: [(Offset, Size)]
      let offsets = zip offsets' [0..]

      for_ offsets $ \((o,sz),i) -> do
        chunk <- blkChunk adapter h o sz
        maybe (pure ()) (response_ . BlockChunk @e i) chunk

    BlockChunk n bs -> do
      h <- blkGetHash adapter c
      who <- thatPeer proto

      maybe1 h (response_ (BlockLost @e)) $ \hh -> do
        blkAcceptChunk adapter (c, who, hh, n, bs)

    BlockNoChunks {} -> do
      -- TODO: notification
      pure ()

    BlockLost{} -> do
      pure ()

  where
    proto = Proxy @(BlockChunks e)
    response_ pt = response (BlockChunks c pt)

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


emptySessions :: IO (Sessions e)
emptySessions =
  Sessions <$> Cache.newCache (Just defCookieTimeout)
           <*> Cache.newCache (Just defBlockInfoTimeout)
           <*> Cache.newCache (Just defBlockInfoTimeout)
           <*> Cache.newCache (Just defBlockInfoTimeout)

newSession :: (Eq k, Hashable k,MonadIO m)
           => s
           -> Getting (Cache k v) s (Cache k v)
           -> k
           -> v
           -> m ()

newSession se l k v = do
  let cache = view l se
  liftIO $ Cache.insert cache k v

withNewSession se l k v m = newSession se l k v >> m

getSession' se l k fn = do
  let cache = view l se
  liftIO $ Cache.lookup cache k <&> fmap fn

getSession se l k = getSession' se l k id

updSession se def  l k fn = liftIO do
  let cache = view l se
  v <- Cache.fetchWithCache cache k (const $ pure def)
  Cache.insert cache k (fn v)

runFakePeer :: forall e . e ~ Fake => Sessions e -> EngineEnv e -> IO ()
runFakePeer se env = do

  let pid = fromIntegral (hash (env ^. self)) :: Word8

  dir <- canonicalizePath ( ".peers" </> show pid)

  let chDir = dir </> "tmp-chunks"

  createDirectoryIfMissing True dir

  let opts = [ StoragePrefix dir
             ]

  storage <- simpleStorageInit opts :: IO (SimpleStorage HbSync)

  w <- async $ simpleStorageWorker storage

  cww <- newChunkWriterIO (Just chDir)

  cw <- async $ runChunkWriter cww

  let size = 1024*1024

  let blk = B8.concat [ fromString (take 1 $ show x)
                      | x <- replicate size (fromIntegral pid :: Int)
                      ]

  root <- putAsMerkle storage blk

  debug $ "I'm" <+> pretty pid <+> pretty root

  let handleBlockInfo (p, h, sz') = do
       maybe1 sz' (pure ()) $ \sz -> do
        let bsz = fromIntegral sz
        updSession se mempty sBlockSizes h (Map.insert p  bsz)
        updSession se bsz sBlockSize h (const bsz)

       debug $ pretty p <+> "has block" <+> pretty h <+> pretty sz'

  let adapter =
        BlockChunksI
        { blkSize        = hasBlock storage
        , blkChunk       = getChunk storage
        , blkGetHash     = getSession se sBlockHash

        -- И ЧТО ТУТ ДЕЛАТЬ.
        -- ЗАПИСАТЬ ЧАНК В ФАЙЛ КУДА-ТО НА TMP (КУДА?
        -- КАК ТОЛЬКО ПРИНЯЛИ ВСЕ ЧАНКИ (ПРИШЁЛ ПОСЛЕДНИЙ ЧАНК):
        --   СЧИТАЕМ ХЭШ ТОГО, ЧТО ПОЛУЧИЛОСЬ
        --   ЕСЛИ ПОЛУЧИЛОСЬ ХОРОШО --- ТО:
        --     ПЕРЕЗАПИСЫВАЕМ БЛОК В СТОРЕЙДЖ
        --     ГОВОРИМ ОЖИДАЮЩЕЙ СТОРОНЕ, ЧТО БЛОК ПРИНЯТ?
        --     УДАЛЯЕМ КУКУ?
        , blkAcceptChunk = \(c,p,h,n,bs) -> void $ runMaybeT $ do

            -- TODO: log this situation
            mbSize   <- MaybeT $ getSession' se sBlockSizes h (Map.lookup p) <&> fromMaybe Nothing
            mbChSize <- MaybeT $ getSession se sBlockChunkSize c

            let offset = fromIntegral n * fromIntegral mbChSize :: Offset

            liftIO $ do
              -- newBlock cww (p,c) h mbSize
              writeChunk cww (p,c) h offset bs

            -- ОТКУДА УЗНАТЬ РАЗМЕР БЛОКА?
            -- ДОПУСТИМ, ОТ БЛОКИНФО?
            -- ЕСЛИ НИЧЕГО НЕТ? => BLOCK_LOST
            debug $ "got chunk" <+> pretty p
                                <+> pretty h
                                <+> pretty n
                                <+> parens ("off:" <+> pretty offset)
                                <+> pretty (B8.length bs)
                                -- <+> parens (pretty mbSize)
                                -- <+> braces ("chunkSize:" <+> pretty mbChSize)
        }

  runPeer env
    [ makeResponse (blockSizeProto (hasBlock storage) handleBlockInfo)
    , makeResponse (blockChunksProto adapter)
    ]

  simpleStorageStop storage

  stopChunkWriter cww

  pause ( 0.25 :: Timeout 'Seconds)

  mapM_ cancel [w,cw]


test1 :: IO ()
test1 = do

  hSetBuffering stderr LineBuffering

  fake <- newFakeP2P True

  let peers@[p0,p1] = [0..1] :: [Peer Fake]

  envs@[e0,e1] <- forM peers $ \p ->  newEnv p fake

  mtS <- emptySessions @Fake
  let ee = zip (repeat mtS) envs

  void $ race (pause (5 :: Timeout 'Seconds)) $ do

    peerz <- mapM (async . uncurry runFakePeer) ee

    runEngineM e0 $ do
      request p1 (GetBlockSize @Fake (fromString "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"))
      request p1 (GetBlockSize @Fake (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

      request p0 (GetBlockSize @Fake (fromString "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"))
      request p0 (GetBlockSize @Fake (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

      let h = fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"


      -- TODO: generate unique cookie!!
      let cookie = 0
      let s0 = (fst . head) ee

      -- getSession' se sBlockSizes h ???

      withNewSession s0 sBlockHash cookie h $ do
        let chsz = defChunkSize
        updSession s0 chsz sBlockChunkSize cookie (const chsz)
        request p1 (BlockChunks @Fake cookie (BlockGetAllChunks h chsz))

      pure ()

      -- cache <- insert кука mempty
      -- request ...

      -- Я ЗАПРОСИЛ БЛОК
      -- У МЕНЯ НЕТ КУКИ
      -- МНЕ ПРИШЛИ ЧАНКИ
      -- КУКИ НЕТ -> ГОВОРЮ "БЛОК ЛОСТ"
      -- НО ХЗ ГДЕ ДЕРЖАТЬ САМ КЭШ для конкретного подпротокола
      -- request p1 (BlockGetAllChunks @Fake 0 (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))


    pause ( 2 :: Timeout 'Seconds)

    mapM_ cancel peerz

    (_, e) <- waitAnyCatchCancel peerz

    debug (pretty $ show e)
    debug "we're done"
    assertBool "success" True
    exitSuccess

  assertBool "failed" False

