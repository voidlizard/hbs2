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


-- FIXME: peer should be a part of the key
--        therefore, key is ( p | cookie )
--        but client's cookie in protocol should be just ( cookie :: Word32 )

data BlockDownload =
  BlockDownload
  { _sBlockHash      :: Hash HbSync
  , _sBlockChunkSize :: ChunkSize
  , _sBlockOffset    :: Offset
  , _sBlockWritten   :: Size
  }

data Sessions e =
  Sessions
  { _sBlockDownload  :: Cache (Peer e, Cookie e) BlockDownload
  , _sBlockSizes     :: Cache (Hash HbSync) (Map (Peer e) Size)
  , _sBlockSize      :: Cache (Hash HbSync) Size
  }



makeLenses 'Sessions
makeLenses 'BlockDownload

newBlockDownload :: Hash HbSync -> BlockDownload
newBlockDownload h = BlockDownload h 0 0 0

type GetBlockChunk h m = Hash h -> Offset -> Size -> m (Maybe ByteString)


data BlockChunksI e m =
  BlockChunksI
  { blkSize         :: GetBlockSize HbSync m
  , blkChunk        :: GetBlockChunk HbSync m
  , blkGetHash      :: (Peer e, Cookie e) -> m (Maybe (Hash HbSync))
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

      let offsets' = calcChunks (fromJust bsz) (fromIntegral size) :: [(Offset, Size)]
      let offsets = zip offsets' [0..]

      for_ offsets $ \((o,sz),i) -> do
        chunk <- blkChunk adapter h o sz
        maybe (pure ()) (response_ . BlockChunk @e i) chunk

    BlockChunk n bs -> do
      who <- thatPeer proto
      h <- blkGetHash adapter (who, c)

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


emptySessions :: forall e m . MonadIO m => m (Sessions e)
emptySessions = liftIO $
  Sessions <$> Cache.newCache (Just defCookieTimeout)
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

getSession' se l k fn = do
  let cache = view l se
  liftIO $ Cache.lookup cache k <&> fmap fn

getSession se l k = getSession' se l k id

updSession se def  l k fn = liftIO do
  let cache = view l se
  v <- Cache.fetchWithCache cache k (const $ pure def)
  Cache.insert cache k (fn v)

delSession se l k = liftIO do
  Cache.delete (view l se) k

expireSession se l = liftIO do
  Cache.purgeExpired (view l se)

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

  cww <- newChunkWriterIO storage (Just chDir)

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

        -- here we cache block size information
        updSession se mempty sBlockSizes h (Map.insert p  bsz)
        updSession se bsz sBlockSize h (const bsz)

       debug $ pretty p <+> "has block" <+> pretty h <+> pretty sz'

  let adapter =
        BlockChunksI
        { blkSize        = hasBlock storage
        , blkChunk       = getChunk storage
        , blkGetHash     = \c -> getSession' se sBlockDownload c (view sBlockHash)

        -- КАК ТОЛЬКО ПРИНЯЛИ ВСЕ ЧАНКИ (ПРИШЁЛ ПОСЛЕДНИЙ ЧАНК):
        --   СЧИТАЕМ ХЭШ ТОГО, ЧТО ПОЛУЧИЛОСЬ
        --   ЕСЛИ ПОЛУЧИЛОСЬ ХОРОШО --- ТО:
        --     ПЕРЕЗАПИСЫВАЕМ БЛОК В СТОРЕЙДЖ
        --     ГОВОРИМ ОЖИДАЮЩЕЙ СТОРОНЕ, ЧТО БЛОК ПРИНЯТ?
        --     УДАЛЯЕМ КУКУ?
        , blkAcceptChunk = \(c,p,h,n,bs) -> void $ runMaybeT $ do

            let def = newBlockDownload h
            let cKey = (p,c)

            let bslen = fromIntegral $ B8.length bs
            -- TODO: log this situation
            mbSize   <- MaybeT $ getSession' se sBlockSizes h (Map.lookup p) <&> fromMaybe Nothing
            mbChSize <- MaybeT $ getSession' se sBlockDownload cKey (view sBlockChunkSize)

            let offset = fromIntegral n * fromIntegral mbChSize :: Offset

            updSession se def sBlockDownload cKey (over sBlockOffset (max offset))

            liftIO $ do
              writeChunk cww cKey h offset bs
              updSession se def sBlockDownload cKey (over sBlockWritten (+bslen))

            maxOff <- MaybeT  $ getSession' se sBlockDownload cKey (view sBlockOffset)
            written <- MaybeT $ getSession' se sBlockDownload cKey (view sBlockWritten)

            let mbDone = (maxOff + fromIntegral mbChSize) > fromIntegral mbSize
                       && written >= mbSize

            when mbDone $ lift do
              deferred (Proxy @(BlockChunks e)) $ do
                h1 <- liftIO $ getHash cww cKey h

              -- ПОСЧИТАТЬ ХЭШ
              -- ЕСЛИ СОШЁЛСЯ - ФИНАЛИЗИРОВАТЬ БЛОК
              -- ЕСЛИ НЕ СОШЁЛСЯ - ТО ПОДОЖДАТЬ ЕЩЕ
                when ( h1 == h ) $ do
                  debug $ "THIS BLOCK IS DEFINITELY DONE" <+> pretty h1
                  liftIO $ commitBlock cww cKey h

            when (written > mbSize * defBlockDownloadThreshold) $ do
              debug $ "SESSION DELETED BECAUSE THAT PEER IS JERK:" <+> pretty p
              delSession se sBlockDownload cKey
              -- ЕСЛИ ТУТ ВИСЕТЬ ДОЛГО, ТО НАС МОЖНО ДИДОСИТЬ,
              -- ПОСЫЛАЯ НЕ ВСЕ БЛОКИ ЧАНКА ИЛИ ПОСЫЛАЯ ОТДЕЛЬНЫЕ
              -- ЧАНКИ, ПО МНОГУ РАЗ. А МЫ БУДЕМ ХЭШИ СЧИТАТЬ.
              -- ТАК НЕ ПОЙДЕТ
              -- ТАК ЧТО ТУТ ЖДЁМ, ДОПУСТИМ 2*mbSize  и отваливаемся
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

  void $ race (pause (2 :: Timeout 'Seconds)) $ do

    peerz <- mapM (async . uncurry runFakePeer) ee

    runEngineM e0 $ do
      request p1 (GetBlockSize @Fake (fromString "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"))
      request p1 (GetBlockSize @Fake (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

      request p0 (GetBlockSize @Fake (fromString "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"))
      request p0 (GetBlockSize @Fake (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

      let h = fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"

      -- TODO: generate unique cookie!!
      --
      -- FIXME: withAllCrap $ do ...
      let s0 = (fst . head) ee
      let cKey@(_, cookie) = (p1, 0) -- <<~~~ FIXME: generate a good session id!
      let chsz = defChunkSize
      let def = newBlockDownload h
      updSession s0 def sBlockDownload cKey (set sBlockChunkSize chsz)

      -- TODO: #ASAP block ready notification

      request p1 (BlockChunks @Fake cookie (BlockGetAllChunks h chsz))

      pure ()


    pause ( 1 :: Timeout 'Seconds)

    mapM_ cancel peerz

    (_, e) <- waitAnyCatchCancel peerz

    debug (pretty $ show e)
    debug "we're done"
    assertBool "success" True
    exitSuccess

  assertBool "failed" False

