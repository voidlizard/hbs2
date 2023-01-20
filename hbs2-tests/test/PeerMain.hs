{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Hash
import HBS2.Actors
-- import HBS2.Net.Messaging
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.BlockChunks
import HBS2.Net.Messaging
import HBS2.Net.Messaging.Fake
import HBS2.Actors.Peer
import HBS2.Defaults

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra
import HBS2.Actors.ChunkWriter

-- import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

import Codec.Serialise
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Foldable
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import Control.Concurrent
import Data.Default
import Control.Monad.Reader
import Data.Dynamic
import Data.Kind

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue qualified as Q
import Control.Concurrent.STM.TBQueue qualified as TBQ
import Control.Concurrent.STM.TBQueue (TBQueue)

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p



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

makeLenses 'BlockDownload

newBlockDownload :: Hash HbSync -> BlockDownload
newBlockDownload h = BlockDownload h 0 0 0

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


type instance SessionData Fake (BlockSize Fake) = BlockSizeSession Fake
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


main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  test1

  -- defaultMain $
  --   testGroup "root"
  --     [
  --       testCase "test1" test1
  --     ]


newtype PeerEvents e (m :: Type -> Type) =
  PeerEvents
  { onBlockSize :: TVar (Map (Hash HbSync) [HasBlockEvent HbSync e m])
  }

newPeerEventsIO :: forall e m . MonadIO m => IO (PeerEvents e m)
newPeerEventsIO = PeerEvents <$> newTVarIO mempty

addBlockSizeEventNotify :: forall e m . (MonadIO m)
                        => PeerEvents e m
                        -> Hash HbSync
                        -> HasBlockEvent HbSync e m
                        -> m ()

addBlockSizeEventNotify pe h e = do
  void $ liftIO $ atomically $ modifyTVar' (onBlockSize pe) (Map.insertWith (<>) h [e])

emitBlockSizeEvent :: MonadIO m
                   => PeerEvents e m
                   -> Hash HbSync
                   -> (Peer e, Hash HbSync, Maybe Integer)
                   -> m ()

emitBlockSizeEvent pe h event = do
  ev <- liftIO $ atomically $ stateTVar (onBlockSize pe) alter
  for_ ev $ \e -> e event

  where
    alter m =
      let ev = Map.lookup h m
      in (mconcat (maybeToList ev), Map.delete h m)


runFakePeer :: forall e  b m . ( e ~ Fake
                               , MonadIO m
                               , Messaging b e ByteString
                               -- , MonadIO m
                               -- , Response e p m
                               -- , EngineM e m
                               )
            => PeerEvents e m
            -> Peer e
            -> b
            -> EngineM e m ()
            -> IO  ()

runFakePeer ev p0 bus work = do

  env <- newEnv p0 bus

  let pid = fromIntegral (hash (env ^. self)) :: Word8

  dir <- liftIO $ canonicalizePath ( ".peers" </> show pid)

  let chDir = dir </> "tmp-chunks"

  liftIO $ createDirectoryIfMissing True dir

  let opts = [ StoragePrefix dir
             ]

  storage <- simpleStorageInit opts :: IO (SimpleStorage HbSync)

  w <- liftIO $ async $ simpleStorageWorker storage

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

        update @e def (BlockSizeKey h) (over bsBlockSizes (Map.insert p bsz))
        emitBlockSizeEvent ev h (p, h, Just sz)

  let adapter =
        BlockChunksI
        { blkSize        = hasBlock storage
        , blkChunk       = getChunk storage
        , blkGetHash     = error "FUCK" -- FIXME! \c -> getSession' se sBlockDownload c (view sBlockHash)

        -- КАК ТОЛЬКО ПРИНЯЛИ ВСЕ ЧАНКИ (ПРИШЁЛ ПОСЛЕДНИЙ ЧАНК):
        --   СЧИТАЕМ ХЭШ ТОГО, ЧТО ПОЛУЧИЛОСЬ
        --   ЕСЛИ ПОЛУЧИЛОСЬ ХОРОШО --- ТО:
        --     ПЕРЕЗАПИСЫВАЕМ БЛОК В СТОРЕЙДЖ
        --     ГОВОРИМ ОЖИДАЮЩЕЙ СТОРОНЕ, ЧТО БЛОК ПРИНЯТ?
        --     УДАЛЯЕМ КУКУ?
        , blkAcceptChunk = \(c,p,h,n,bs) -> void $ runMaybeT $ do

            let cKey = DownloadSessionKey (p,c)

            -- check if there is a session
            -- FIXME
            -- void $ MaybeT $ getSession' se sBlockDownload cKey id

            let de = newBlockDownload h

            let bslen = fromIntegral $ B8.length bs
            -- TODO: log this situation
            -- FIXME
            -- mbSize   <- MaybeT $ getSession' se sBlockSizes h (Map.lookup p) <&> fromMaybe Nothing
            -- mbChSize <- MaybeT $ getSession' se sBlockDownload cKey (view sBlockChunkSize)

            -- let offset = fromIntegral n * fromIntegral mbChSize :: Offset

            -- updSession se de sBlockDownload cKey (over sBlockOffset (max offset))

            -- liftIO $ do
            --   writeChunk cww cKey h offset bs
            --   updSession se de sBlockDownload cKey (over sBlockWritten (+bslen))

            -- dwnld <- MaybeT $ getSession' se sBlockDownload cKey id

            -- let maxOff  = view sBlockOffset dwnld
            -- let written = view sBlockWritten dwnld
            -- let notify  = view sOnBlockReady dwnld

            -- let mbDone = (maxOff + fromIntegral mbChSize) > fromIntegral mbSize
            --            && written >= mbSize

            -- when mbDone $ lift do
            --   deferred (Proxy @(BlockChunks e)) $ do
            --     h1 <- liftIO $ getHash cww cKey h

            --   -- ПОСЧИТАТЬ ХЭШ
            --   -- ЕСЛИ СОШЁЛСЯ - ФИНАЛИЗИРОВАТЬ БЛОК
            --   -- ЕСЛИ НЕ СОШЁЛСЯ - ТО ПОДОЖДАТЬ ЕЩЕ
            --     when ( h1 == h ) $ do
            --       lift $ commitBlock cww cKey h
            --       lift $ notify h
            --       delSession se sBlockDownload cKey

            -- when (written > mbSize * defBlockDownloadThreshold) $ do
            --   debug $ "SESSION DELETED BECAUSE THAT PEER IS JERK:" <+> pretty p
            --   delSession se sBlockDownload cKey
              -- ЕСЛИ ТУТ ВИСЕТЬ ДОЛГО, ТО НАС МОЖНО ДИДОСИТЬ,
              -- ПОСЫЛАЯ НЕ ВСЕ БЛОКИ ЧАНКА ИЛИ ПОСЫЛАЯ ОТДЕЛЬНЫЕ
              -- ЧАНКИ ПО МНОГУ РАЗ. А МЫ БУДЕМ ХЭШИ СЧИТАТЬ.
              -- ТАК НЕ ПОЙДЕТ
              -- ТАК ЧТО ТУТ ЖДЁМ, ДОПУСТИМ 2*mbSize  и отваливаемся
            pure ()
        }

  peer <- async $ runPeer env
            [ makeResponse (blockSizeProto (hasBlock storage) handleBlockInfo)
            , makeResponse (blockChunksProto adapter)
            ]

  runEngineM env work

  simpleStorageStop storage

  stopChunkWriter cww

  mapM_ cancel [w,cw,peer]


test1 :: IO ()
test1 = do

  hSetBuffering stderr LineBuffering

  fake <- newFakeP2P True

  void $ race (pause (2 :: Timeout 'Seconds)) $ do

      let p0 = 0 :: Peer Fake
      let p1 = 1 :: Peer Fake

      ev1 <- liftIO newPeerEventsIO
      ev0 <- liftIO newPeerEventsIO

      p1Thread <- async $ runFakePeer ev1 p1 fake (liftIO $ forever yield)

      let ini = [ "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"
                , "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"
                ]

      blkQ <- liftIO $ do
        b <- newTBQueueIO defBlockDownloadQ
        traverse_ (atomically . TBQ.writeTBQueue b) ini
        pure b

      p0Thread <- async $ runFakePeer ev0 p0 fake $ do

        let knownPeers = [p1]

        fix \next -> do

          -- НА САМОМ ДЕЛЕ НАМ НЕ НАДО ЖДАТЬ БЛОКИНФЫ.
          -- НАМ НАДО ОТПРАВЛЯТЬ КАЧАТЬ БЛОК, КАК ТОЛЬКО
          -- ПО НЕМУ ПОЯВИЛАСЬ ИНФА

          blkHash <- liftIO $ atomically $ TBQ.readTBQueue blkQ

          -- TODO: надо трекать, может блок-то и найден
          --       либо по всем пирам спросить

          addBlockSizeEventNotify ev0 blkHash $ \case
            (p, h, Just _) -> do
              -- coo <- genCookie (p,blkHash)
              -- let key = DownloadSessionKey (p, coo)
              -- let new = newBlockDownload blkHash
              -- update @Fake new key id
              -- (over bsBlockSizes (Map.insert p bsz))
              request p (GetBlockSize @Fake blkHash)
              -- liftIO $ print $ "DAVAI BLOCK!" <+> pretty h
              -- update
              -- let q = pure ()
              pure ()

            _ -> pure ()

          -- КТО ПЕРВЫЙ ВСТАЛ ТОГО И ТАПКИ
          for_ knownPeers $ \who ->
            request who (GetBlockSize @Fake blkHash)

          next

      let peerz = p0Thread : [p1Thread]

    -- peerz <- mapM (async . uncurry runFakePeer) ee

    --runEngineM e0 $ do


    --  -- TODO: #ASAP generate unique cookie!!
    --  --
    --  -- FIXME: withAllCrap $ do ...
    --  let s0 = (fst . head) ee

    --  newCookie <- genCookie @Fake (p1, h) -- <<~~~ FIXME: generate a good session id!

    --  let cKey@(_, cookie) = (p1, newCookie)
    --  let chsz = defChunkSize

    --  debug $ "new cookie:" <+> pretty cookie

    --  qblk <- liftIO Q.newTQueueIO

    --  let onBlockReady bh = do
    --        liftIO $ atomically $ Q.writeTQueue qblk bh

    --  let def = newBlockDownload h onBlockReady

    --  -- create sessions before sequesting anything
    --  updSession s0 def sBlockDownload cKey (set sBlockChunkSize chsz)

    --  request p1 (GetBlockSize @Fake (fromString "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"))
    --  request p1 (GetBlockSize @Fake (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

    --  request p0 (GetBlockSize @Fake (fromString "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"))
    --  request p0 (GetBlockSize @Fake (fromString "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"))

    --  -- TODO: #ASAP block ready notification

    --  debug $ "REQUEST BLOCK:" <+> pretty h <+> "from" <+> pretty p1

    --  request p1 (BlockChunks @Fake cookie (BlockGetAllChunks h chsz))

    --  blk <- liftIO $ atomically $ Q.readTQueue qblk

    --  debug $ "BLOCK READY:" <+> pretty blk

    --  -- TODO: смотрим, что за блок
    --  --       если Merkle - то качаем рекурсивно
    --  --       если ссылка - то смотрим, что за ссылка
    --  --       проверяем пруфы
    --  --       качаем рекурсивно

    --  -- let mbLink = deserialiseOrFail @Merkle obj

    --  pure ()

      pause ( 1 :: Timeout 'Seconds)

      mapM_ cancel peerz

      (_, e) <- waitAnyCatchCancel peerz

      debug (pretty $ show e)
      debug "we're done"
      assertBool "success" True
      exitSuccess

  assertBool "failed" False

