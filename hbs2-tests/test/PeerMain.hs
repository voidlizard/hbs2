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
import Data.Foldable hiding (find)
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
  , _sBlockSize      :: Size
  , _sBlockChunkSize :: ChunkSize
  , _sBlockOffset    :: Offset
  , _sBlockWritten   :: Size
  }

makeLenses 'BlockDownload

newBlockDownload :: Hash HbSync -> BlockDownload
newBlockDownload h = BlockDownload h 0 0 0 0

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


-- TODO: абстрактные нотификации, т.к это всё типизируется
--       по ключу-значению

data PeerEvents e (m :: Type -> Type) =
  PeerEvents
  { onBlockSize  :: TVar (Map (Hash HbSync) [HasBlockEvent HbSync e m])
  , onBlockReady :: TVar (Map (Hash HbSync) [OnBlockReady HbSync m])
  }

newPeerEventsIO :: forall e m . MonadIO m => IO (PeerEvents e m)
newPeerEventsIO = PeerEvents <$> newTVarIO mempty
                             <*> newTVarIO mempty

addBlockSizeEventNotify :: forall e m . (MonadIO m)
                        => PeerEvents e m
                        -> Hash HbSync
                        -> HasBlockEvent HbSync e m
                        -> m ()

addBlockSizeEventNotify pe h e = do
  void $ liftIO $ atomically $ modifyTVar' (onBlockSize pe) (Map.insertWith (<>) h [e])

addBlockReadyEventNotify :: forall e m . (MonadIO m)
                        => PeerEvents e m
                        -> Hash HbSync
                        -> OnBlockReady HbSync m
                        -> m ()

addBlockReadyEventNotify pe h e = do
  void $ liftIO $ atomically $ modifyTVar' (onBlockReady pe) (Map.insertWith (<>) h [e])

emitBlockSizeEvent :: forall e m . MonadIO m
                   => PeerEvents e m
                   -> Hash HbSync
                   -> (Peer e, Hash HbSync, Maybe Integer)
                   -> m  ()

emitBlockSizeEvent pe h event = do
  ev <- liftIO $ atomically $ stateTVar (onBlockSize pe) alter
  for_ ev $ \e -> e event

  where
    alter m =
      let ev = Map.lookup h m
      in (mconcat (maybeToList ev), Map.delete h m)


emitBlockReadyEvent :: forall e m . MonadIO m
                    => PeerEvents e m
                    -> Hash HbSync
                    -> m  ()

emitBlockReadyEvent pe h = do
  ev <- liftIO $ atomically $ stateTVar (onBlockReady pe) alter
  for_ ev $ \e -> e h

  where
    alter m =
      let ev = Map.lookup h m
      in (mconcat (maybeToList ev), Map.delete h m)


runFakePeer :: forall e  b . ( e ~ Fake
                             -- , MonadIO m
                             , Messaging b e ByteString
                             -- , Sessions Fake (BlockSize Fake)
                             -- , m ~ ResponseM Fake IO
                             -- , MonadIO m
                             -- , Response e p m
                             -- , EngineM e m
                             )
            => PeerEvents e (EngineM e IO)
            -> Peer e
            -> b
            -> EngineM e IO ()
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
        lift $ runEngineM env $ emitBlockSizeEvent ev h (p, h, Just sz) -- TODO: fix this crazy shit

  let adapter =
        BlockChunksI
        { blkSize     = hasBlock storage
        , blkChunk    = getChunk storage
        , blkGetHash  = \c -> find (DownloadSessionKey c) (view sBlockHash)

        -- КАК ТОЛЬКО ПРИНЯЛИ ВСЕ ЧАНКИ (ПРИШЁЛ ПОСЛЕДНИЙ ЧАНК):
        --   СЧИТАЕМ ХЭШ ТОГО, ЧТО ПОЛУЧИЛОСЬ
        --   ЕСЛИ ПОЛУЧИЛОСЬ ХОРОШО --- ТО:
        --     ПЕРЕЗАПИСЫВАЕМ БЛОК В СТОРЕЙДЖ
        --     ГОВОРИМ ОЖИДАЮЩЕЙ СТОРОНЕ, ЧТО БЛОК ПРИНЯТ?
        --     УДАЛЯЕМ КУКУ?
        , blkAcceptChunk = \(c,p,h,n,bs) -> void $ runMaybeT $ do

            let cKey = DownloadSessionKey (p,c)

            -- check if there is a session
            -- FIXME:
            -- TODO: log situation when no session
            dwnld <- MaybeT $ find cKey id

            let bslen = fromIntegral $ B8.length bs

            let mbSize   = view sBlockSize dwnld
            let mbChSize = view sBlockChunkSize dwnld

            let offset0 = fromIntegral n * fromIntegral mbChSize :: Offset

            liftIO $ do
              writeChunk cww cKey h offset0 bs

            let written = view sBlockWritten dwnld + bslen
            let maxOff  = max offset0 (view sBlockOffset dwnld)

            lift $ update dwnld cKey ( set sBlockOffset maxOff
                                     . set sBlockWritten written
                                     )

            let mbDone = (maxOff + fromIntegral mbChSize) > fromIntegral mbSize
                       && written >= mbSize

            when mbDone $ lift do
              deferred (Proxy @(BlockChunks e)) $ do
                h1 <- liftIO $ getHash cww cKey h

              -- ПОСЧИТАТЬ ХЭШ
              -- ЕСЛИ СОШЁЛСЯ - ФИНАЛИЗИРОВАТЬ БЛОК
              -- ЕСЛИ НЕ СОШЁЛСЯ - ТО ПОДОЖДАТЬ ЕЩЕ
                when ( h1 == h ) $ do
                  liftIO $ commitBlock cww cKey h
                  expire cKey
                  lift $ runEngineM env $ emitBlockReadyEvent ev h -- TODO: fix this crazy shit

            when (written > mbSize * defBlockDownloadThreshold) $ do
              debug $ "SESSION DELETED BECAUSE THAT PEER IS JERK:" <+> pretty p
              lift $ expire cKey
              -- ЕСЛИ ТУТ ВИСЕТЬ ДОЛГО, ТО НАС МОЖНО ДИДОСИТЬ,
              -- ПОСЫЛАЯ НЕ ВСЕ БЛОКИ ЧАНКА ИЛИ ПОСЫЛАЯ ОТДЕЛЬНЫЕ
              -- ЧАНКИ ПО МНОГУ РАЗ. А МЫ БУДЕМ ХЭШИ СЧИТАТЬ.
              -- ТАК НЕ ПОЙДЕТ
              -- ТАК ЧТО ТУТ ЖДЁМ, ДОПУСТИМ 2*mbSize  и отваливаемся
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

      ev1 <- newPeerEventsIO
      ev0 <- newPeerEventsIO

      p1Thread <- async $ runFakePeer ev1 p1 fake $ forever $ liftIO yield

      -- TODO: замутить мап/кэш со статистикой по блоку:
      --       сколько блок там маринуется и т.п.
      --       Если блок в этом кэше и еще не скачан, то
      --       ... пробуем качать повторно?
      --       ... увеличиваем время
      --       ... если не появилось новых пиров
      --       ... запоминать, у какого пира уже спрашивали и стараться
      --       ... спрашивать у других?
      --       ... для каждого блока - вести список, у кого лучше спрашивать?
      --       ... и там whilelist, blacklist
      --       ... не дохрена ли это будет занимать?
      --
      --       ... и тут, короче, еще кэш WiP
      --       ... и еще один поток, который это всё хэндлит, например:
      --       ... берём статистику блоков, берём wip
      --       ... если блок не wip и до сих пор в мапе --- то то добавляем
      --       ... в очередь.
      --
      --       ... блоку пишем, у каких пиров уже спрашивали (Set)
      --       ... блоку пишем, когда стартовал процесс
      --
      --

      let ini = [ "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"
                , "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"
                ]

      blkQ <- liftIO $ do
        b <- newTBQueueIO defBlockDownloadQ
        traverse_ (atomically . TBQ.writeTBQueue b) ini
        pure b

      p0Thread <- async $ runFakePeer ev0 p0 fake $ do

        -- TODO: random shuffle and take X
        -- подтягиваем новых пиров откуда можем
        -- для каждого блока решаем, откуда брать:
        --  shuffle (white-list) <> shuffle (black-list)
        --
        --
        let knownPeers = [p1]

        fix \next -> do

          blkHash <- liftIO $ atomically $ TBQ.readTBQueue blkQ

          -- TODO: check is this block is already here
          --       maybe emit event to continue -> parse/seek for content

          -- TODO: убивать нотификации, если блок скачан или что-то с ним еще
          --       приключилось
          --
          --       добавляем сюда экшоны на почистить:
          --       добавили нотификацию --- экшон на
          --       почистить нотификацию
          --
          --       добавили еще какую парашу -- экшон на
          --       её очистку
          --
          --       у каждого экшона - дедлайн
          --       и там процесс, который берёт тех, у кого дедлайн
          --       истёк и вызывает их
          --       ?

          addBlockReadyEventNotify ev0 blkHash $ \h -> do
            debug $ "DOWNLOADED BLOCK" <+> pretty h <+> "NOW WHAT?"

          --  -- TODO: смотрим, что за блок
          --  --       если Merkle - то качаем рекурсивно
          --  --       если ссылка - то смотрим, что за ссылка
          --  --       проверяем пруфы
          --  --       качаем рекурсивно

          -- TODO: надо трекать, может блок-то и найден
          --       либо по всем пирам спросить

          addBlockSizeEventNotify ev0 blkHash $ \case
            (p, h, Just size) -> do
              coo <- genCookie (p,blkHash)
              let key = DownloadSessionKey (p, coo)
              let chusz = defChunkSize

              let new =   set sBlockChunkSize chusz
                        . set sBlockSize (fromIntegral size)
                          $ newBlockDownload blkHash

              update @Fake new key id
              request p (BlockChunks coo (BlockGetAllChunks @Fake blkHash chusz)) -- FIXME: nice construction
              liftIO $ print $ "DAVAI BLOCK!" <+> pretty h

            _ -> pure ()

          -- TODO: смотрим, может у нас уже есть block-size
          --       тогда ловим случайного пира, у которого оно есть
          --       и ставим на закачку

          -- КТО ПЕРВЫЙ ВСТАЛ ТОГО И ТАПКИ
          for_ knownPeers $ \who ->
            request who (GetBlockSize @Fake blkHash)

          next

      let peerz = p0Thread : [p1Thread]


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

