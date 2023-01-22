{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Actors.ChunkWriter
import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Events
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
import HBS2.Defaults

import Test.Tasty.HUnit

import Codec.Serialise hiding (encode,decode)
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.Default
import Data.Foldable hiding (find)
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
  deriving stock (Typeable)

makeLenses 'BlockDownload

newBlockDownload :: Hash HbSync -> BlockDownload
newBlockDownload h = BlockDownload h 0 0 0 0

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

-- FIXME: 3 is for debug only!
instance Expires (EventKey Fake (BlockSize Fake)) where
  expiresIn _  = 3

instance HasProtocol Fake (BlockChunks Fake) where
  type instance ProtocolId (BlockChunks Fake) = 2
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


type instance SessionData e (BlockSize e) = BlockSizeSession e
type instance SessionData e (BlockChunks e) = BlockDownload

newtype instance SessionKey e (BlockChunks e) =
  DownloadSessionKey (Peer e, Cookie e)
  deriving stock (Generic,Typeable)

newtype BlockSizeSession e =
  BlockSizeSession
  { _bsBlockSizes :: Map (Peer e) Size
  }

makeLenses 'BlockSizeSession

instance Ord (Peer e) => Default (BlockSizeSession e) where
  def = BlockSizeSession mempty

deriving stock instance Show (BlockSizeSession Fake)

deriving newtype instance Hashable (SessionKey Fake (BlockChunks Fake))
deriving stock instance Eq (SessionKey Fake (BlockChunks Fake))

runTestPeer :: Peer Fake
            -> (SimpleStorage HbSync -> ChunkWriter HbSync IO  -> IO ())
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

  zu stor cww

  simpleStorageStop stor
  stopChunkWriter cww

  mapM_ cancel [sw,cw]


handleBlockInfo :: forall e m . ( MonadIO m
                                , Sessions e (BlockSize e) m
                                , Default (SessionData e (BlockSize e))
                                , Ord (Peer e)
                                , Pretty (Peer e)
                                -- , EventEmitter e (BlockSize e) m
                                )

                => (Peer e, Hash HbSync, Maybe Integer)
                -> m ()

handleBlockInfo (p, h, sz') = do
   maybe1 sz' (pure ()) $ \sz -> do
    let bsz = fromIntegral sz
    update @e def (BlockSizeKey h) (over bsBlockSizes (Map.insert p bsz))

blockDownloadLoop :: forall e . ( HasProtocol e (BlockSize e)
                                , HasProtocol e (BlockChunks e)
                                , Request e (BlockSize e) (PeerM e IO)
                                , Request e (BlockChunks e) (PeerM e IO)
                                , EventListener e (BlockSize e) (PeerM e IO)
                                , Sessions e (BlockSize e) (PeerM e IO)
                                , Sessions e (BlockChunks e) (PeerM e IO)
                                , Num (Peer e)
                                -- , Ord (Peer e)
                                ) =>  PeerM e IO ()
blockDownloadLoop = do

  let blks = [ "5KP4vM6RuEX6RA1ywthBMqZV5UJDLANC17UrF6zuWdRt"
             , "81JeD7LNR6Q7RYfyWBxfjJn1RsWzvegkUXae6FUNgrMZ"
             , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
             ]

  for_ blks $ \h -> do

    debug $ "subscribing to" <+> pretty h

    subscribe @e @(BlockSize e) (BlockSizeEventKey h) $ \(BlockSizeEvent (p,h,s)) -> do
      debug $  "can't believe this shit works" <+> pretty h
      coo <- genCookie (p,h)
      let key = DownloadSessionKey (p, coo)
      let chusz = defChunkSize
      let new =   set sBlockChunkSize chusz
                . set sBlockSize (fromIntegral s)
                  $ newBlockDownload h

      update @e new key id
      request p (BlockChunks coo (BlockGetAllChunks @e h chusz)) -- FIXME: nicer construction

    request 1 (GetBlockSize @e h)

  fix \next -> do
    liftIO $ print "piu!"

    pause ( 0.85 :: Timeout 'Seconds )
    next

-- NOTE: this is an adapter for a ResponseM monad
--       because response is working in ResponseM monad (ha!)
--       So don't be confused with types
--
mkAdapter :: forall e m . ( m ~  PeerM e IO
                          , HasProtocol e (BlockChunks e)
                          , Hashable (SessionKey e (BlockChunks e))
                          , Sessions e (BlockChunks e) (ResponseM e m)
                          , EventEmitter e (BlockChunks e) m
                          , Pretty (Peer e)
                          )
          => ChunkWriter HbSync IO -> m (BlockChunksI e (ResponseM e m ))
mkAdapter cww = do
  storage <- getStorage
  pure $
    BlockChunksI
    { blkSize     = liftIO . hasBlock storage
    , blkChunk    = \h o s -> liftIO (getChunk storage h o s)
    , blkGetHash  = \c -> find (DownloadSessionKey @e c) (view sBlockHash)

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
              debug $ "BLOCK IS READY" <+> pretty h
              -- FIXME: return this event!
              -- lift $ runEngineM env $ emitBlockReadyEvent ev h -- TODO: fix this crazy shit

        when (written > mbSize * defBlockDownloadThreshold) $ do
          debug $ "SESSION DELETED BECAUSE THAT PEER IS JERK:" <+> pretty p
          lift $ expire cKey
          -- ЕСЛИ ТУТ ВИСЕТЬ ДОЛГО, ТО НАС МОЖНО ДИДОСИТЬ,
          -- ПОСЫЛАЯ НЕ ВСЕ БЛОКИ ЧАНКА ИЛИ ПОСЫЛАЯ ОТДЕЛЬНЫЕ
          -- ЧАНКИ ПО МНОГУ РАЗ. А МЫ БУДЕМ ХЭШИ СЧИТАТЬ.
          -- ТАК НЕ ПОЙДЕТ
          -- ТАК ЧТО ТУТ ЖДЁМ, ДОПУСТИМ 2*mbSize  и отваливаемся
    }


main :: IO ()
main = do
  hSetBuffering stderr LineBuffering

  void $ race (pause (10 :: Timeout 'Seconds)) $ do

    fake <- newFakeP2P True <&> Fabriq

    let (p0:ps) = [0..1] :: [Peer Fake]

    -- others
    others <- forM ps $ \p -> async $ runTestPeer p $ \s cw  -> do
                let findBlk = hasBlock s

                let size = 1024*1024

                let blk = B8.concat [ fromString (take 1 $ show x)
                                    | x <- replicate size (fromIntegral p :: Int)
                                    ]

                root <- putAsMerkle s blk

                debug $ "I'm" <+> pretty p <+> pretty root

                runPeerM (AnyStorage s) fake p $ do
                  adapter <- mkAdapter cw
                  runProto @Fake
                    [ makeResponse (blockSizeProto findBlk dontHandle)
                    , makeResponse (blockChunksProto adapter)
                    ]

    our <- async $ runTestPeer p0 $ \s cw -> do
                let blk = hasBlock s
                runPeerM (AnyStorage s) fake p0 $ do
                  adapter <- mkAdapter cw
                  env <- ask
                  as <- liftIO $ async $ withPeerM env blockDownloadLoop

                  runProto @Fake
                    [ makeResponse (blockSizeProto blk handleBlockInfo)
                    , makeResponse (blockChunksProto adapter)
                    ]

                  liftIO $ cancel as

    pause ( 8 :: Timeout 'Seconds)

    mapM_ cancel (our:others)

    (_, e) <- waitAnyCatchCancel (our:others)

    debug (pretty $ show e)
    debug "we're done"
    assertBool "success" True
    exitSuccess

  assertBool "failed" False



