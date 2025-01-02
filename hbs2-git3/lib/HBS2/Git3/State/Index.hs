module HBS2.Git3.State.Index where

import HBS2.Git3.Prelude
import HBS2.System.Dir
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Git3.State.Types

import HBS2.Data.Log.Structured

import Data.ByteString qualified as BS
import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as L
import Network.ByteOrder qualified as N
import System.IO.Temp as Temp
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM

import Codec.Compression.Zstd.Lazy qualified as ZstdL
import Streaming.Prelude qualified as S
import Streaming hiding (run,chunksOf)

import UnliftIO
import UnliftIO.IO.File qualified as UIO



readLogFileLBS :: forall opts m . ( MonadIO m, ReadLogOpts opts, BytesReader m )
               => opts
               -> ( GitHash -> Int -> ByteString -> m () )
               -> m Int

readLogFileLBS _ action = flip fix 0 \go n -> do
  done <- noBytesLeft
  if done then pure n
    else do
      ssize <- readBytesMaybe 4
                  >>= orThrow SomeReadLogError
                  <&> fromIntegral . N.word32 . LBS.toStrict

      hash  <- readBytesMaybe 20
                  >>= orThrow SomeReadLogError
                  <&> GitHash . BS.copy . LBS.toStrict

      sdata <- readBytesMaybe ( ssize - 20 )
                  >>= orThrow SomeReadLogError

      void $ action hash (fromIntegral ssize) sdata
      go (succ n)

indexPath :: forall m . ( Git3Perks m
                        , MonadReader Git3Env m
                        ) => m FilePath
indexPath = do
  reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet
  getStatePath (AsBase58 reflog) <&> (</> "index")

listObjectIndexFiles :: forall m . ( Git3Perks m
                                   , MonadReader Git3Env m
                                   ) => m [(FilePath, Natural)]

listObjectIndexFiles = do
  path <- indexPath
  dirFiles path
    <&> filter ( ("objects*.idx" ?==) .  takeFileName )
    >>= \fs -> for fs $ \f -> do
           z <- fileSize f  <&> fromIntegral
           pure (f,z)



enumEntries :: forall m . ( Git3Perks m
                          , MonadReader Git3Env m
                          ) => ( BS.ByteString -> m () ) -> m ()

enumEntries action = do
  files <- listObjectIndexFiles <&> fmap fst
  forConcurrently_ files $ \f -> do
    bs <- liftIO $ mmapFileByteString f Nothing
    scanBS bs action

startReflogIndexQueryQueue :: forall a m . ( Git3Perks m
                                           , MonadReader Git3Env m
                                           , HasClientAPI PeerAPI UNIX m
                                           , HasClientAPI RefLogAPI UNIX m
                                           , HasStorage m
                                           )
                       => TQueue (BS.ByteString, BS.ByteString -> a, TMVar (Maybe a))
                       -> m  ()

startReflogIndexQueryQueue rq = flip runContT pure do
  files <- lift $ listObjectIndexFiles <&> fmap fst

  -- один файл - не более, чем один поток
  -- мапим файлы
  -- возвращаем функцию запроса?
  -- для каждого файла -- мы создаём отдельную очередь,
  -- нам надо искать во всех файлах

  mmaped <- liftIO $ for files (liftIO . flip mmapFileByteString Nothing)

  r <- newTVarIO (mempty :: HashMap N.ByteString N.ByteString)

  -- FIXME: may-explode
  for_ mmaped $ \bs -> do
    scanBS bs $ \segment -> do
      let ha = BS.take 20 segment & coerce
      atomically $ modifyTVar r (HM.insert ha segment)

  forever do
    (s, f, answ) <- atomically $ readTQueue rq
    found <- readTVarIO r <&> HM.lookup s

    atomically do
      case found of
        Nothing -> writeTMVar answ Nothing
        Just x  -> writeTMVar answ (Just (f x))

  -- forever $ liftIO do
  --   (s, f, answ) <- atomically $ readTQueue rq

  --   found <- forConcurrently mmaped $ \bs -> runMaybeT do
  --              -- FIXME: size-hardcodes
  --              w <- binarySearchBS 56 ( BS.take 20 . BS.drop 4 ) s bs
  --                     >>= toMPlus

  --              let v = BS.drop ( w * 56 ) bs

  --              pure $ f v

  --   let what = headMay (catMaybes found)
  --   atomically $ writeTMVar answ what

writeReflogIndex :: forall m . ( Git3Perks m
                               , MonadReader Git3Env m
                               , HasClientAPI PeerAPI UNIX m
                               , HasClientAPI RefLogAPI UNIX m
                               , HasStorage m
                               ) => m ()
writeReflogIndex = do

    reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet

    api <- getClientAPI @RefLogAPI @UNIX

    sto <- getStorage

    flip runContT pure do

      what' <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 2) api reflog
                >>= orThrow Git3RpcTimeout

      what <- ContT $ maybe1 what' none

      idxPath <- lift indexPath
      mkdir idxPath

      notice $ "STATE" <+> pretty idxPath

      sink <- S.toList_ do
        walkMerkle (coerce what) (getBlock sto) $ \case
          Left{} -> throwIO MissedBlockError
          Right (hs :: [HashRef]) -> do
            for_ hs $ \h -> void $ runMaybeT do

              tx <- getBlock sto (coerce h)
                       >>= toMPlus

              RefLogUpdate{..} <- deserialiseOrFail @(RefLogUpdate L4Proto) tx
                                    & toMPlus

              AnnotatedHashRef _ href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict _refLogUpdData)
                                           & toMPlus

              -- FIXME: error logging
              lbs <- liftIO (runExceptT (getTreeContents sto href))
                       >>= orThrow MissedBlockError

              pieces <- S.toList_ do
                void $ runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \o s _ -> do
                  lift $ S.yield o

              lift $ S.yield (h, pieces)

      liftIO $ forConcurrently_ sink $ \(tx, pieces) -> do
        idxName <- emptyTempFile idxPath "objects-.idx"
        let ss = L.sort pieces
        UIO.withBinaryFileAtomic idxName WriteMode $ \wh -> do
          for_ ss $ \sha1 -> do
            let key   = coerce @_ @N.ByteString sha1
            let value = coerce @_ @N.ByteString tx
            -- notice $ pretty sha1 <+> pretty tx
            writeSection ( LBS.fromChunks [key,value] ) (LBS.hPutStr wh)


