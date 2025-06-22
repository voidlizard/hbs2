{-# Language MultiWayIf #-}
{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ2 where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Storage
import HBS2.Misc.PrettyStuff
import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.NCQ
import HBS2.Data.Log.Structured.SD

import Data.Config.Suckless.System
import Data.Config.Suckless.Script hiding (void)

import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Lazy as ZstdL
import Codec.Compression.Zstd.Streaming qualified as ZstdS
import Codec.Compression.Zstd.Streaming (Result(..))

import Control.Applicative
import Data.ByteString.Builder
import Network.ByteOrder qualified as N
import Data.HashMap.Strict (HashMap)
import Control.Monad.Except
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Ord (Down(..),comparing)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TSem
import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.IntSet qualified as IntSet
import Data.IntSet (IntSet)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..), (|>))
import Data.List qualified as List
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isDigit)
import Data.Fixed
import Data.Coerce
import Data.Word
import Data.Either
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Int
import Data.Vector qualified as V
import Data.Vector (Vector, (!))
import Lens.Micro.Platform
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import System.Directory (makeAbsolute)
import System.FilePath.Posix
import System.Posix.Fcntl
import System.Posix.Files qualified as Posix
import System.Posix.IO as PosixBase
import System.Posix.Types as Posix
import System.Posix.IO.ByteString as Posix
import System.Posix.Unistd
import System.Posix.Files ( getFileStatus
                          , modificationTimeHiRes
                          , setFileTimesHiRes
                          , getFdStatus
                          , FileStatus(..)
                          , setFileMode
                          )
import System.Posix.Files qualified as PFS
import System.IO.Error (catchIOError)
import System.IO.MMap as MMap
import System.IO.Temp (emptyTempFile)
import System.Mem
-- import Foreign.Ptr
-- import Foreign di
import qualified Data.ByteString.Internal as BSI
import Streaming.Prelude qualified as S

import UnliftIO
import UnliftIO.Concurrent(getNumCapabilities)
import UnliftIO.IO.File

import System.FileLock as FL


data NCQEntry =
    NCQEntryNew          Int ByteString
  | NCQEntryJustWritten  Int Fd ByteString
  | NCQEntrySynced       Fd  Word64

type Shard = TVar (HashMap HashRef (TVar NCQEntry))

data NCQStorage2 =
  NCQStorage2
  { ncqFsync          :: Int
  , ncqWriteQLen      :: Int
  , ncqWriteBlock     :: Int
  , ncqMemTable       :: Vector  Shard
  , ncqWriteSem       :: TSem
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqStorageStopReq :: TVar Bool
  , ncqStorageSyncReq :: TVar Bool
  , ncqSyncNo         :: TVar Int
  } deriving (Generic)

ncqStorageOpen2 :: MonadIO m => FilePath -> (NCQStorage2 -> NCQStorage2)-> m NCQStorage2
ncqStorageOpen2 fp upd = do
  let ncqFsync      = 16 * 1024^2
  let ncqWriteQLen  = 1024 * 16
  let ncqWriteBlock = 4096 * 4
  cap               <- getNumCapabilities <&> fromIntegral
  ncqWriteQ         <- newTVarIO mempty
  ncqWriteSem       <- atomically $ newTSem 16 -- (fromIntegral cap)
  ncqMemTable       <- V.fromList <$> replicateM (max 2 (cap `div` 2)) (newTVarIO mempty)
  ncqStorageStopReq <- newTVarIO False
  ncqStorageSyncReq <- newTVarIO False
  ncqSyncNo         <- newTVarIO 0
  pure $ NCQStorage2{..} & upd

ncqStorageStop2 :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqStorageStop2 NCQStorage2{..} = do
  atomically $ writeTVar ncqStorageStopReq True

ncqStorageSync2 :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqStorageSync2 NCQStorage2{..} = do
  atomically $ writeTVar ncqStorageSyncReq True

ncqShardIdx :: NCQStorage2 -> HashRef -> Int
ncqShardIdx NCQStorage2{..} h =
  fromIntegral (BS.head (coerce h)) `mod` V.length ncqMemTable

ncqGetShard :: NCQStorage2 -> HashRef -> Shard
ncqGetShard ncq@NCQStorage2{..} h = ncqMemTable ! ncqShardIdx ncq h

ncqLookupEntrySTM :: NCQStorage2 -> HashRef -> STM (Maybe (NCQEntry, TVar NCQEntry))
ncqLookupEntrySTM ncq h = do
  readTVar (ncqGetShard ncq h)
    <&> HM.lookup h
    >>= \case
      Nothing -> pure Nothing
      Just tv -> do
        v <- readTVar tv
        pure $ Just (v, tv)

ncqPutBS :: MonadUnliftIO m => NCQStorage2 -> ByteString -> m HashRef
ncqPutBS ncq@NCQStorage2{..} bs = do
  let h = HashRef (hashObject @HbSync bs)
  atomically do
    waitTSem ncqWriteSem
    stop <- readTVar ncqStorageStopReq
    filled <- readTVar ncqWriteQ <&> Seq.length

    when (not stop && filled > ncqWriteQLen) STM.retry

    n <- readTVar ncqSyncNo
    ncqAlterEntrySTM ncq h $ \case
      Just e  -> Just e
      Nothing -> Just (NCQEntryNew n bs)
    modifyTVar' ncqWriteQ (|> h)
    signalTSem ncqWriteSem

  pure h

ncqLookupEntry :: MonadUnliftIO m => NCQStorage2 -> HashRef -> m (Maybe NCQEntry)
ncqLookupEntry sto hash = atomically (ncqLookupEntrySTM sto hash) <&> fmap fst

ncqAlterEntrySTM :: NCQStorage2 -> HashRef -> (Maybe NCQEntry -> Maybe NCQEntry) -> STM ()
ncqAlterEntrySTM ncq h alterFn = do
  let shard = ncqGetShard ncq h
  readTVar shard <&> HM.lookup h >>= \case
    Just tve  -> do
      e  <- readTVar tve
      case alterFn (Just e) of
        Nothing -> modifyTVar' shard (HM.delete h)
        Just e' -> writeTVar tve e'

    Nothing -> case alterFn Nothing of
      Nothing -> modifyTVar' shard (HM.delete h)
      Just e  -> do
        tve <- newTVar e
        modifyTVar' shard (HM.insert h tve)

ncqStorageRun2 :: forall m . MonadUnliftIO m
               => NCQStorage2
               -> m ()
ncqStorageRun2 ncq@NCQStorage2{..} = flip runContT pure $ callCC \exit -> do

  jobQ <- newTQueueIO

  fname <-  liftIO $ emptyTempFile "." "datafile-.data"

  let flags = defaultFileFlags { exclusive = False, creat = Just 0o666 }
  fh0 <- liftIO (PosixBase.openFd fname  Posix.ReadWrite flags)

  ContT $ bracket none $ const do
    liftIO $ closeFd fh0

  jobz <- ContT $ withAsync $ forever (atomically (readTQueue jobQ) >>= join)
  link jobz

  flip fix (fh0,0) $ \loop (fh,w) -> do

    sync <- readTVarIO ncqStorageSyncReq

    when (w > ncqFsync || sync) do
      liftIO (fileSynchronise fh)
      atomically do
        writeTVar   ncqStorageSyncReq False
        modifyTVar' ncqSyncNo succ
      loop (fh,0)

    chunk <- atomically do
      stop  <- readTVar ncqStorageStopReq
      sy    <- readTVar ncqStorageSyncReq
      chunk <- stateTVar ncqWriteQ (Seq.splitAt ncqWriteBlock)

      if | Seq.null chunk && stop             -> pure $ Left ()
         | Seq.null chunk && not (stop || sy) -> STM.retry
         | otherwise                          -> pure $ Right chunk

    case chunk of
      Left{} -> exit ()
      Right chu -> do
        ws <- for chu $ \h -> do
                atomically (ncqLookupEntrySTM ncq h) >>= \case
                  Nothing -> pure 0
                  Just (r,t)  -> lift (appendEntry fh h r)

        loop (fh, w + sum ws)

  where

    appendEntry :: Fd -> HashRef -> NCQEntry -> m Int

    appendEntry fh h (NCQEntryNew _ bs) = do
      let ss = N.bytestring32 (32 + fromIntegral (BS.length bs))
      let section = ss <> coerce h <> bs
      liftIO (Posix.fdWrite fh section) <&> fromIntegral

    appendEntry fh h _ = do
      pure 0

    {-# INLINE appendEntry #-}


