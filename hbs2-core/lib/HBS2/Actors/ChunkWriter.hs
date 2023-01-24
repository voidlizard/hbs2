module HBS2.Actors.ChunkWriter
  ( ChunkWriter
  , ChunkId
  , newChunkWriterIO
  , runChunkWriter
  , stopChunkWriter
  , newBlock
  , delBlock
  , commitBlock
  , writeChunk
  , getHash
  , blocksInProcess
  ) where

import HBS2.Prelude
import HBS2.Actors
import HBS2.Hash
import HBS2.Storage
import HBS2.Defaults
import HBS2.Clock

import Data.Functor
import Data.Function
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
-- import Data.Cache (Cache)
-- import Data.Cache qualified as Cache
import Data.Foldable
import Data.Traversable
import Data.Hashable (hash)
import Data.Maybe
import Data.Word
import Prettyprinter
import System.Directory
import System.FilePath
import System.IO.Error
import System.IO
import System.IO.Temp

import Control.Concurrent.Async

import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue qualified as Q
import Control.Concurrent.STM.TSem qualified as Sem
import Control.Concurrent.STM.TSem (TSem)

import Control.Concurrent.STM.TQueue qualified as Q0
import Control.Concurrent
--
--
--TODO: cache file handles

newtype ChunkId = ChunkId FilePath
                  deriving newtype (IsString)
                  deriving stock (Eq,Ord,Show)

data ChunkWriter h m = forall a . ( MonadIO m
                                  , Storage a h ByteString m
                                  , Block ByteString ~ ByteString
                                  ) =>
  ChunkWriter
  { stopped  :: TVar Bool
  , pipeline :: Pipeline m ()
  , dir      :: FilePath
  , storage  :: a
  , perBlock :: Cache FilePath (TQueue (Handle -> IO ()))
  , semFlush :: Cache FilePath TSem
  }


blocksInProcess :: MonadIO m => ChunkWriter h m -> m Int
blocksInProcess cw = liftIO $ Cache.purgeExpired cache >> Cache.size cache
  where
   cache = perBlock cw

runChunkWriter :: forall h m . ( Eq (Hash h)
                                , Hashable (Hash h)
                                , MonadIO m )
                => ChunkWriter h m -> m ()

runChunkWriter = runChunkWriter2

runChunkWriter1 :: forall h m . ( Eq (Hash h)
                                , Hashable (Hash h)
                                , MonadIO m )
                => ChunkWriter h m -> m ()

runChunkWriter1 w = do
  liftIO $ createDirectoryIfMissing True ( dir w )
  runPipeline (pipeline w)


runChunkWriter2 :: forall h m . ( Eq (Hash h)
                                , Hashable (Hash h)
                                , MonadIO m )
                => ChunkWriter h m -> m ()

runChunkWriter2 w = do
  liftIO $ createDirectoryIfMissing True ( dir w )
  let cache = perBlock w
  fix \next -> do
    -- kks  <- liftIO $ take 1 <$> Cache.keys cache
    -- liftIO $ for_ kks $ \h -> flush w h

    -- pause ( 1 :: Timeout 'Seconds )
    -- yield
    -- next

    stop <- liftIO $ readTVarIO (stopped w)

    if stop then do
      ks <- liftIO $ take 100  <$> Cache.keys cache
      liftIO $ for_ ks $ \k -> flush w k
    else do
      ks <- liftIO $ Cache.keys cache

      amount <- for ks $ \k -> flush w k

      if (sum amount == 0) then do
        pause ( 0.5 :: Timeout 'Seconds )
      else do
        liftIO $ print ("flushed:" <+> pretty (sum amount))


stopChunkWriter :: MonadIO m => ChunkWriter h m -> m ()
stopChunkWriter w = do
  liftIO $ atomically $ writeTVar (stopped w) True

stopChunkWriter1 :: MonadIO m => ChunkWriter h m -> m ()
stopChunkWriter1 w = do
  let cache = perBlock w
  stopPipeline ( pipeline w )

newChunkWriterIO :: forall h a m . ( Key h ~ Hash h, h ~ HbSync
                                   , Storage a h ByteString m
                                   , Block ByteString ~ ByteString
                                   , MonadIO m
                                   )
                 => a
                 -> Maybe FilePath
                 -> m (ChunkWriter h m)

newChunkWriterIO s tmp = do
  pip <- newPipeline defChunkWriterQ

  def  <- liftIO $ getXdgDirectory XdgData (defStorePath  </> "temp-chunks")
  let d  = fromMaybe def tmp

  mt <- liftIO $ Cache.newCache Nothing
  sem <- liftIO $ Cache.newCache Nothing

  running <- liftIO $ newTVarIO False

  pure $
    ChunkWriter
    { stopped = running
    , pipeline = pip
    , dir = d
    , storage  = s
    , perBlock = mt
    , semFlush = sem
    }

makeFileName :: (Hashable salt, Pretty (Hash h)) => ChunkWriter h m -> salt -> Hash h -> FilePath
makeFileName w salt h = dir w </> suff
  where
    suff = show $ pretty (fromIntegral (hash salt) :: Word32) <> "@" <> pretty h

-- TODO: check uniqueness
newBlock :: ( MonadIO m
            , Hashable salt
            , Pretty (Hash h)
            )
         => ChunkWriter h m
         -> salt
         -> Hash h
         -> Size -> m ()

newBlock w salt h size = liftIO do
  withBinaryFile fn ReadWriteMode (`hSetFileSize` fromIntegral size)
  where
    fn = makeFileName w salt h

delBlock :: (Hashable salt, MonadIO m, Pretty (Hash h))
         => ChunkWriter h m -> salt -> Hash h -> m ()

delBlock w salt h = liftIO do
  void $ tryJust (guard . isDoesNotExistError) (removeFile fn)
  where
    fn = makeFileName w salt h

writeChunk :: ( Hashable salt
              , MonadIO m
              , Pretty (Hash h)
              , Hashable (Hash h), Eq (Hash h)
              )
           => ChunkWriter h m
           -> salt
           -> Hash h
           -> Offset
           -> ByteString -> m ()

writeChunk = writeChunk2


getHash :: forall salt h m .
           ( Hashable salt
           , Hashed h ByteString
           , MonadIO m
           , Block ByteString ~ ByteString
           , Pretty (Hash h)
           , Hashable (Hash h), Eq (Hash h)
           )
         => ChunkWriter h m
         -> salt
         -> Hash h
         -> m (Hash h)

getHash = getHash2


commitBlock :: forall salt h m .
               ( Hashable salt
               , Hashed h ByteString
               , Block ByteString ~ ByteString
               , MonadIO m
               , Pretty (Hash h)
               , Hashable (Hash h), Eq (Hash h)
               )
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> m ()

commitBlock = commitBlock2



writeChunk1 :: (Hashable salt, MonadIO m, Pretty (Hash h))
           => ChunkWriter h m
           -> salt
           -> Hash h
           -> Offset
           -> ByteString -> m ()

writeChunk1 w salt h o bs = addJob (pipeline w) $ liftIO do
-- writeChunk w salt h o bs = liftIO do
  -- print $ "writeChunk:" <+> pretty fn
  withBinaryFile fn ReadWriteMode $ \fh -> do
    hSeek fh AbsoluteSeek (fromIntegral o)
    B.hPutStr fh bs
    hFlush fh

  where
    fn = makeFileName w salt h

writeChunk2 :: (Hashable salt, MonadIO m, Pretty (Hash h), Hashable (Hash h), Eq (Hash h))
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> Offset
            -> ByteString -> m ()

writeChunk2  w salt h o bs = do

  let cache = perBlock w

  liftIO $ do
    q <- Cache.fetchWithCache cache fn $ const Q0.newTQueueIO
    atomically $ Q0.writeTQueue q $ \fh -> do
      -- withBinaryFile fn ReadWriteMode $ \fh -> do
        hSeek fh AbsoluteSeek (fromIntegral o)
        B.hPutStr fh bs
        -- hFlush fh

  where
    fn = makeFileName w salt h


-- Blocking!
-- we need to write last chunk before this will happen
-- FIXME: incremental calculation,
--        streaming, blah-blah
getHash1 :: forall salt h m .
           ( Hashable salt
           , Hashed h ByteString
           , MonadIO m
           , Block ByteString ~ ByteString
           , Pretty (Hash h)
           , Hashable (Hash h), Eq (Hash h)
           )
         => ChunkWriter h m
         -> salt
         -> Hash h
         -> m (Hash h)

getHash1 w salt h = liftIO do

  q <- Q.newTBQueueIO 1

  addJob (pipeline w) $ liftIO do
    h1 <- hashObject @h <$> B.readFile fn
    atomically $ Q.writeTBQueue q h1

  atomically $ Q.readTBQueue q

  where
    fn = makeFileName w salt h


flush w fn = do
  let cache = perBlock w
  let scache = semFlush w
  liftIO $ do
    q <- Cache.fetchWithCache cache fn $ const Q0.newTQueueIO
    s <- Cache.fetchWithCache scache fn $ const (atomically $ Sem.newTSem 2)

    atomically $ Sem.waitTSem  s

    Cache.delete cache fn

    flushed <- atomically (Q0.flushTQueue q)

    liftIO $ do

      -- withBinaryFile fn ReadWriteMode $ \fh -> do
      withFile fn ReadWriteMode $ \fh -> do
          for_ flushed $ \f -> f fh

    atomically $ Sem.signalTSem s

    pure (length flushed)


-- Blocking!
-- we need to write last chunk before this will happen
-- FIXME: incremental calculation,
--        streaming, blah-blah
getHash2 :: forall salt h m .
           ( Hashable salt
           , Hashed h ByteString
           , MonadIO m
           , Block ByteString ~ ByteString
           , Pretty (Hash h)
          , Hashable (Hash h), Eq (Hash h)
           )
         => ChunkWriter h m
         -> salt
         -> Hash h
         -> m (Hash h)

getHash2 w salt h = do
  flush w fn
  h1 <- liftIO $ hashObject @h <$> B.readFile fn
  pure h1

  where
    fn = makeFileName w salt h


commitBlock1 :: forall salt h m .
               ( Hashable salt
               , Hashed h ByteString
               , Block ByteString ~ ByteString
               , MonadIO m
               , Pretty (Hash h)
               )
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> m ()

commitBlock1 w@(ChunkWriter {storage = stor}) salt h = do
  q <- liftIO $ Q.newTBQueueIO 1

  addJob (pipeline w)  (liftIO $ B.readFile fn >>= atomically . Q.writeTBQueue q)

  s <- liftIO $ atomically $ Q.readTBQueue q

  void $ putBlock stor s

  delBlock w salt h

  where
    fn = makeFileName w salt h



commitBlock2 :: forall salt h m .
               ( Hashable salt
               , Hashed h ByteString
               , Block ByteString ~ ByteString
               , MonadIO m
               , Pretty (Hash h)
               , Hashable (Hash h), Eq (Hash h)
               )
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> m ()

commitBlock2 w@(ChunkWriter {storage = stor}) salt h = do
  let cache = perBlock w
  let scache = semFlush w
  flush w fn
  s <- liftIO $ B.readFile fn
  void $ putBlock stor s
  delBlock w salt h
  liftIO $ Cache.delete cache fn
  liftIO $ Cache.delete scache fn

  where
    fn = makeFileName w salt h


