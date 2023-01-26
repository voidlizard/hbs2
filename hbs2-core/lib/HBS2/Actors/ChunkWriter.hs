module HBS2.Actors.ChunkWriter
  ( ChunkWriter
  , ChunkId
  , newChunkWriterIO
  , runChunkWriter
  , stopChunkWriter
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

import Control.Monad.Trans.Maybe
import Data.List qualified as L
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
import System.FileLock

import Control.Concurrent.Async

import Control.Monad
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar as TV
import Control.Concurrent.STM.TBQueue qualified as Q
import Control.Concurrent.STM.TSem qualified as Sem
import Control.Concurrent.STM.TSem (TSem)

import Control.Concurrent.STM.TQueue qualified as Q0
import Control.Concurrent

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

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
  { stopped     :: TVar Bool
  , pipeline    :: Pipeline IO ()
  , dir         :: FilePath
  , storage     :: a
  , perBlock    :: TVar (HashMap FilePath [Handle -> IO ()])
  , perBlockSem :: TVar (HashMap FilePath TSem)
  }


blocksInProcess :: MonadIO m => ChunkWriter h m -> m Int
blocksInProcess cw = do
  liftIO $ readTVarIO (perBlock cw) <&> HashMap.size

runChunkWriter :: forall h m . ( Eq (Hash h)
                                , Hashable (Hash h)
                                , MonadIO m )
                => ChunkWriter h IO -> m ()

runChunkWriter = runChunkWriter2


runChunkWriter2 :: forall h m . ( Eq (Hash h)
                                , Hashable (Hash h)
                                , MonadIO m )
                => ChunkWriter h IO -> m ()

runChunkWriter2 w = do
  liftIO $ createDirectoryIfMissing True ( dir w )
  let tv = perBlock w
  liftIO $ runPipeline (pipeline w)
  -- fix \next -> do
  --   keys <- liftIO $ readTVarIO tv <&> (L.take 20 . HashMap.keys)
  --   liftIO $ forConcurrently_ keys $ \f -> flush w f
  --   pause ( 1.00 :: Timeout 'Seconds)
  --   next

stopChunkWriter :: MonadIO m => ChunkWriter h m -> m ()
stopChunkWriter w = do
  liftIO $ atomically $ writeTVar (stopped w) True

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

  mt <- liftIO $ newTVarIO mempty
  mts <- liftIO $ newTVarIO mempty

  running <- liftIO $ newTVarIO False

  pure $
    ChunkWriter
    { stopped = running
    , pipeline = pip
    , dir = d
    , storage  = s
    , perBlock = mt
    , perBlockSem = mts
    }

makeFileName :: (Hashable salt, Pretty (Hash h)) => ChunkWriter h m -> salt -> Hash h -> FilePath
makeFileName w salt h = dir w </> suff
  where
    suff = show $ pretty (fromIntegral (hash salt) :: Word32) <> "@" <> pretty h

delBlock :: (Hashable salt, MonadIO m, Pretty (Hash h))
         => ChunkWriter h IO -> salt -> Hash h -> m ()

delBlock w salt h = liftIO do

  let cache = perBlock w
  let se   = perBlockSem w

  liftIO $ flush w fn

  liftIO $ atomically $ TV.modifyTVar' cache $ HashMap.delete fn
  liftIO $ atomically $ TV.modifyTVar' se $ HashMap.delete fn

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
           , m ~ IO
           , Block ByteString ~ ByteString
           , Pretty (Hash h)
           , Hashable (Hash h), Eq (Hash h)
           )
         => ChunkWriter h m
         -> salt
         -> Hash h
         -> m (Maybe (Hash h))

getHash = getHash2


commitBlock :: forall salt h m .
               ( Hashable salt
               , Hashed h ByteString
               , Block ByteString ~ ByteString
               , m ~ IO
               , Pretty (Hash h)
               , Hashable (Hash h), Eq (Hash h)
               )
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> m ()

commitBlock = commitBlock2

writeChunk2 :: (Hashable salt, MonadIO m, Pretty (Hash h), Hashable (Hash h), Eq (Hash h))
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> Offset
            -> ByteString -> m ()

writeChunk2  w salt h o bs = do

  let cache = perBlock w

  let action fh = do
        -- withBinaryFile fn ReadWriteMode $ \fh -> do
        hSeek fh AbsoluteSeek (fromIntegral o)
        B.hPutStr fh bs

  liftIO $ do
    atomically $ modifyTVar cache (HashMap.insertWith (<>) fn [action])

  where
    fn = makeFileName w salt h

flush :: ChunkWriter h IO -> FilePath -> IO ()
flush w fn = do
  let cache = perBlock w
  let pip = pipeline w

  liftIO $ do

    actions <- atomically $ stateTVar cache (\v -> (HashMap.lookup fn v, HashMap.delete fn v))

    q <- liftIO $ Q.newTBQueueIO 1

    addJob pip $ do

      as <- asyncBound $ do
        withBinaryFile fn ReadWriteMode $ \h -> do
          withFileLock fn Exclusive $ \_ -> do
              for_ (fromMaybe mempty actions) $ \f -> f h
      wait as

      void $ liftIO $ atomically $ Q.writeTBQueue q ()

    liftIO $ atomically $ Q.readTBQueue q


-- Blocking!
-- we need to write last chunk before this will happen
-- FIXME: incremental calculation,
--        streaming, blah-blah
getHash2 :: forall salt h m .
           ( Hashable salt
           , Hashed h ByteString
           , m ~ IO
           , Block ByteString ~ ByteString
           , Pretty (Hash h)
          , Hashable (Hash h), Eq (Hash h)
           )
         => ChunkWriter h IO
         -> salt
         -> Hash h
         -> m (Maybe (Hash h))

getHash2 w salt h = do
  flush w fn

  runMaybeT $ do
    res <- liftIO $ tryJust (guard . isDoesNotExistError)
                            ( B.readFile fn  >>= \s -> pure $ hashObject @h s )

    MaybeT $ pure $ either (const Nothing) Just res

  where
    fn = makeFileName w salt h


commitBlock2 :: forall salt h m .
               ( Hashable salt
               , Hashed h ByteString
               , Block ByteString ~ ByteString
               , m ~ IO
               , Pretty (Hash h)
               , Hashable (Hash h), Eq (Hash h)
               )
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> m ()

commitBlock2 w@(ChunkWriter {storage = stor}) salt h = do

  print "FLUSHING"

  flush w fn

  print "FLUSHED"

  res <- liftIO $ tryJust (guard . isDoesNotExistError)
                          ( B.readFile fn )

  case res of
    Left _ -> pure ()
    Right s -> do
      void $ putBlock stor s
      delBlock w salt h

  where
    fn = makeFileName w salt h


