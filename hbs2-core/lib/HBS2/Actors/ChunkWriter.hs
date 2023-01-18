{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
module HBS2.Actors.ChunkWriter
  ( ChunkWriter
  , ChunkId
  , newChunkWriterIO
  , runChunkWriter
  , stopChunkWriter
  , newBlock
  , delBlock
  , writeChunk
  , getHash
  ) where

import HBS2.Prelude
import HBS2.Actors
import HBS2.Hash
import HBS2.Storage
import HBS2.Defaults

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
-- import Data.Cache (Cache)
-- import Data.Cache qualified as Cache
import Data.Hashable (hash)
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.FilePath
import System.IO.Error
import System.IO

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue qualified as Q

-- TODO: cache file handles

newtype ChunkId = ChunkId FilePath
                  deriving newtype (IsString)
                  deriving stock (Eq,Ord,Show)

data ChunkWriter h m =
  ChunkWriter
  { _pipeline :: Pipeline IO ()
  , _dir      :: FilePath
  ,  storage  :: forall a . (Key h ~ Hash h, Storage a h ByteString m) => a
  }

makeLenses 'ChunkWriter

runChunkWriter :: MonadIO m => ChunkWriter h m -> m ()
runChunkWriter w = do
  liftIO $ createDirectoryIfMissing True ( w ^. dir )
  liftIO $ runPipeline ( w ^. pipeline)

stopChunkWriter :: MonadIO m => ChunkWriter h m -> m ()
stopChunkWriter w = liftIO $ stopPipeline ( w ^. pipeline )

newChunkWriterIO :: Maybe FilePath -> IO (ChunkWriter h m)
newChunkWriterIO  tmp = do
  pip <- newPipeline defChunkWriterQ

  def  <- getXdgDirectory XdgData (defStorePath  </> "temp-chunks")

  let d  = fromMaybe def tmp

  pure $
    ChunkWriter
    { _pipeline = pip
    , _dir = d
    ,  storage  = undefined
    }

makeFileName :: (Hashable salt, Pretty (Hash h)) => ChunkWriter h m -> salt -> Hash h -> FilePath
makeFileName w salt h = (w ^. dir) </> suff
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

writeChunk :: (Hashable salt, MonadIO m, Pretty (Hash h))
           => ChunkWriter h m
           -> salt
           -> Hash h
           -> Offset
           -> ByteString -> m ()

writeChunk w salt h o bs = addJob (w ^. pipeline) $ liftIO do
  withBinaryFile fn ReadWriteMode $ \fh -> do
    hSeek fh AbsoluteSeek (fromIntegral o)
    B.hPutStr fh bs
    hFlush fh

  where
    fn = makeFileName w salt h

-- Blocking!
-- we need to write last chunk before this will happen
-- FIXME: incremental calculation,
--        streaming, blah-blah
getHash :: forall salt h m .
           ( Hashable salt
           , Hashed h ByteString
           , MonadIO m
           , Pretty (Hash h)
           )
         => ChunkWriter h m
         -> salt
         -> Hash h
         -> m (Hash h)

getHash w salt h = liftIO do

  q <- Q.newTBQueueIO 1

  addJob (w ^. pipeline) do
    h1 <- hashObject @h <$> B.readFile fn
    atomically $ Q.writeTBQueue q h1

  atomically $ Q.readTBQueue q

  where
    fn = makeFileName w salt h

