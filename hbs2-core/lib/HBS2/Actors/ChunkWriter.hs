{-# Language UndecidableInstances #-}
module HBS2.Actors.ChunkWriter
  ( ChunkWriter
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
import HBS2.Net.Proto.Sessions

import Control.Monad.Trans.Maybe
import Data.List qualified as L
import Data.Functor
import Data.Function
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.ByteString qualified as BS
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

import Control.Monad.Except
import Control.Monad
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar as TV
import Control.Concurrent.STM.TBQueue qualified as Q
import Control.Concurrent.STM.TSem qualified as Sem
import Control.Concurrent.STM.TSem (TSem)

import Data.Typeable
import Control.Concurrent.MVar as MVar

import Control.Concurrent.STM.TQueue qualified as Q0
import Control.Concurrent

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)


class ( Eq salt
      , Eq (Hash h)
      , Hashable salt
      , Hashable (Hash h)
      , Typeable salt
      , Typeable (Hash h)
      , Hashed h ByteString
      ) => ChunkKey salt h

instance ( Hashable salt
         , Typeable salt
         , Eq salt
         , Eq (Hash h)
         , Hashable (Hash h)
         , Typeable (Hash h)
         , Hashed h ByteString
         ) => ChunkKey salt h


data Chunk h = P (IntMap ByteString)
             | S (Hash h) ByteString


instance Hashed h ByteString => Monoid (Chunk h) where
  mempty = P mempty

instance Hashed h ByteString => Semigroup (Chunk h) where
  (<>) (P a) (P b) = P ( a <> b )

  (<>) (S _ s1) (S _ s2) = S h3 s3
    where
      s3 = s1 <> s2
      h3 = hashObject s3

  (<>) p@(P{}) (S _ s) = S h3 s3
    where
      (S _ s1)  = toS p
      s3 = s1 <> s
      h3 = hashObject s3

  (<>) (S _ s) p@(P{}) = S h3 s3
    where
      (S _ s1)  = toS p
      s3 = s <> s1
      h3 = hashObject s3

mkP :: Offset -> ByteString -> Chunk h
mkP o b = P (IntMap.singleton (fromIntegral o) b)

toS :: Hashed h ByteString => Chunk h -> Chunk h
toS s@(S{}) = s
toS (P xs) = S h s
  where
    s = mconcat $ IntMap.elems xs
    h = hashObject s

data ChunkWriter h m = forall a . ( MonadIO m
                                  , Storage a h ByteString m
                                  , Block ByteString ~ ByteString
                                  ) =>
  ChunkWriter
  { stopped      :: TVar Bool
  , pipeline     :: Pipeline IO ()
  , dir          :: FilePath
  , storage      :: a
  , perBlock     :: !(TVar (HashMap SKey (Chunk h)))
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

  running <- liftIO $ newTVarIO False

  pure $
    ChunkWriter
    { stopped = running
    , pipeline = pip
    , dir = d
    , storage  = s
    , perBlock = mt
    }


delBlock :: (MonadIO m, Pretty (Hash h))
         => ChunkWriter h IO -> SKey -> m ()

delBlock w  k = liftIO do
  let cache = perBlock w
  liftIO $ atomically $ TV.modifyTVar' cache $ HashMap.delete k

writeChunk :: ( ChunkKey salt h
              , MonadIO m
              , Pretty (Hash h)
              )
           => ChunkWriter h m
           -> salt
           -> Hash h
           -> Offset
           -> ByteString -> m ()

writeChunk = writeChunk2


getHash :: forall salt h m .
           ( ChunkKey salt h
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
               ( ChunkKey salt h
               , Hashed h ByteString
               , Block ByteString ~ ByteString
               , m ~ IO
               , Pretty (Hash h)
               )
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> m ()

commitBlock = commitBlock2

writeChunk2 :: (ChunkKey salt h, MonadIO m, Pretty (Hash h))
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> Offset
            -> ByteString -> m ()

writeChunk2  w salt h o !bs = do

  let cache = perBlock w
  let k = newSKey (salt, h)
  liftIO $ do
    atomically $ modifyTVar cache (HashMap.insertWith (<>) k (mkP o bs) )

getHash2 :: forall salt h m .
           ( ChunkKey salt h
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
  let k = newSKey (salt, h)
  chunk <- readTVarIO (perBlock w) <&> fmap toS . HashMap.lookup k
  case chunk of
    Just (S h1 _) -> pure (Just h1)
    _             -> pure Nothing


commitBlock2 :: forall salt h m .
               ( ChunkKey salt h
               , Hashed h ByteString
               , Block ByteString ~ ByteString
               , m ~ IO
               , Pretty (Hash h)
               )
            => ChunkWriter h m
            -> salt
            -> Hash h
            -> m ()

commitBlock2 w@(ChunkWriter {storage = stor}) salt h = do
  let k = newSKey (salt, h)
  chunk <- readTVarIO (perBlock w) <&> fmap toS . HashMap.lookup k

  case chunk of
    Just (S _ s) -> void $ putBlock stor s >> delBlock w k
    _            -> pure () -- FIXME: error


