{-# Language UndecidableInstances #-}
module HBS2.Storage.Simple.Extra where

import HBS2.Merkle
import HBS2.Hash
import HBS2.Prelude
import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Data.Types.Refs
import HBS2.Defaults

import Data.Foldable (for_)
import Data.Bifunctor
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Function
import Lens.Micro.Platform
import System.FilePattern.Directory
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Char8 qualified as BS
import System.FilePath
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad

import Streaming.Prelude qualified as S
import Streaming qualified as S

import System.IO

pieces :: Integral a => a
pieces = 1024

-- FIXME: to-remove-in-a-sake-of-operations-class
class MonadIO m => SimpleStorageExtra a m  where
  putAsMerkle :: forall h . (IsSimpleStorageKey h, Hashed h ByteString) => SimpleStorage h -> a -> m MerkleHash

-- TODO: move-to-hbs2-storage-operations
readChunked :: MonadIO m => Handle -> Int -> S.Stream (S.Of ByteString) m ()
readChunked handle size = fuu
  where
  fuu = fix \next -> do
    chunk <- liftIO do
      B.hGet handle size
    unless (B.null chunk) do
      S.yield chunk
      next


-- TODO: sparse-merkle-tree-representation
--  Блоки пишутся таким образом потому,
--  что хотелось, что бы листы являлись частями
--  исходной информации без всяких метаданных,
--  то есть каждый блок в отдельности является
--  только частью исходных данных, а их конкатенация
--  является этими самыми данными. Это менее оптимальное
--  представление для передачи, но в этом есть смысл.
--
--  то есть у нас есть Merkle Tree которое как бы
--  является торрентом неограниченного размера,
--  скачиваемого по частям, в котором множество
--  указателей на реальные файлы. Имеет смысл.
--
--  Мы в принципе можем измененить способ записи,
--  интересно, что при этом особо ничего не поменяется ---
--  то есть система будет продолжать работать.

instance MonadIO m => SimpleStorageExtra Handle m where
  putAsMerkle ss handle = do

    hashes <- readChunked handle (fromIntegral defBlockSize) -- FIXME: to settings!
                  & S.mapM (\blk -> enqueueBlock ss blk >> pure blk)
                  & S.map (HashRef . hashObject)
                  & S.toList_

    putAsMerkle ss hashes

instance MonadIO m => SimpleStorageExtra (S.Stream (S.Of ByteString) m ()) m where
  putAsMerkle ss streamChunks = do

    hashes <- streamChunks
                  & S.mapM (\blk -> enqueueBlock ss blk >> pure blk)
                  & S.map (HashRef . hashObject)
                  & S.toList_

    putAsMerkle ss hashes

instance MonadIO m => SimpleStorageExtra [HashRef] m where
  putAsMerkle ss hashes = do

    let pt = toPTree (MaxSize pieces) (MaxNum pieces) hashes -- FIXME: settings

    root <- makeMerkle 0 pt $ \(_,_,bs) -> void $ putBlock ss bs

    pure (MerkleHash root)

instance MonadIO m => SimpleStorageExtra ByteString m where
  putAsMerkle ss bs = do

    hashes <- S.each (B.unpack bs)
                & S.chunksOf (fromIntegral defBlockSize)
                & S.mapped (fmap (first B.pack) . S.toList)
                & S.mapM (\blk -> enqueueBlock ss blk >> pure blk)
                & S.map (HashRef . hashObject)
                & S.toList_

    let pt = toPTree (MaxSize pieces) (MaxNum pieces) hashes -- FIXME: settings

    root <- makeMerkle 0 pt $ \(_,_,bss) -> void $ putBlock ss bss

    pure (MerkleHash root)


simpleStorageFsck :: forall h . (IsSimpleStorageKey h, Hashed h ByteString)
                  => SimpleStorage h
                  -> IO [(Maybe (Hash HbSync), FilePath)]

simpleStorageFsck sto = do
  let fblock = view storageBlocks sto

  files <- getDirectoryFiles fblock ["**/*"]

  -- FIXME: thread-num-hardcode
  bad <- forM files $ \f -> do
          let fname = fblock </> f
          let ha = splitDirectories f & mconcat & fromStringMay @(Hash HbSync)
          case ha of
            Just hash -> do
              hr <- BS.readFile fname <&> hashObject @HbSync
              if hr == hash then do
                pure []
              else
                pure [(Just hash, fname)]

            Nothing -> do
              pure [(Nothing, fname)]

  pure $ mconcat bad


