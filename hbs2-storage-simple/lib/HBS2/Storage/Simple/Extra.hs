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

class SimpleStorageExtra a  where
  putAsMerkle :: forall h . (IsSimpleStorageKey h, Hashed h ByteString) => SimpleStorage h -> a -> IO MerkleHash

readChunked :: MonadIO m => Handle -> Int -> S.Stream (S.Of ByteString) m ()
readChunked handle size = fuu
  where
  fuu = fix \next -> do
    chunk <- liftIO do
      B.hGet handle size
    unless (B.null chunk) do
      S.yield chunk
      next

instance SimpleStorageExtra Handle where
  putAsMerkle ss handle = do

    hashes <- readChunked handle (fromIntegral defBlockSize) -- FIXME: to settings!
                  & S.mapM (\blk -> enqueueBlock ss blk >> pure blk)
                  & S.map (HashRef . hashObject)
                  & S.toList_

    putAsMerkle ss hashes

instance SimpleStorageExtra (S.Stream (S.Of ByteString) IO ()) where
  putAsMerkle ss streamChunks = do

    hashes <- streamChunks
                  & S.mapM (\blk -> enqueueBlock ss blk >> pure blk)
                  & S.map (HashRef . hashObject)
                  & S.toList_

    putAsMerkle ss hashes

instance SimpleStorageExtra [HashRef] where
  putAsMerkle ss hashes = do

    let pt = toPTree (MaxSize pieces) (MaxNum pieces) hashes -- FIXME: settings

    root <- makeMerkle 0 pt $ \(_,_,bs) -> void $ putBlock ss bs

    pure (MerkleHash root)

instance Block ByteString ~ ByteString => SimpleStorageExtra ByteString where
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


