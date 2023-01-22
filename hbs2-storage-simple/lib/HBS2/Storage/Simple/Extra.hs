{-# Language UndecidableInstances #-}
module HBS2.Storage.Simple.Extra where

import HBS2.Merkle
import HBS2.Hash
import HBS2.Prelude
import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Data.Types.Refs
import HBS2.Defaults

import Data.Bifunctor
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Function

import Streaming.Prelude qualified as S
import Streaming qualified as S

import System.IO

pieces :: Integral a => a
pieces = 8192

class SimpleStorageExtra a  where
  putAsMerkle :: forall h . (IsKey h, Hash h ~ Key h, Hashed h ByteString, Block ByteString ~ ByteString) => SimpleStorage h -> a -> IO MerkleHash

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

