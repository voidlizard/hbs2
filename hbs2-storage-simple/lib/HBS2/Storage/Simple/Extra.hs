{-# Language UndecidableInstances #-}
module HBS2.Storage.Simple.Extra where

import HBS2.Merkle
import HBS2.Hash
import HBS2.Prelude
import HBS2.Storage.Simple
import HBS2.Data.Types.Refs
import HBS2.Defaults

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Function
import Streaming.Prelude qualified as S
import System.IO

class IsKey h => SimpleStorageExtra h a  where
  putAsMerkle :: SimpleStorage h -> a -> IO MerkleHash

readChunked :: MonadIO m => Handle -> Int -> S.Stream (S.Of ByteString) m ()
readChunked handle size = fuu
  where
  fuu = fix \next -> do
    chunk <- liftIO do
      B.hGet handle size
    unless (B.null chunk) do
      S.yield chunk
      next

instance (IsKey h, Key h ~ Hash h, Hashed h ByteString) => SimpleStorageExtra h Handle where
  putAsMerkle ss handle = do

    hashes <- readChunked handle (fromIntegral defBlockSize) -- FIXME: to settings!
                  & S.mapM (\blk -> enqueueBlock ss blk >> pure blk)
                  & S.map (HashRef . hashObject)
                  & S.toList_

    let pt = toPTree (MaxSize 8192) (MaxNum 8192) hashes -- FIXME: settings

    root <- makeMerkle 0 pt $ \(_,_,bs) -> void $ putBlock ss bs

    pure (MerkleHash root)

