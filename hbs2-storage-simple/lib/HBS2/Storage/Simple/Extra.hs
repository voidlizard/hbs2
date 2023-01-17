module HBS2.Storage.Simple.Extra where

import HBS2.Merkle
import HBS2.Prelude
import HBS2.Storage.Simple

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Function
import Streaming.Prelude qualified as S
import System.IO

class SimpleStorageExtra a where
  putAsMerkle :: SimpleStorage h -> a -> MerkleHash

readChunked :: MonadIO m => Handle -> Int -> S.Stream (S.Of ByteString) m ()
readChunked handle size = fuu
  where
  fuu = fix \next -> do
    chunk <- liftIO do
      B.hGet handle size
    unless (B.null chunk) do
      S.yield chunk
      next

-- instance SimpleStorageExtra Handle where
--   putAsMerkle ss handle = undefined

