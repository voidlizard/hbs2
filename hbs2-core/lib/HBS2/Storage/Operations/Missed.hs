module HBS2.Storage.Operations.Missed where

import HBS2.Prelude.Plated
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Merkle
import HBS2.Storage

import HBS2.System.Logger.Simple

import Streaming.Prelude qualified as S
import Control.Monad.Trans.Maybe
import Control.Monad
import Data.Maybe

-- TODO: slow-dangerous
findMissedBlocks :: (MonadIO m) => AnyStorage -> HashRef -> m [HashRef]
findMissedBlocks sto href = do

  trace $ "findMissedBlocks" <+> pretty href

  S.toList_ $

    walkMerkle (fromHashRef href) (lift . getBlock sto) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
      case hr of
        -- FIXME: investigate-this-wtf
        Left hx -> S.yield (HashRef hx)
        Right (hrr :: [HashRef]) -> do
          forM_ hrr $ \hx -> runMaybeT do
              blk <- lift $ getBlock sto (fromHashRef hx)

              unless (isJust blk) do
                lift $ S.yield hx

              maybe1 blk none $ \bs -> do
                let w = tryDetect (fromHashRef hx) bs
                r <- case w of
                      Merkle{}    -> lift $ lift $ findMissedBlocks sto hx
                      MerkleAnn{} -> lift $ lift $ findMissedBlocks sto hx
                      _ -> pure mempty

                lift $ mapM_ S.yield r


