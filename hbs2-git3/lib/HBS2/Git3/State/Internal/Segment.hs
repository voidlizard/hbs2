module HBS2.Git3.State.Internal.Segment where

import HBS2.Git3.Prelude
import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Lazy qualified as LBS

import Codec.Compression.Zstd.Streaming as ZStdS

decompressSegmentLBS :: MonadIO m => ByteString -> m (Maybe ByteString)
decompressSegmentLBS source = runMaybeT do
  let chunks = LBS.toChunks source
  toMPlus =<< liftIO do
    init <- decompress
    flip fix (init, chunks, mempty :: LBS.ByteString) $ \next -> \case

      (Consume work, [], o) -> do
        r1 <- work ""
        next (r1, [], o)

      (Consume work, e:es, o) -> do
        r1 <- work e
        next (r1, es, o)

      (Produce piece r, e, o) -> do
        r1 <- r
        next (r1, e, LBS.append o (LBS.fromStrict piece))

      (ZStdS.Done bs, _, o) -> pure (Just (LBS.append o (LBS.fromStrict bs)))

      (Error _ _, _, _) -> do
        pure Nothing

