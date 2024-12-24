module HBS2.Data.Log.Structured where

import HBS2.Prelude.Plated

import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Maybe
import Network.ByteOrder hiding (ByteString)

import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Streaming qualified as Zstd
import Codec.Compression.Zstd.Streaming (Result(..))

import Control.Exception

-- import UnliftIO

writeSection :: forall m . Monad m
             => ByteString
             -> ( ByteString -> m () )
             -> m ()

writeSection bs output = do
  let bssize  = bytestring32 (fromIntegral $ LBS.length bs)
  let section = B.byteString bssize <> B.lazyByteString bs
  output (B.toLazyByteString section)


writeSections :: forall m . Monad m
              => m (Maybe ByteString)
              -> ( ByteString -> m () )
              -> m ()

writeSections source sink = fix \next -> do
    source >>= maybe none (\bs -> writeSection bs sink >> next)


data CompressedStreamError =
  CompressedStreamWriteError
  deriving stock (Typeable,Show)

instance Exception CompressedStreamError

writeCompressedChunkZstd :: forall m . MonadIO m
                         => ( ByteString -> m () )
                         -> Result
                         -> Maybe ByteString
                         -> m Result

writeCompressedChunkZstd sink stream mlbs  = do
  flip fix ( LBS.toChunks lbs, stream) $ \next -> \case

    ([], r@(Done s)) -> sink (LBS.fromStrict s) >> pure r

    (_, Done{}) -> liftIO (throwIO CompressedStreamWriteError)

    (_, Error{})-> liftIO (throwIO CompressedStreamWriteError)

    (w, Produce s continue) -> do
      sink (LBS.fromStrict s)
      c <- liftIO continue
      next (w, c)

    (_, Consume consume) | isNothing mlbs -> do
      r <- liftIO (consume mempty)
      next ([], r)

    ([], r@(Consume{})) -> pure r

    (x:xs, r@(Consume consume)) -> do
      what <- liftIO (consume x)
      next (xs, what)

  where
    lbs = fromMaybe mempty mlbs


writeCompressedStreamZstd :: forall m . MonadIO m
                          => Result
                          -> m (Maybe ByteString)
                          -> ( ByteString -> m () )
                          -> m ()
writeCompressedStreamZstd stream source sink = do
  flip fix stream $ \next sn -> do
    source >>= \case
      Nothing  -> writeCompressedChunkZstd sink sn Nothing >> none
      Just lbs -> writeCompressedChunkZstd sink sn (Just lbs) >>= next

