module HBS2.Data.Log.Structured where

import HBS2.Prelude.Plated

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Network.ByteOrder hiding (ByteString)

import Codec.Compression.Zstd.Streaming qualified as Zstd
import Codec.Compression.Zstd qualified as Zstd
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

writeCompressedStreamZstd :: forall m . MonadIO m
                          => Result
                          -> m (Maybe ByteString)
                          -> ( ByteString -> m () )
                          -> m ()
writeCompressedStreamZstd stream source sink = do

  flip fix (mempty,stream) $ \next -> \case
    (_, Done s)    -> sink (LBS.fromStrict s)

    (_,Error _ _)  -> liftIO (throwIO CompressedStreamWriteError)

    (some, Produce s continue) -> do
      sink (LBS.fromStrict s)
      c <- liftIO continue
      next (some, c)

    ([], w@(Consume consume)) -> do
      source >>= \case
        Just piece -> do
          next (LBS.toChunks piece, w)

        Nothing -> do
          c <- liftIO (consume mempty)
          next ([], c)

    (x:xs, Consume consume) -> do
      c <- liftIO (consume  x)
      next (xs, c)

