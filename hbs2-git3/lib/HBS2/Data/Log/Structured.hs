module HBS2.Data.Log.Structured where

import HBS2.Prelude.Plated

import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Maybe
import Network.ByteOrder hiding (ByteString)
import Control.Monad.State

import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Streaming qualified as Zstd
import Codec.Compression.Zstd.Streaming (Result(..))

import Control.Exception

-- import UnliftIO

class ReadLogOpts a where

data ReadLogError = SomeReadLogError
  deriving stock (Typeable, Show)

instance Exception ReadLogError

instance ReadLogOpts ()

type NumBytes = Int

class Monad m => BytesReader m where
  noBytesLeft    :: m Bool
  readBytes      :: NumBytes -> m ByteString

  readBytesMaybe :: NumBytes -> m (Maybe ByteString)
  readBytesMaybe n = do
    bs <- readBytes n
    if LBS.length bs == fromIntegral n then pure (Just bs) else pure Nothing

newtype ConsumeLBS m a = ConsumeLBS { fromConsumeLBS :: StateT ByteString m a  }
                         deriving newtype ( Applicative
                                          , Functor
                                          , Monad
                                          , MonadState ByteString
                                          , MonadIO
                                          , MonadTrans
                                          )

readChunkThrow :: MonadIO m => Int -> ConsumeLBS m ByteString
readChunkThrow n = do
  lbs <- get
  let (this, that) = LBS.splitAt (fromIntegral n) lbs
  if LBS.length this /= fromIntegral n then
      liftIO $ throwIO SomeReadLogError
  else do
    put $! that
    pure this

readChunkSimple :: Monad m => Int -> ConsumeLBS m ByteString
readChunkSimple n = do
  lbs <- get
  let (this, that) = LBS.splitAt (fromIntegral n) lbs
  put $! that
  pure this

reminds :: Monad m => ConsumeLBS m Int
reminds = gets (fromIntegral . LBS.length)

consumed  :: Monad m => ConsumeLBS m Bool
consumed = gets LBS.null

runConsumeLBS :: Monad m => ByteString -> ConsumeLBS m a -> m a
runConsumeLBS s m  = evalStateT (fromConsumeLBS m) s

newtype ConsumeBS m a = ConsumeBS { fromConsumeBS :: StateT BS.ByteString m a  }
                        deriving newtype ( Applicative
                                         , Functor
                                         , Monad
                                         , MonadState BS.ByteString
                                         , MonadIO
                                         , MonadTrans
                                         )


instance Monad m => BytesReader (ConsumeLBS m) where
  readBytes n = readChunkSimple n
  noBytesLeft = consumed

instance Monad m => BytesReader (ConsumeBS m) where
  noBytesLeft = gets BS.null
  readBytes n = do
    s <- get
    let (a,b) = BS.splitAt n s
    put $! b
    pure (LBS.fromStrict a)

runConsumeBS :: Monad m => BS.ByteString -> ConsumeBS m a -> m a
runConsumeBS s m = evalStateT (fromConsumeBS m) s







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

