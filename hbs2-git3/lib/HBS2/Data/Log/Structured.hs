module HBS2.Data.Log.Structured where

import HBS2.Prelude.Plated
import HBS2.OrDie

import Network.ByteOrder qualified as N
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
import Control.Monad.Trans.Maybe
import Lens.Micro.Platform

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
  readBytes = readChunkSimple
  noBytesLeft = consumed

instance Monad m => BytesReader (ConsumeBS m) where
  noBytesLeft = gets BS.null
  readBytes n = do
    s <- get
    let (a,b) = BS.splitAt n s
    put $! b
    pure (LBS.fromStrict a)

{- HLINT ignore "Eta reduce"-}
toSectionList :: BS.ByteString -> [BS.ByteString]
toSectionList source = go source
  where
    go bs | BS.length bs < 4 = []
          | otherwise = go1 (BS.splitAt 4 bs & over _1 (fromIntegral . N.word32))

    go1 (len,rest) | BS.length rest < len = []

    go1 (len,rest) = do
      let (sect, rest1) = BS.splitAt len rest
      sect : go rest1

validateSorted :: BS.ByteString -> Bool
validateSorted bs = do
    let sections = toSectionList bs
    let r = flip fix (Nothing, sections, 0) $ \next -> \case
              (_, [], e) -> e
              (Nothing, x:xs, e) -> next (Just x, xs, e)
              (Just v, x:_, e) | v > x -> (e+1)
              (Just _, x:xs, e)  -> next (Just x, xs, e)
    r == 0


scanBS :: Monad m => BS.ByteString -> ( BS.ByteString -> m () ) -> m ()
scanBS bs action = do
  let hsz = 4
  flip fix bs $ \next bss -> do
    if BS.length bss < hsz then pure ()
    else do
      let (ssize, rest) = BS.splitAt hsz bss
      let size =  N.word32 ssize & fromIntegral
      let (sdata, rest2) = BS.splitAt size rest
      if BS.length sdata < size  then
        pure ()
      else do
        action sdata
        next rest2

runConsumeBS :: Monad m => BS.ByteString -> ConsumeBS m a -> m a
runConsumeBS s m = evalStateT (fromConsumeBS m) s


readSections :: forall m . (MonadIO m, BytesReader m)
             => ( Int -> ByteString -> m () )
             -> m ()

readSections action = fix \next -> do
  done <- noBytesLeft
  if done then
    pure ()
  else do
    ssize <- readBytesMaybe 4
               >>= orThrow SomeReadLogError
               <&> fromIntegral . N.word32 . LBS.toStrict

    sdata <- readBytesMaybe ssize
               >>= orThrow SomeReadLogError

    action ssize sdata
    next

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


binarySearchBS :: Monad m
             => Int           -- ^ record size
             -> ( BS.ByteString -> BS.ByteString ) -- ^ key extractor
             -> BS.ByteString -- ^ key
             -> BS.ByteString -- ^ source
             -> m (Maybe Int)

binarySearchBS rs getKey s source = do
  let maxn = BS.length source `div` rs
  loop 0 maxn
  where
    loop l u | u <= l = pure Nothing
             | otherwise = do
                 let e = getKey (BS.drop ( k * rs ) source)
                 case compare e s of
                  EQ -> pure $ Just (k * rs)
                  LT -> loop (k+1) u
                  GT -> loop l k

      where k = (l + u) `div` 2

