{-# Language TemplateHaskell #-}
module HBS2.Storage.Simple where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Error

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue qualified as TBQ
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBMQueue qualified as TBMQ
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TV

import Debug.Trace

import HBS2.Clock
import HBS2.Hash
import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Storage

-- NOTE:  random accessing files in a git-like storage
--        causes to file handles exhaust.
--        Therefore, those handles MUST be in something like
--        pool.
--
--        I.e. we're should use something like TBChan to queue
--        operations and wait in getBlock 'till it's completion
--        in order to make the disk access in this fashion safe


newtype Raw a = Raw { fromRaw :: a }

type instance Block (Raw LBS.ByteString) = LBS.ByteString
type instance Key (Raw LBS.ByteString) = Hash HbSync


newtype StorageQueueSize = StorageQueueSize { fromQueueSize :: Int }
                           deriving stock   (Data,Show)
                           deriving newtype (Eq,Ord,Enum,Num,Integral,Real)


data SimpleStorage a =
  SimpleStorage
  { _storageDir     :: FilePath
  , _storageOpQ     :: TBMQueue ( IO () )
  , _storageChunksCache :: Cache (FilePath, Offset, Size) ByteString
  , _storageStopWriting :: TVar Bool
  }

makeLenses ''SimpleStorage

storageBlocks :: SimpleGetter (SimpleStorage h) FilePath
storageBlocks = to f
  where
    f b = _storageDir b </> "blocks"


simpleStorageInit :: (MonadIO m, Data opts) => opts -> m (SimpleStorage h)
simpleStorageInit opts = liftIO $ do
  let prefix = uniLastDef "." opts :: StoragePrefix
  let qSize  = uniLastDef 500 opts :: StorageQueueSize

  pdir <- canonicalizePath (fromPrefix prefix)

  tbq <- TBMQ.newTBMQueueIO (fromIntegral (fromQueueSize qSize))

  tstop <- TV.newTVarIO False

  hcache <- Cache.newCache (Just (toTimeSpec @'Seconds 1)) -- FIXME: real setting

  let stor = SimpleStorage
             { _storageDir = pdir
             , _storageOpQ = tbq
             , _storageChunksCache = hcache
             , _storageStopWriting = tstop
             }

  createDirectoryIfMissing True (stor ^. storageBlocks)

  let alph = getAlphabet

  for_ alph $ \a -> do
    createDirectoryIfMissing True ( (stor ^. storageBlocks) </> L.singleton a )

  pure stor

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

simpleAddTask :: SimpleStorage h -> IO () -> IO ()
simpleAddTask s task = do
  atomically $ TBMQ.writeTBMQueue (s ^. storageOpQ) task

simpleStorageStop :: SimpleStorage h -> IO ()
simpleStorageStop ss = do
  atomically $ TV.writeTVar ( ss ^. storageStopWriting ) True
  fix \next -> do
    mt <- atomically $ TBMQ.isEmptyTBMQueue ( ss ^. storageOpQ )
    if mt then do
      atomically $ TBMQ.closeTBMQueue ( ss ^. storageOpQ )
      pure ()
    else
      pause ( 0.01 :: Timeout 'Seconds ) >> next

simpleStorageWorker :: SimpleStorage h -> IO ()
simpleStorageWorker ss = do

  ops <- async $ fix \next -> do
    s <- atomically $ do TBMQ.readTBMQueue ( ss ^. storageOpQ )
    case s of
      Nothing -> pure ()
      Just a  -> a >> next

  killer <- async $ forever $ do
    pause ( 30 :: Timeout 'Seconds ) -- FIXME: setting
    Cache.purgeExpired ( ss ^. storageChunksCache )

  (_, e) <- waitAnyCatchCancel [ops,killer]

  pure ()


simpleChunkLookup :: SimpleStorage h
                  -> Key (Raw LBS.ByteString)
                  -> Offset
                  -> Size
                  -> IO (Maybe LBS.ByteString)

simpleChunkLookup s k off size = do
  let fn = simpleBlockFileName s k
  let cache  = s ^. storageChunksCache
  Cache.lookup cache (fn, off, size) <&> fmap LBS.fromStrict

simpleChunkCache :: SimpleStorage h
                 -> Key (Raw LBS.ByteString)
                 -> Offset
                 -> Size
                 -> LBS.ByteString
                 -> IO ()

simpleChunkCache s k off size bs = do
  let fn = simpleBlockFileName s k
  let cache  = s ^. storageChunksCache
  -- print ("caching!", fn, off, size)
  Cache.insert cache (fn, off, size) (LBS.toStrict bs)

simpleBlockFileName :: SimpleStorage h -> Hash HbSync -> FilePath
simpleBlockFileName ss h = path
  where
    (pref,suf) = splitAt 1 (show (pretty h))
    path = view storageBlocks ss </> pref </> suf



-- NOTE: reads a whole file into memory!
--       if file size is too big --- it will
--       cause consequences!
--
--       However, we can not hold file
--       handles in lazy bytestrings, because
--       here maybe too many open files
--
--       So, the block MUST be small
--
simpleGetBlockLazy ::  SimpleStorage h
                    -> Key (Raw LBS.ByteString)
                    -> IO (Maybe LBS.ByteString)

simpleGetBlockLazy s key = do
  resQ <- TBMQ.newTBMQueueIO 1 :: IO (TBMQueue (Maybe LBS.ByteString))
  let fn = simpleBlockFileName s key
  let action = do

        r <- tryJust (guard . isDoesNotExistError)
                     (BS.readFile fn <&> LBS.fromStrict)

        result <- case r of
                    Right bytes -> pure (Just bytes)
                    Left _      -> pure Nothing

        void $ atomically $ TBMQ.writeTBMQueue resQ result

  let onFail (_ :: IOError)=  void $ atomically $ TBMQ.writeTBMQueue resQ Nothing

  simpleAddTask s (catch action onFail)

  atomically $ TBMQ.readTBMQueue resQ >>= maybe (pure Nothing) pure

simpleGetChunkLazy :: SimpleStorage h
                   -> Key (Raw LBS.ByteString)
                   -> Offset
                   -> Size
                   -> IO (Maybe LBS.ByteString)

simpleGetChunkLazy s key off size = do
  resQ <- TBMQ.newTBMQueueIO 1 :: IO (TBMQueue (Maybe LBS.ByteString))
  let action = do
       let fn = simpleBlockFileName s key

       cached <- simpleChunkLookup s key off size

       case cached of
        Just chunk -> do
           void $ atomically $ TBMQ.writeTBMQueue resQ (Just chunk)

        Nothing -> do
           r <- tryJust (guard . isDoesNotExistError)
                 $ withBinaryFile fn ReadMode $ \handle -> do
                      hSeek handle AbsoluteSeek ( fromIntegral off )
                      bytes <- LBS.hGet handle ( fromIntegral size )

                      let ahead = 16
                      let bnum = off `div` fromIntegral size
                      let doCache =
                                 ahead > 0
                              && size > 0
                              && size < 4096
                              && (bnum `mod` ahead) == 0

                      when doCache do -- FIXME:! setting
                        chunks <- forM [ size .. size * fromIntegral ahead ] $ \i -> do
                                    let o = fromIntegral off + fromIntegral (i * size)
                                    hSeek handle AbsoluteSeek o
                                    fwd <- LBS.hGet handle (fromIntegral size)
                                    pure (fwd, fromIntegral o)

                        let chunks' = takeWhile (not . LBS.null . fst) chunks
                        mapM_ (\(c,o) -> simpleChunkCache s key o size c) chunks'

                      pure bytes

           result <- case r of
                       Right bytes -> pure (Just bytes)
                       Left _      -> pure Nothing

           void $ atomically $ TBMQ.writeTBMQueue resQ result

  let onFail (_ :: IOError)=  void $ atomically $ TBMQ.writeTBMQueue resQ Nothing

  simpleAddTask s (catch action onFail )

  atomically $ TBMQ.readTBMQueue resQ >>= maybe (pure Nothing) pure

simplePutBlockLazy :: Bool -- | wait
                   -> SimpleStorage h
                   -> LBS.ByteString
                   -> IO (Maybe (Key (Raw LBS.ByteString)))

simplePutBlockLazy doWait s lbs = do

  let hash = hashObject lbs :: Key (Raw LBS.ByteString)
  let fn = simpleBlockFileName s hash

  stop <- atomically $ TV.readTVar ( s ^. storageStopWriting )

  if stop then do
    pure Nothing

  else do

    waits <- TBQ.newTBQueueIO 1 :: IO (TBQueue Bool)

    let action = do
          catch (LBS.writeFile fn lbs)
                (\(_ :: IOError) -> atomically $ TBQ.writeTBQueue waits False)

          atomically $ TBQ.writeTBQueue waits True

    simpleAddTask s action

    if doWait then do
      ok <- atomically $ TBQ.readTBQueue waits
      pure $! if ok then Just hash else Nothing
    else
      pure $ Just hash


simpleBlockExists :: SimpleStorage h
                  -> Key (Raw LBS.ByteString)
                  -> IO Bool

simpleBlockExists ss hash = doesFileExist $ simpleBlockFileName ss hash

instance Hashed HbSync (Raw LBS.ByteString) where
  hashObject (Raw s) = hashObject s

instance (MonadIO m, (Hashed hash (Raw LBS.ByteString)))
  => Storage (SimpleStorage hash) (Raw LBS.ByteString) m where

  type instance StorageHash (SimpleStorage hash) (Raw LBS.ByteString) = hash

  putBlock s lbs = liftIO $ simplePutBlockLazy True s lbs

  enqueueBlock s lbs = liftIO $ simplePutBlockLazy False s  lbs

  getBlock s key = liftIO $ simpleGetBlockLazy s key

  getChunk s k off size = liftIO $ simpleGetChunkLazy s k off size

  hasBlock s k = liftIO $ simpleBlockExists s k

