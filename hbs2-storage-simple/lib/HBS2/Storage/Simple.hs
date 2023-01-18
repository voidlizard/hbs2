{-# Language TemplateHaskell #-}
{-# Language ScopedTypeVariables #-}
module HBS2.Storage.Simple
  ( module HBS2.Storage.Simple
  , module HBS2.Storage
  ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Except
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
import Control.Concurrent.STM.TBMQueue qualified as TBMQ
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TVar qualified as TV


import HBS2.Clock
import HBS2.Hash
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




type instance Block LBS.ByteString = LBS.ByteString


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


storageRefs :: SimpleGetter (SimpleStorage h) FilePath
storageRefs = to f
  where
    f b = _storageDir b </> "refs"


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
    createDirectoryIfMissing True ( (stor ^. storageRefs) </> L.singleton a )

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


simpleChunkLookup :: IsKey h
                  => SimpleStorage h
                  -> Hash h
                  -> Offset
                  -> Size
                  -> IO (Maybe LBS.ByteString)

simpleChunkLookup s k off size = do
  let fn = simpleBlockFileName s k
  let cache  = s ^. storageChunksCache
  Cache.lookup cache (fn, off, size) <&> fmap LBS.fromStrict

simpleChunkCache :: IsKey h
                 => SimpleStorage h
                 -> Hash h
                 -> Offset
                 -> Size
                 -> LBS.ByteString
                 -> IO ()

simpleChunkCache s k off size bs = do
  let fn = simpleBlockFileName s k
  let cache  = s ^. storageChunksCache
  -- print ("caching!", fn, off, size)
  Cache.insert cache (fn, off, size) (LBS.toStrict bs)

simpleBlockFileName :: Pretty (Hash h) => SimpleStorage h -> Hash h -> FilePath
simpleBlockFileName ss h = path
  where
    (pref,suf) = splitAt 1 (show (pretty h))
    path = view storageBlocks ss </> pref </> suf

simpleRefFileName :: Pretty (Hash h) => SimpleStorage h -> Hash h -> FilePath
simpleRefFileName ss h = path
  where
    (pref,suf) = splitAt 1 (show (pretty h))
    path = view storageRefs ss </> pref </> suf


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
simpleGetBlockLazy ::  (IsKey h, Pretty (Key h))
                    => SimpleStorage h
                    -> Hash h
                    -> IO (Maybe LBS.ByteString)

simpleGetBlockLazy s key = do

  liftIO $ print $ "simpleGetBlockLazy" <+> pretty key

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

simpleGetChunkLazy :: IsKey h
                   => SimpleStorage h
                   -> Hash h
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

simplePutBlockLazy :: (IsKey h, Hashed h LBS.ByteString)
                   => Bool -- | wait
                   -> SimpleStorage h
                   -> LBS.ByteString
                   -> IO (Maybe (Hash h))

simplePutBlockLazy doWait s lbs = do

  let hash = hashObject lbs
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

-- TODO: should be async as well?
simpleBlockExists :: IsKey h
                  => SimpleStorage h
                  -> Hash h
                  -> IO (Maybe Integer)

simpleBlockExists ss hash = runMaybeT $ do
  let fn = simpleBlockFileName ss hash
  exists <- liftIO $ doesFileExist fn
  unless exists mzero
  liftIO $ getFileSize fn

spawnAndWait :: SimpleStorage h -> IO a -> IO (Maybe a)
spawnAndWait s act = do
  doneQ <- TBMQ.newTBMQueueIO 1
  simpleAddTask s (act >>= \r -> atomically (TBMQ.writeTBMQueue doneQ r))
  atomically $ TBMQ.readTBMQueue doneQ


simpleWriteLinkRaw :: forall h . ( IsKey h
                                 , Key h ~ Hash h
                                 , Hashed h LBS.ByteString
                                 )
                   => SimpleStorage h
                   -> Hash h
                   -> LBS.ByteString
                   -> IO (Maybe (Hash h))

simpleWriteLinkRaw ss h lbs = do
  let fnr = simpleRefFileName ss h

  runMaybeT $ do
    r <- MaybeT $ putBlock ss lbs
    MaybeT $ liftIO $ spawnAndWait ss $ do
      writeFile fnr (show (pretty r))
      pure h

simpleReadLinkRaw :: IsKey h
                  => SimpleStorage h
                  -> Hash h
                  -> IO (Maybe LBS.ByteString)

simpleReadLinkRaw ss hash = do
  let fn = simpleRefFileName ss hash
  rs <- spawnAndWait ss $ do
    r <- tryJust (guard . isDoesNotExistError) (LBS.readFile fn)
    case r of
      Right bs -> pure (Just bs)
      Left  _  -> pure Nothing

  pure $ fromMaybe Nothing rs

-- instance Hashed hash LBS.ByteString => Hashed hash LBS.ByteString where
--   hashObject s = hashObject s

instance ( MonadIO m, IsKey hash
         , Hashed hash LBS.ByteString
         , Key hash ~ Hash hash
         )
  => Storage (SimpleStorage hash) hash LBS.ByteString m where

  putBlock s lbs = liftIO $ simplePutBlockLazy True s lbs

  enqueueBlock s lbs = liftIO $ simplePutBlockLazy False s  lbs

  getBlock s key = liftIO $ simpleGetBlockLazy s key

  getChunk s k off size = liftIO $ simpleGetChunkLazy s k off size

  hasBlock s k = liftIO $ simpleBlockExists s k




