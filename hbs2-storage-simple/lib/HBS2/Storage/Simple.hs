{-# Language TemplateHaskell #-}
module HBS2.Storage.Simple where

import Control.Concurrent.Async
import Control.Exception (try,tryJust)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Foldable
import Data.List qualified as L
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Error

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue qualified as TBQ
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TV

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

newtype StoragePrefix = StoragePrefix  { fromPrefix :: FilePath }
                        deriving stock (Data,Show)
                        deriving newtype (IsString)

newtype StorageQueueSize = StorageQueueSize { fromQueueSize :: Int }
                           deriving stock   (Data,Show)
                           deriving newtype (Eq,Ord,Enum,Num,Integral,Real)


data SimpleStorage a =
  SimpleStorage
  { _storageDir     :: FilePath
  , _storageOpQ     :: TBQueue ( IO () )
  , _storageHandles :: Cache (Key (Raw LBS.ByteString)) Handle
  }

makeLenses ''SimpleStorage

storageBlocks :: SimpleGetter (SimpleStorage h) FilePath
storageBlocks = to f
  where
    f b = _storageDir b </> "blocks"


simpleStorageInit :: (MonadIO m, Data opts) => opts -> m (SimpleStorage h)
simpleStorageInit opts = liftIO $ do
  let prefix = uniLastDef "." opts :: StoragePrefix
  let qSize  = uniLastDef 100 opts :: StorageQueueSize

  pdir <- canonicalizePath (fromPrefix prefix)

  tbq <- TBQ.newTBQueueIO (fromIntegral (fromQueueSize qSize))

  hcache <- Cache.newCache (Just (toTimeSpec @'Seconds 10)) -- FIXME: real setting

  let stor = SimpleStorage
             { _storageDir = pdir
             , _storageOpQ = tbq
             , _storageHandles = hcache
             }

  -- print ("STORAGE", stor ^. storageDir, stor ^. storageBlocks )

  createDirectoryIfMissing True (stor ^. storageBlocks)

  let alph = getAlphabet

  for_ alph $ \a -> do
    createDirectoryIfMissing True ( (stor ^. storageBlocks) </> L.singleton a )

  pure stor

simpleStorageWorker :: SimpleStorage h -> IO ()
simpleStorageWorker ss = do

  readOps <- async $ forever $ do
    join $ atomically $ TBQ.readTBQueue ( ss ^. storageOpQ )

  writeOps <- async $ forever $ do
    join $ atomically $ TBQ.readTBQueue ( ss ^. storageOpQ )

  killer <- async $ forever $ do
    pause ( 1 :: Timeout 'Minutes ) -- FIXME: setting
    Cache.purgeExpired ( ss ^. storageHandles )

  void $ waitAnyCatchCancel [readOps,writeOps,killer]

simpleGetHandle :: SimpleStorage h -> Key (Raw LBS.ByteString) -> IO Handle
simpleGetHandle s k = do
  let cache = s ^. storageHandles
  let fn = simpleBlockFileName s k
  Cache.fetchWithCache cache k $ const $ openFile fn ReadMode

simpleBlockFileName :: SimpleStorage h -> Hash HbSync -> FilePath
simpleBlockFileName ss h = path
  where
    (pref,suf) = splitAt 1 (show (pretty h))
    path = view storageBlocks ss </> pref </> suf

-- NOTE: reads whole file into memory!
--       if size is too big --- it will
--       cause consequences!
--
--       However, we can not hold the file
--       handle in lazy bytestring, because
--       here maybe too many open files
--
--       So, the block MUST be small
--
simpleGetBlockLazy ::  SimpleStorage h
                    -> Key (Raw LBS.ByteString)
                    -> IO (Maybe LBS.ByteString)

simpleGetBlockLazy s key = do
  resQ <- TBQ.newTBQueueIO 1 :: IO (TBQueue (Maybe LBS.ByteString))
  let fn = simpleBlockFileName s key
  let action = do

        r <- tryJust (guard . isDoesNotExistError)
                     (BS.readFile fn <&> LBS.fromStrict)

        result <- case r of
                    Right bytes -> pure (Just bytes)
                    Left _      -> pure Nothing

        void $ atomically $ TBQ.writeTBQueue resQ result


  void $ atomically $ TBQ.writeTBQueue ( s ^. storageOpQ ) action

  atomically $ TBQ.readTBQueue resQ

simpleGetChunkLazy :: SimpleStorage h
                   -> Key (Raw LBS.ByteString)
                   -> Offset
                   -> Size
                   -> IO (Maybe LBS.ByteString)

simpleGetChunkLazy s key off size = do
  resQ <- TBQ.newTBQueueIO 1 :: IO (TBQueue (Maybe LBS.ByteString))
  let action = do

       r <- tryJust (guard . isDoesNotExistError)
                    (simpleGetHandle s key)

       chunk <- runMaybeT $ do

                  handle <- MaybeT $ case r of
                              Right  h -> pure (Just h)
                              Left _   -> pure Nothing

                  liftIO $ do
                    hSeek handle AbsoluteSeek ( fromIntegral off )
                    LBS.hGet handle (fromIntegral size)

       void $ atomically $ TBQ.writeTBQueue resQ chunk


  void $ atomically $ TBQ.writeTBQueue ( s ^. storageOpQ ) action
  atomically $ TBQ.readTBQueue resQ

simplePutBlockLazy :: SimpleStorage h
                   -> LBS.ByteString
                   -> IO (Maybe (Key (Raw LBS.ByteString)))

simplePutBlockLazy  s lbs = do

  let hash = hashObject lbs :: Key (Raw LBS.ByteString)
  let fn = simpleBlockFileName s hash

  wait <- TBQ.newTBQueueIO 1 :: IO (TBQueue ())

  let action = do
        LBS.writeFile fn lbs
        atomically $ TBQ.writeTBQueue wait ()

  atomically $ TBQ.writeTBQueue (s ^. storageOpQ) action

  void $ atomically $ TBQ.readTBQueue wait

  pure (Just hash)


instance Hashed HbSync (Raw LBS.ByteString) where
  hashObject (Raw s) = hashObject s


instance (MonadIO m, (Hashed hash (Raw LBS.ByteString)))
  => Storage (SimpleStorage hash) (Raw LBS.ByteString) m where

  type instance StorageHash (SimpleStorage hash) (Raw LBS.ByteString) = hash

  putBlock s lbs = liftIO $ simplePutBlockLazy s lbs

  getBlock s key = liftIO $ simpleGetBlockLazy s key

  getChunk s k off size = liftIO $ simpleGetChunkLazy s k off size



