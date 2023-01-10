{-# Language TemplateHaskell #-}
module HBS2.Storage.Simple where

import Control.Monad.IO.Class
import System.FilePath.Posix
import Lens.Micro.Platform
import Data.ByteString.Lazy qualified as LBS
import System.Directory

import HBS2.Storage
import HBS2.Prelude
import HBS2.Prelude.Plated

newtype Raw a = Raw { fromRaw :: a }


newtype StoragePrefix = StoragePrefix { fromPrefix :: FilePath }
                        deriving stock (Data,Show)
                        deriving newtype (IsString)

newtype SimpleStorage =
  SimpleStorage
  { _storageDir :: FilePath
  }

makeLenses ''SimpleStorage

storageBlocksDir :: SimpleStorage -> FilePath
storageBlocksDir s = view storageDir s </> "blocks"

storageBlocks :: SimpleGetter SimpleStorage FilePath
storageBlocks = to f
  where
    f b = _storageDir b </> "blocks"


simpleStorageInit :: (MonadIO m, Data opts) => opts -> m SimpleStorage
simpleStorageInit opts = liftIO $ do
  let prefix = uniLastDef "." opts :: StoragePrefix

  pdir <- canonicalizePath (fromPrefix prefix)

  let stor = SimpleStorage
             { _storageDir = pdir
             }

  createDirectoryIfMissing True (stor ^. storageBlocks)

  pure stor

instance MonadIO m => Storage SimpleStorage (Raw LBS.ByteString) m where


