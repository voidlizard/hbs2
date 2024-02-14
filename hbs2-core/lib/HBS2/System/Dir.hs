module HBS2.System.Dir
  ( module HBS2.System.Dir
  , module System.FilePath
  , module System.FilePattern
  , module UnliftIO
  ) where

import System.FilePath
import System.FilePattern
import System.Directory as D
import UnliftIO hiding (try)


data MkDirOpt = MkDirOptNone

class HasMkDirOptions a where
  mkdirOpts :: a -> [MkDirOpt]

instance HasMkDirOptions FilePath where
  mkdirOpts = mempty

class ToFilePath a where
  toFilePath :: a -> FilePath

instance ToFilePath FilePath where
  toFilePath = id

mkdir :: (MonadIO m, ToFilePath a) => a -> m ()
mkdir a = do
  liftIO $ createDirectoryIfMissing True (toFilePath a)

pwd :: MonadIO m => m FilePath
pwd = liftIO D.getCurrentDirectory

doesPathExist :: MonadIO m => FilePath -> m Bool
doesPathExist = liftIO . D.doesPathExist

canonicalizePath :: MonadIO m => FilePath -> m FilePath
canonicalizePath = liftIO . D.canonicalizePath

expandPath :: MonadIO m => FilePath -> m FilePath
expandPath = liftIO . D.canonicalizePath

doesDirectoryExist :: MonadIO m => FilePath -> m Bool
doesDirectoryExist = liftIO . D.doesDirectoryExist

