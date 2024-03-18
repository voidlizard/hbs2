module HBS2.System.Dir
  ( module HBS2.System.Dir
  , module System.FilePath
  , module System.FilePattern
  , module UnliftIO
  ) where

import System.FilePath
import System.FilePattern
import System.Directory qualified as D
import Data.ByteString.Lazy qualified as LBS
import UnliftIO
import Control.Exception qualified as E
import Control.Monad

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
  void $ liftIO $ E.try @SomeException (D.createDirectoryIfMissing True (toFilePath a))

data TouchOpt = TouchEasy | TouchHard
                 deriving stock (Eq,Ord,Show)

class ToFilePath a => HasTouchOpts a where
  touchOpts :: a -> [TouchOpt]

instance HasTouchOpts FilePath where
  touchOpts = const [TouchEasy]

touch :: (MonadIO m, HasTouchOpts a) => a -> m ()
touch what = do
  here <- doesPathExist fn
  dir  <- doesDirectoryExist fn

  when (not here || hard) do
    mkdir (takeDirectory fn)
    liftIO $ print (takeDirectory fn)
    unless dir do
      liftIO $ print fn
      liftIO $ LBS.appendFile fn mempty

  where
    hard = TouchHard `elem` touchOpts  what
    fn = toFilePath what

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


