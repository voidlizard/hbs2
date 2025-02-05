{-# Language MultiWayIf #-}
module Data.Config.Suckless.System where

import Data.Function
import System.FilePath
import System.Directory qualified as D
import Data.ByteString.Lazy qualified as LBS
import UnliftIO
import Control.Exception qualified as E
import Control.Monad

import Streaming.Prelude qualified as S

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
    unless dir do
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

doesFileExist :: MonadIO m => FilePath -> m Bool
doesFileExist = liftIO . D.doesFileExist


fileSize :: MonadIO m => FilePath -> m Integer
fileSize = liftIO . D.getFileSize

mv :: MonadIO m => FilePath -> FilePath -> m ()
mv a b = liftIO $ D.renamePath a b

rm :: MonadIO m => FilePath -> m ()
rm fn = liftIO $ D.removePathForcibly fn

home :: MonadIO m => m FilePath
home = liftIO D.getHomeDirectory

cd :: MonadIO m => FilePath -> m()
cd = liftIO . D.setCurrentDirectory

data DirEntry =
    EntryFile  { dirEntryPath :: FilePath }
  | EntryDir   { dirEntryPath :: FilePath }
  | EntryOther { dirEntryPath :: FilePath }

dirFiles :: MonadIO m => FilePath -> m [FilePath]
dirFiles d = S.toList_ $ do
  dirEntries d $ \case
    EntryFile f -> S.yield f >> pure True
    _ -> pure True

dirEntries :: MonadIO m => FilePath -> ( DirEntry -> m Bool ) -> m ()
dirEntries dir what  = do
  es <- liftIO $ D.listDirectory dir

  flip fix es $ \next -> \case
    [] -> pure ()
    (x:xs) -> do
      let entry = dir </> x
      isFile <- liftIO (D.doesFileExist entry)
      isDir <- liftIO (D.doesDirectoryExist entry)
      if | isFile    -> continueThen (what (EntryFile entry)) (next xs)
         | isDir     -> continueThen (what (EntryDir entry)) (next xs)
         | otherwise -> continueThen (what (EntryOther entry)) (next xs)

  where
    continueThen a b = do
      r <- a
      when r b


