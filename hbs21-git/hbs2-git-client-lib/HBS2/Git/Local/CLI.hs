module HBS2.Git.Local.CLI where

import HBS2.Prelude

import System.FilePath
import HBS2.System.Dir

import System.Environment hiding (setEnv)

import Control.Monad.Trans.Maybe
import Control.Applicative
import System.Process.Typed
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Text.InterpolatedString.Perl6 (qc)

findGitDir :: MonadIO m => m (Maybe FilePath)
findGitDir = findGitDir' =<< pwd
  where
    findGitDir' dir = do
      let gd = dir </> ".git"
      exists <- liftIO $ doesDirectoryExist gd
      if exists
        then return $ Just gd
        else let parentDir = takeDirectory dir
             in if parentDir == dir -- we've reached the root directory
                   then return Nothing
                   else findGitDir' parentDir

checkIsBare :: MonadIO m => Maybe FilePath -> m Bool
checkIsBare fp = do

  let wd = maybe id setWorkingDir fp

  (code,s,_)  <- readProcess ( shell [qc|git config --local core.bare|]
                                & setStderr closed & wd
                             )

  case (code, LBS8.words s) of
    (ExitSuccess, "true" : _)  -> pure True
    _ -> pure False

gitDir :: MonadIO m => m (Maybe FilePath)
gitDir = runMaybeT do
  byEnv <- liftIO $ lookupEnv "GIT_DIR"
  byDir <- findGitDir

  byBare <- checkIsBare Nothing >>= \case
              True  -> pwd >>= expandPath <&> Just
              False -> pure Nothing

  toMPlus (byEnv <|> byDir <|> byBare)


gitRunCommand :: MonadIO m
              => String
              -> m (Either ExitCode ByteString)

gitRunCommand cmd = do
  let procCfg = setStdin closed $ setStderr closed $ shell cmd
  (code, out, _) <- readProcess procCfg
  case code of
    ExitSuccess -> pure (Right out)
    e           -> pure (Left e)


