module HBS2Git.Config where

import HBS2.Prelude
import HBS2.System.Logger.Simple
import HBS2.OrDie

import Data.Config.Suckless

import System.FilePath
import System.Directory
import System.IO


findGitDir :: FilePath -> IO (Maybe FilePath)
findGitDir dir = do
  let gitDir = dir </> ".git"
  exists <- doesDirectoryExist gitDir
  if exists
    then return $ Just gitDir
    else let parentDir = takeDirectory dir
         in if parentDir == dir -- we've reached the root directory
               then return Nothing
               else findGitDir parentDir


configPath :: MonadIO m => FilePath -> m FilePath
configPath pwd = do
  xdg <- liftIO $ getXdgDirectory XdgConfig "hbs2-git"
  home <- liftIO getHomeDirectory
  pure $ xdg </> makeRelative home pwd

configInit :: MonadIO m => m ()
configInit = liftIO do
  trace "configInit"

  trace "locating .git directory"

  this <- getCurrentDirectory

  gitDir <- findGitDir this `orDie` ".git directory not found"

  let pwd = takeDirectory gitDir

  confP <- configPath pwd

  trace $ "git dir" <+> pretty gitDir
  trace $ "confPath:" <+> pretty confP

  here <- doesDirectoryExist confP

  unless here do
    debug $ "create directory" <+> pretty confP
    createDirectoryIfMissing True confP



