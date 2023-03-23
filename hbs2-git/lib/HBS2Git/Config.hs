module HBS2Git.Config
  ( module HBS2Git.Config
  , module Data.Config.Suckless
  ) where

import HBS2.Prelude
import HBS2.System.Logger.Simple
import HBS2.OrDie

import Data.Config.Suckless

import HBS2Git.Types

import Data.Functor
import System.FilePath
import System.Directory

-- type C = MegaParsec

appName :: FilePath
appName = "hbs2-git"

findGitDir :: MonadIO m => FilePath -> m (Maybe FilePath)
findGitDir dir = liftIO do
  let gitDir = dir </> ".git"
  exists <- doesDirectoryExist gitDir
  if exists
    then return $ Just gitDir
    else let parentDir = takeDirectory dir
         in if parentDir == dir -- we've reached the root directory
               then return Nothing
               else findGitDir parentDir


configPath :: MonadIO m => FilePath -> m FilePath
configPath pwd = liftIO do
  xdg <- liftIO $ getXdgDirectory XdgConfig appName
  home <- liftIO getHomeDirectory
  gitDir <- findGitDir pwd `orDie` ".git directory not found"
  pure $ xdg </> makeRelative home pwd

-- returns current directory, where found .git directory
configInit :: MonadIO m => m (FilePath, [Syntax C])
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

  let confFile = confP </> "config"

  confHere <- doesFileExist confFile

  unless confHere do
    appendFile confFile ""

  cfg <- readFile confFile <&> parseTop <&> either mempty id

  pure (pwd, cfg)



