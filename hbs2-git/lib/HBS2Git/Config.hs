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

-- Finds .git dir inside given directory moving upwards
findGitDir :: MonadIO m => FilePath -> m (Maybe FilePath)
findGitDir dir = liftIO do
  trace "locating .git directory"
  let gitDir = dir </> ".git"
  exists <- doesDirectoryExist gitDir
  if exists
    then return $ Just gitDir
    else let parentDir = takeDirectory dir
         in if parentDir == dir -- we've reached the root directory
               then return Nothing
               else findGitDir parentDir

-- Finds .git dir inside current directory moving upwards
findWorkingGitDir :: MonadIO m => m FilePath
findWorkingGitDir = do
  this <- liftIO getCurrentDirectory
  findGitDir this `orDie` ".git directory not found"

configPathOld :: MonadIO m => FilePath -> m FilePath
configPathOld pwd = liftIO do
  xdg <- liftIO $ getXdgDirectory XdgConfig appName
  home <- liftIO getHomeDirectory
  pure $ xdg </> makeRelative home pwd

configPath :: MonadIO m => FilePath -> m FilePath
configPath _ = liftIO do
  here   <- liftIO getCurrentDirectory
  (findGitDir here <&> fmap ((</> ".hbs2") . takeDirectory)) `orDie` "*** hbs2-git: .git directory not found"

data ConfigPathInfo = ConfigPathInfo {
  configRepoParentDir :: FilePath,
  configDir :: FilePath,
  configFilePath :: FilePath
} deriving (Eq, Show)

-- returns git repository parent dir, config directory and config file path
getConfigPathInfo :: MonadIO m => m ConfigPathInfo
getConfigPathInfo = do
  trace "getConfigPathInfo"
  gitDir <- findWorkingGitDir
  let pwd = takeDirectory gitDir
  confP <- configPath pwd
  let confFile = confP </> "config"
  trace $ "git dir" <+> pretty gitDir
  trace $ "confPath:" <+> pretty confP
  pure ConfigPathInfo {
      configRepoParentDir = pwd,
      configDir = confP,
      configFilePath = confFile
    }

-- returns current directory, where found .git directory
configInit :: MonadIO m => m (FilePath, [Syntax C])
configInit = liftIO do
  trace "configInit"
  ConfigPathInfo{..} <- getConfigPathInfo
  here <- doesDirectoryExist configDir
  unless here do
    debug $ "create directory" <+> pretty configDir
    createDirectoryIfMissing True configDir
  confHere <- doesFileExist configFilePath
  unless confHere do
    appendFile configFilePath ""
  cfg <- readFile configFilePath <&> parseTop <&> either mempty id
  pure (configRepoParentDir, cfg)

cookieFile :: MonadIO m => m FilePath
cookieFile = configPath "" <&> (</> "cookie")

