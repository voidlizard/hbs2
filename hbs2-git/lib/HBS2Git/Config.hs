module HBS2Git.Config
  ( module HBS2Git.Config
  , module Data.Config.Suckless
  ) where

import HBS2.Prelude
import HBS2.Base58
import HBS2.System.Logger.Simple
import HBS2.OrDie

import Data.Config.Suckless

import HBS2Git.Types

import Control.Applicative

import Data.Functor
import System.FilePath
import System.Directory

import System.Environment

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
  env <- liftIO getEnvironment
  -- hPrint stderr $ pretty env
  pwd <- liftIO getCurrentDirectory
  git <- findGitDir pwd
  byEnv <- lookupEnv "GIT_DIR"
  -- hPrint stderr ("BY-ENV", byEnv)
  -- hPrint stderr =<< getEnvironment
  path <- pure (git <|> byEnv) `orDie`  "*** hbs2-git: .git directory not found"
  pure (takeDirectory path </> ".hbs2")

data ConfigPathInfo = ConfigPathInfo {
  configRepoParentDir :: FilePath,
  configDir :: FilePath,
  configFilePath :: FilePath
} deriving (Eq, Show)

-- returns git repository parent dir, config directory and config file path
getConfigPathInfo :: MonadIO m => m ConfigPathInfo
getConfigPathInfo = do
  trace "getConfigPathInfo"
  confP <- configPath ""
  let pwd = takeDirectory confP
  let confFile = confP </> "config"
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

getAppStateDir :: forall m . MonadIO m => m FilePath
getAppStateDir = liftIO $ getXdgDirectory XdgData appName


makeDbPath :: MonadIO m => RepoRef -> m FilePath
makeDbPath h = do
  state <- getAppStateDir
  liftIO $ createDirectoryIfMissing True state
  pure $ state </> show (pretty (AsBase58 h))

