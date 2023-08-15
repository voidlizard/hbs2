module HBS2Git.Config
  ( module HBS2Git.Config
  , module Data.Config.Suckless
  ) where

import Data.Char (toLower)
import Data.Config.Suckless
import Data.Functor
import HBS2.OrDie
import HBS2.Prelude
import HBS2.System.Logger.Simple
import Prettyprinter.Render.Terminal
import System.Directory
import System.FilePath
import System.IO
import Text.InterpolatedString.Perl6 (qc)

-- type C = MegaParsec

appName :: FilePath
appName = "hbs2-git"

configFileName :: FilePath
configFileName = "config"

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

getRepoDir :: MonadIO m => m FilePath
getRepoDir = takeDirectory <$> findWorkingGitDir

getOldConfigDir :: MonadIO m => FilePath -> m FilePath
getOldConfigDir repoDir = liftIO do
  xdg <- liftIO $ getXdgDirectory XdgConfig appName
  home <- liftIO getHomeDirectory
  pure $ xdg </> makeRelative home repoDir

getOldConfigPath :: MonadIO m => FilePath -> m FilePath
getOldConfigPath repoDir = do
  oldConfigDir <- getOldConfigDir repoDir
  pure $ oldConfigDir </> configFileName

getNewConfigDir :: FilePath -> FilePath
getNewConfigDir repoDir = repoDir </> ("." <> appName)

getNewConfigPath :: FilePath -> FilePath
getNewConfigPath repoDir = getNewConfigDir repoDir </> configFileName

askPermissionToMoveConfig :: MonadIO m => FilePath -> FilePath -> m Bool
askPermissionToMoveConfig oldConfigPath newConfigPath = do
  liftIO $
    putDoc
      [qc|We've detected an existing config file in the old location: 
{pretty oldConfigPath}

The new location is:
{pretty newConfigPath}

Would you like to automatically move the config file to the new location? [Y/n] |]
  liftIO $ hFlush stdout
  response <- liftIO getLine
  if map toLower response `elem` ["y", "yes"]
    then pure True
    else pure False

isDirectoryEmpty :: FilePath -> IO Bool
isDirectoryEmpty path = do
  entries <- listDirectory path
  return $ null entries

getConfigPath :: MonadIO m => m FilePath
getConfigPath = do
  repoDir <- getRepoDir
  oldConfigPath <- getOldConfigPath repoDir
  let newConfigPath = getNewConfigPath repoDir
  oldConfigExists <- liftIO $ doesFileExist oldConfigPath
  if oldConfigExists
    then do
      permitted <- askPermissionToMoveConfig oldConfigPath newConfigPath
      if permitted
        then do
          liftIO $ createDirectoryIfMissing True $ takeDirectory newConfigPath 
          liftIO $ renameFile oldConfigPath newConfigPath
          liftIO $ putDoc "Config file moved successfully."

          -- also remove parent dir if it's empty
          let oldConfigDir = takeDirectory oldConfigPath
          isEmpty <- liftIO $ isDirectoryEmpty oldConfigDir
          when isEmpty $ 
            liftIO $ removeDirectory oldConfigDir

          pure newConfigPath
        else pure oldConfigPath
    else pure newConfigPath

-- returns config file location and its content, if file it doesn't exist creates one
configInit :: MonadIO m => m (FilePath, [Syntax C])
configInit = liftIO do
  trace "configInit"
  configPath <- getConfigPath
  let configDir = takeDirectory configPath
  configDirExists <- doesDirectoryExist configDir
  unless configDirExists do
    debug $ "create directory" <+> pretty configDir
    createDirectoryIfMissing True configDir
  configExists <- doesFileExist configPath
  unless configExists do
    appendFile configPath ""
  config <- readFile configPath <&> parseTop <&> either mempty id
  pure (configPath, config)
