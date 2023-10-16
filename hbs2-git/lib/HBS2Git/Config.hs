module HBS2Git.Config
  ( module HBS2Git.Config
  , module Data.Config.Suckless
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.System.Logger.Simple
import HBS2.OrDie

import Data.Config.Suckless

import HBS2Git.Types

import Control.Applicative

import Data.Functor
import System.FilePath
import System.Directory
import Data.Maybe
import Data.Either
import Data.List (isSuffixOf)

import System.Environment

import System.IO (stderr)

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

configPathOld :: MonadIO m => FilePath -> m FilePath
configPathOld pwd = liftIO do
  xdg <- liftIO $ getXdgDirectory XdgConfig appName
  home <- liftIO getHomeDirectory
  pure $ xdg </> makeRelative home pwd

configPath :: MonadIO m => FilePath -> m FilePath
configPath _ = liftIO do
  pwd <- liftIO getCurrentDirectory
  git <- findGitDir pwd
  byEnv <- lookupEnv "GIT_DIR"

  bare <- if isJust (git <|> byEnv) then do
            pure Nothing
          else do
            -- check may be it's a bare git repo
            gitConf <- readFile "config"
                         <&> parseTop
                         <&> fromRight mempty

            let core = or [True | SymbolVal @C "core" <- universeBi gitConf]
            let bare = or [True | ListVal [SymbolVal @C "bare", _, SymbolVal "true"] <- universeBi gitConf ]
            let repo = or [True | SymbolVal @C "repositoryformatversion" <- universeBi gitConf ]

            if core && bare && repo then do
                pure $ Just pwd
            else
                pure Nothing

  path <- pure (dropSuffix <$> (git <|> byEnv <|> bare)) `orDie`  "*** hbs2-git: .git directory not found"

  pure (path </> ".hbs2")

  where
    dropSuffix s | isSuffixOf ".git/" s = takeDirectory s
                 | isSuffixOf ".git" s  = takeDirectory s
                 | otherwise = s

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

