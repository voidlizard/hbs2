module HBS2.Share.Config
  ( module Data.Config.Suckless.KeyValue
  , appName
  , confDirName
  , getWorkingDir
  , getLocalConfigDir'
  , getLocalConfigDir
  , getLocalStatePath
  , getLocalConfigDir'
  , getLocalConfigFile'
  , getLocalConfigFile
  , readConfig
  , IgnoreOpt
  , RefChanOpt
  , RpcUnixOpt
  , SigilPathOpt
  ) where

import HBS2.Prelude.Plated
import HBS2.OrDie

import HBS2.Share.App.Types

import Data.Config.Suckless
import Data.Config.Suckless.KeyValue

import System.Directory
import System.FilePath
import Data.Either
import Data.Set (Set)
import UnliftIO


data IgnoreOpt

data RefChanOpt

data RpcUnixOpt

data SigilPathOpt

instance HasCfgKey IgnoreOpt (Set String) where
  key = "ignore"

instance HasCfgKey RefChanOpt (Maybe RChan) where
  key = "refchan"

instance HasCfgKey RpcUnixOpt (Maybe String) where
  key = "rpc.unix"

instance HasCfgKey SigilPathOpt (Maybe String) where
  key = "sigil"

appName :: FilePath
appName = "hbs2-share"

confDirName :: FilePath
confDirName = "." <> appName

getWorkingDir :: MonadUnliftIO m => m FilePath
getWorkingDir = getLocalConfigDir <&> takeDirectory

getLocalConfigDir' :: MonadIO m => m FilePath
getLocalConfigDir' = pure confDirName


getLocalConfigDir :: MonadIO m => m FilePath
getLocalConfigDir = findLocalConfDir confDirName
                      >>= orThrowUser "config not found"

getLocalConfigFile' :: MonadIO m => m FilePath
getLocalConfigFile' = getLocalConfigDir' <&> (</> "config")

getLocalConfigFile :: MonadIO m => m FilePath
getLocalConfigFile = do
  dir <- findLocalConfDir confDirName
            >>= orThrowUser "config not found"
  pure $ dir </> "config"

getLocalStatePath :: MonadIO m => m FilePath
getLocalStatePath = do
  path <- findLocalConfDir confDirName
            >>= orThrowUser "config not found"
  pure ( path </> "state.db" )

readConfig :: MonadIO m => m [Syntax C]
readConfig = do
 liftIO $ try @_ @IOError (getLocalConfigFile >>= readFile)
  <&> fromRight ""
  <&> parseTop
  <&> fromRight mempty


findLocalConfDir :: MonadIO m => FilePath -> m (Maybe FilePath)
findLocalConfDir filename = liftIO $ do
  homeDir <- getHomeDirectory
  currentDir <- getCurrentDirectory
  findRecursively (</> filename) currentDir homeDir
  where
    findRecursively _ currentDir homeDir
      | currentDir == homeDir = return Nothing
      | otherwise = do
        let searchDir = currentDir </> filename
        dirExists <- doesDirectoryExist searchDir
        if dirExists
          then return $ Just searchDir
          else findRecursively (</> filename) (takeDirectory currentDir) homeDir


