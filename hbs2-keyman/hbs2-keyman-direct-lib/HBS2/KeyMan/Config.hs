module HBS2.KeyMan.Config
  ( keymanAppName
  , getConfigPath
  , getDefaultKeyPath
  , getDefaultKeyPath0
  , getDefaultKeyMask
  , getStatePath
  , readConfig
  , KeyFilesOpt
  , Set
  ) where

import HBS2.Prelude.Plated

import Data.Config.Suckless

import System.Directory
import System.FilePath
import Control.Exception
import Data.Text.IO qualified as Text
import Data.Either
import Data.Maybe
import Data.Set (Set)
import HBS2.System.Dir (mkdir)


data KeyFilesOpt

keymanAppName :: FilePath
keymanAppName = "hbs2-keyman"

getConfigPath :: MonadIO m => m FilePath
getConfigPath = liftIO (getXdgDirectory XdgConfig keymanAppName) <&> (</> "config")

getDefaultKeyPath0 :: MonadIO m => m FilePath
getDefaultKeyPath0 = do
   -- TODO: Use xdg path?
   homeDirectory <- liftIO $ getHomeDirectory
   let defaultDirectory = homeDirectory </> ("." <> keymanAppName) </> "keys"
   pure defaultDirectory

getDefaultKeyPath :: MonadIO m => [Syntax C] -> m FilePath
getDefaultKeyPath config = do
  defaultDirectory <- getDefaultKeyPath0
  let path = [ p
             | ListVal [SymbolVal "default-key-path", StringLike p] <- config
             ] & headMay & fromMaybe defaultDirectory

  mkdir path
  return path

getDefaultKeyMask :: MonadIO m => [Syntax C] -> m String
getDefaultKeyMask config = do
  path <- getDefaultKeyPath config
  let mask = [ p
             | ListVal [SymbolVal "default-key-mask", StringLike p] <- config
             ] & headMay & fromMaybe "**/*.key"

  return $ path </> mask


getStatePath :: MonadIO m => m FilePath
getStatePath = liftIO (getXdgDirectory XdgData keymanAppName) <&> (</> "state.db")

readConfig :: MonadIO m => m [Syntax C]
readConfig = do
 liftIO $ try @IOError (getConfigPath >>= Text.readFile)
  <&> fromRight ""
  <&> parseTop
  <&> fromRight mempty

instance HasCfgKey KeyFilesOpt (Set String) where
  key = "key-files"
