module HBS2.KeyMan.Config
  ( keymanAppName
  , getConfigPath
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
import Data.Set (Set)


data KeyFilesOpt

keymanAppName :: FilePath
keymanAppName = "hbs2-keyman"

getConfigPath :: MonadIO m => m FilePath
getConfigPath = liftIO (getXdgDirectory XdgConfig keymanAppName) <&> (</> "config")


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

