module HBS2Git.Config where

import HBS2.Prelude
import HBS2.System.Logger.Simple

import Data.Config.Suckless

import System.FilePath
import System.Directory
import System.IO

configPath :: MonadIO m => m FilePath
configPath = do
  xdg <- liftIO $ getXdgDirectory XdgConfig "hbs2-git"
  pwd <- liftIO (getCurrentDirectory >>= canonicalizePath)
  home <- liftIO getHomeDirectory
  pure $ xdg </> makeRelative home pwd

configInit :: MonadIO m => m ()
configInit = liftIO do
  trace "configInit"

  confP <- configPath

  trace $ "confPath:" <+> pretty confP

  here <- doesDirectoryExist confP

  unless here do
    debug $ "create directory" <+> pretty confP
    createDirectoryIfMissing True confP



