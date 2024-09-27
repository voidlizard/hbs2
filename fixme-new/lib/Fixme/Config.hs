module Fixme.Config where

import Fixme.Prelude
import Fixme.Types

import HBS2.System.Dir
import System.Environment
import System.Directory (getXdgDirectory, XdgDirectory(..))

binName :: FixmePerks m => m FilePath
binName = liftIO getProgName

localConfigDir :: (FixmePerks m, MonadReader FixmeEnv m) =>  m FilePath
localConfigDir = do
  p <- asks fixmeEnvWorkDir >>= readTVarIO
  b <- binName
  pure (p </> ("." <> b))

fixmeWorkDir :: (FixmePerks m, MonadReader FixmeEnv m) => m FilePath
fixmeWorkDir = asks fixmeEnvWorkDir >>= readTVarIO

localConfig:: (FixmePerks m, MonadReader FixmeEnv m) => m FilePath
localConfig = localConfigDir <&> (</> "config")

userConfigs :: FixmePerks m => m [FilePath]
userConfigs= do
  bin <- binName
  h <- home
  xdg <- liftIO (getXdgDirectory XdgConfig bin)

  let conf1 = h </> ("." <> bin)
  let conf2 = xdg </> "config"

  pure [conf2, conf1]

localDBName :: FilePath
localDBName = "state.db"

localDBPath :: (FixmePerks m, MonadReader FixmeEnv m) => m FilePath
localDBPath = localConfigDir <&> (</> localDBName)

