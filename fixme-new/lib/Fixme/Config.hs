module Fixme.Config where

import Fixme.Prelude
import Fixme.Types

import HBS2.System.Dir
import System.Environment
import System.Directory (getXdgDirectory, XdgDirectory(..))

binName :: FixmePerks m => m FilePath
binName = liftIO getProgName

localConfigDir :: FixmePerks m => m FilePath
localConfigDir = do
  p <- pwd
  b <- binName
  pure (p </> ("." <> b))

fixmeWorkDir :: FixmePerks m => m FilePath
fixmeWorkDir = localConfigDir <&> takeDirectory >>= canonicalizePath

localConfig:: FixmePerks m => m FilePath
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

localDBPath :: FixmePerks m => m FilePath
localDBPath = localConfigDir <&> (</> localDBName)

