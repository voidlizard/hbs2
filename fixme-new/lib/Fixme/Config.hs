module Fixme.Config where

import Fixme.Prelude
import Fixme.Types

import HBS2.System.Dir
import System.Environment

binName :: FixmePerks m => m FilePath
binName = liftIO getProgName

localConfigDir :: FixmePerks m => m FilePath
localConfigDir = do
  p <- pwd
  b <- binName
  pure (p </> ("." <> b))

localConfig:: FixmePerks m => m FilePath
localConfig = localConfigDir <&> (</> "config")

localDBName :: FilePath
localDBName = "state.db"

localDBPath :: FixmePerks m => m FilePath
localDBPath = localConfigDir <&> (</> localDBName)

