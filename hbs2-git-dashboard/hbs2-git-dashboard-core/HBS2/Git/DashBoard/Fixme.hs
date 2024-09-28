module HBS2.Git.DashBoard.Fixme where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State

import Fixme.State
import Fixme.Types

withFixme :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoLww -> FixmeM m a ->  m ()
withFixme repo m = do
  p <- fixmeDataPath (coerce repo)
  debug $ pretty p




