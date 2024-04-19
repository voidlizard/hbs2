module HBS2.Git.DashBoard.State.Index.Peer where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types

updateIndexFromPeer :: (DashBoardPerks m, HasConf m, MonadReader DashBoardEnv m) => m ()
updateIndexFromPeer = do
  debug "updateIndexFromPeer"
