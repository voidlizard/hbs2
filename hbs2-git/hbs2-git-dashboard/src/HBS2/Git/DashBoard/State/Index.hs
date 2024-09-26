module HBS2.Git.DashBoard.State.Index
  ( module HBS2.Git.DashBoard.State.Index
  , module HBS2.Git.DashBoard.State.Index.Channels
  , module HBS2.Git.DashBoard.State.Index.Peer

  ) where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State.Index.Channels
import HBS2.Git.DashBoard.State.Index.Peer

updateIndex :: (DashBoardPerks m, MonadReader DashBoardEnv m) => m ()
updateIndex = do
  debug "updateIndex"
  updateIndexFromPeer
  updateIndexFromChannels



