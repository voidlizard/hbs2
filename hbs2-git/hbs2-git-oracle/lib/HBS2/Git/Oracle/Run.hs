module HBS2.Git.Oracle.Run where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App

import Data.Maybe

runOracle :: MonadUnliftIO m => Oracle m ()
runOracle = do
  debug "hbs2-git-oracle"

  debug "list all git references from peer"
  -- TODO: introduce-paging

  peer <- asks _peerAPI

  polls <- callRpcWaitMay @RpcPollList (TimeoutSec 1) peer ()
            <&> join . maybeToList


  for_ polls $ \(p, s, _) -> do
    debug $ "found poll" <+> pretty (AsBase58 p) <+> pretty s


