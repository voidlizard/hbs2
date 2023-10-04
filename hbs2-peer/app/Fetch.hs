module Fetch where

import HBS2.Prelude
import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Types

import HBS2.System.Logger.Simple

import PeerTypes
import DownloadQ
import BlockDownload

fetch :: forall e m . (e ~ L4Proto, MonadIO m)
      => PeerEnv e
      -> DownloadEnv e
      -> HashRef
      -> m ()

fetch penv denv href  = do
  debug  $ "fetchAction" <+> pretty h
  liftIO $ withPeerM penv $ do
    downloadLogAppend @e h
    withDownload denv (processBlock h)
  where
    h = fromHashRef href

