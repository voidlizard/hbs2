module Fetch where

import HBS2.Prelude
import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Storage.Operations.Missed
import HBS2.Net.Proto.Types

import PeerTypes
import BlockDownload

import Data.Foldable (for_)

fetchHash :: forall e m . (e ~ L4Proto, MonadIO m)
      => PeerEnv e
      -> DownloadEnv e
      -> HashRef
      -> m ()

fetchHash penv denv href  = do
  debug  $ "fetchAction" <+> pretty h
  liftIO $ withPeerM penv $ do
    sto <- getStorage
    missed <- findMissedBlocks sto href
    for_ missed $ \miss -> do
      withDownload denv (processBlock (fromHashRef miss))

  where
    h = fromHashRef href

