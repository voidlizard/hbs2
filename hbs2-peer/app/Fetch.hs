module Fetch where

import HBS2.Prelude
import HBS2.Actors.Peer
import HBS2.Events
import HBS2.Data.Types.Refs
import HBS2.Storage.Operations.Missed
import HBS2.Net.Proto.Types

import PeerTypes

fetchHash :: forall e m . ( e ~ L4Proto
                          , MonadIO m
                          )
      => PeerEnv e
      -> HashRef
      -> m ()

fetchHash penv href  = do
  debug  $ "fetchAction" <+> pretty h
  liftIO $ withPeerM penv $ do
    sto <- getStorage
    missed <- findMissedBlocks sto href
    for_ missed $ \miss -> do
       addDownload @e Nothing (fromHashRef miss)

  where
    h = fromHashRef href

