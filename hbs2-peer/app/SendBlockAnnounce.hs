module SendBlockAnnounce where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Actors.Peer
import HBS2.Storage(Storage(..))
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.BlockAnnounce

import PeerTypes

import HBS2.System.Logger.Simple

sendBlockAnnounce :: forall e m . (e ~ L4Proto, MonadIO m)
                  => PeerEnv e
                  -> Peer e
                  -> Hash HbSync
                  -> m ()

sendBlockAnnounce env mcast h = liftIO $ withPeerM env do
  debug $ "got announce rpc" <+> pretty h
  sto <- getStorage
  mbsize <- liftIO $ hasBlock sto h

  maybe1 mbsize (pure ()) $ \size -> do
    debug "send multicast announce"

    no <- peerNonce @e
    let annInfo = BlockAnnounceInfo 0 NoBlockInfoMeta size h
    let announce = BlockAnnounce @e no annInfo

    request mcast announce

    forKnownPeers $ \p _ -> do
      debug $ "send single-cast announces" <+> pretty p
      request @e p announce

