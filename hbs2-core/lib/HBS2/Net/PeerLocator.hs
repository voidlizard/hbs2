{-# Language AllowAmbiguousTypes #-}
module HBS2.Net.PeerLocator where

import HBS2.Prelude
import HBS2.Net.Proto.Types

class PeerLocator e l where
  knownPeers :: forall m . (HasPeer e, MonadIO m) => l -> m [Peer e]
  addPeers   :: forall m . (HasPeer e, MonadIO m) => l -> [Peer e] -> m ()
  delPeers   :: forall m . (HasPeer e, MonadIO m) => l -> [Peer e] -> m ()

data AnyPeerLocator e = forall a . PeerLocator e a => AnyPeerLocator a

instance HasPeer e => PeerLocator e (AnyPeerLocator e) where
  knownPeers (AnyPeerLocator l) = knownPeers  l
  addPeers (AnyPeerLocator l) = addPeers l
  delPeers (AnyPeerLocator l) = addPeers l


