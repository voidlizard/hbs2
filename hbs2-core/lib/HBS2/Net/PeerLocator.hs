module HBS2.Net.PeerLocator where

-- import HBS2.Prelude
import HBS2.Net.Proto

class PeerLocator l where
  knownPeers :: (HasPeer p, Monad m) => l -> m [Peer p]

data AnyPeerLocator = forall a . PeerLocator a => AnyPeerLocator a

instance PeerLocator AnyPeerLocator where
  knownPeers (AnyPeerLocator l) = knownPeers  l



