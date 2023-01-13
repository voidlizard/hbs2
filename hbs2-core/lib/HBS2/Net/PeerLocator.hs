module HBS2.Net.PeerLocator where

-- import HBS2.Prelude
import HBS2.Net.Proto

class PeerLocator l where
  knownPeers :: (IsPeer peer, Monad m) => l -> m [Peer peer]

data AnyPeerLocator = forall a . PeerLocator a => AnyPeerLocator a

instance PeerLocator AnyPeerLocator where
  knownPeers (AnyPeerLocator l) = knownPeers  l


