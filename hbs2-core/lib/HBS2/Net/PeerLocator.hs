module HBS2.Net.PeerLocator where

import HBS2.Net.Proto

class (IsPeer peer, Monad m) => PeerLocator l peer m where
  knownPeers :: l -> m [Peer peer]


