{-# Language AllowAmbiguousTypes #-}
module HBS2.Net.PeerLocator where

import HBS2.Prelude
import HBS2.Net.Proto.Types

import System.Random.Shuffle (shuffleM)

class PeerLocator e l where
  knownPeers   :: forall m . (HasPeer e, MonadIO m) => l -> m [Peer e]
  addPeers     :: forall m . (HasPeer e, MonadIO m) => l -> [Peer e] -> m ()
  delPeers     :: forall m . (HasPeer e, MonadIO m) => l -> [Peer e] -> m ()
  bestPeers    :: forall m . (HasPeer e, MonadIO m) => l -> m [Peer e]
  addExcluded  :: forall m . (HasPeer e, MonadIO m) => l -> [Peer e] -> m ()

data AnyPeerLocator e = forall a . PeerLocator e a => AnyPeerLocator a

instance HasPeer e => PeerLocator e (AnyPeerLocator e) where
  knownPeers (AnyPeerLocator l) = knownPeers  l
  addPeers (AnyPeerLocator l) = addPeers l
  delPeers (AnyPeerLocator l) = delPeers l
  addExcluded (AnyPeerLocator l) = addExcluded l

  -- FIXME: a better algorithm of choice
  bestPeers (AnyPeerLocator l) = liftIO $ knownPeers l >>= shuffleM

class Monad m => HasPeerLocator e m where
  getPeerLocator ::  m (AnyPeerLocator e)
