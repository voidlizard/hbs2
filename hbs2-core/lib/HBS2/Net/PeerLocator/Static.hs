module HBS2.Net.PeerLocator.Static where

import HBS2.Prelude
import HBS2.Net.Proto
import HBS2.Net.PeerLocator

import Control.Concurrent.STM
import Data.Set (Set)
import Data.Set qualified as Set

import Prettyprinter

newtype StaticPeerLocator e =
  StaticPeerLocator (TVar (Set (Peer e)))


newStaticPeerLocator :: (Ord (Peer p), HasPeer p, MonadIO m) => [Peer p] -> m (StaticPeerLocator p)
newStaticPeerLocator seeds = do
  tv <- liftIO $ newTVarIO (Set.fromList seeds)
  pure $ StaticPeerLocator tv

instance (Ord (Peer e), Pretty (Peer e)) => PeerLocator  e (StaticPeerLocator e)  where

  knownPeers (StaticPeerLocator peers) = do
    ps <- liftIO $ readTVarIO  peers
    pure $ Set.toList ps

  addPeers (StaticPeerLocator peers) new = do
    liftIO $ atomically $ modifyTVar' peers (<> Set.fromList new)

  delPeers (StaticPeerLocator peers) del = do
    liftIO $ atomically $ modifyTVar' peers (`Set.difference` Set.fromList del)


