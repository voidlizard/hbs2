{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module HBS2.Net.PeerLocator.Static
  ( StaticPeerLocator
  , newStaticPeerLocator
  , PeerLocator()
  ) where

import HBS2.Prelude
import HBS2.Net.Proto
import HBS2.Net.PeerLocator

import Control.Concurrent.STM
import Data.Set (Set)
import Data.Set qualified as Set


data StaticPeerLocator e =
  StaticPeerLocator
  { include :: TVar (Set (Peer e))
  , exclude :: TVar (Set (Peer e))
  }


newStaticPeerLocator :: (Ord (Peer p), HasPeer p, MonadIO m) => [Peer p] -> m (StaticPeerLocator p)
newStaticPeerLocator seeds = do
  tv  <- liftIO $ newTVarIO (Set.fromList seeds)
  tv2 <- liftIO $ newTVarIO mempty
  pure $ StaticPeerLocator tv tv2

instance (Ord (Peer e), Pretty (Peer e)) => PeerLocator  e (StaticPeerLocator e)  where

  knownPeers (StaticPeerLocator peers e) = do
    ps <- liftIO $ readTVarIO  peers
    excl <- liftIO $ readTVarIO e
    pure $ Set.toList (ps `Set.difference` excl)

  addPeers (StaticPeerLocator peers te) new = do
    excl <- liftIO $ readTVarIO te
    liftIO $ atomically $ modifyTVar' peers ((`Set.difference` excl) . (<> Set.fromList new))

  delPeers (StaticPeerLocator peers _) del = do
    liftIO $ atomically $ modifyTVar' peers (`Set.difference` Set.fromList del)

  addExcluded p excl = do
    liftIO $ atomically $ modifyTVar' (exclude p)  (<> Set.fromList excl)

  bestPeers = knownPeers

