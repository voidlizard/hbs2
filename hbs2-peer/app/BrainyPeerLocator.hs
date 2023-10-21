{-# Language UndecidableInstances #-}
module BrainyPeerLocator
  ( BrainyPeerLocator
  , newBrainyPeerLocator
  ) where


import HBS2.Prelude
import HBS2.Net.Proto
import HBS2.Net.PeerLocator
import HBS2.Peer.Brains

import Control.Concurrent.STM
import Data.Set (Set)
import Data.Set qualified as Set


data BrainyPeerLocator =
  BrainyPeerLocator
  { brains  :: SomeBrains L4Proto
  , include :: TVar (Set (Peer L4Proto))
  , exclude :: TVar (Set (Peer L4Proto))
  }

newBrainyPeerLocator :: forall e m . (Ord (Peer e), HasPeer e, e ~ L4Proto, MonadIO m)
                     => SomeBrains e
                     -> [Peer e]
                     -> m BrainyPeerLocator

newBrainyPeerLocator brains seeds = do
  tv  <- liftIO $ newTVarIO (Set.fromList seeds)
  tv2 <- liftIO $ newTVarIO mempty
  pure $ BrainyPeerLocator brains tv tv2

instance (Ord (Peer L4Proto), Pretty (Peer L4Proto)) => PeerLocator L4Proto BrainyPeerLocator where

  knownPeers (BrainyPeerLocator br peers e) = do


    ps <- liftIO $ readTVarIO peers

    excl <- liftIO $ readTVarIO e
    pure $ Set.toList (ps `Set.difference` excl)

  knownPeersForPEX l@(BrainyPeerLocator br _ e) = do

    excl <- liftIO $ readTVarIO e

    pips <- knownPeers @L4Proto l
            <&> filter udpOnly
            <&> Set.fromList

    tcp <- listTCPPexCandidates @L4Proto br
             >>= liftIO . mapM (fromPeerAddr @L4Proto)
             <&> Set.fromList

    let what = Set.toList ( (pips <> tcp) `Set.difference` excl)

    addr <- liftIO $ mapM (toPeerAddr @L4Proto) what

    updatePexInfo br addr

    pure what

    where
      udpOnly = \case
        (PeerL4 UDP _) -> True
        _              -> False


  addPeers (BrainyPeerLocator _ peers te) new = do
    excl <- liftIO $ readTVarIO te
    liftIO $ atomically $ modifyTVar' peers ((`Set.difference` excl) . (<> Set.fromList new))

  delPeers (BrainyPeerLocator _ peers _) del = do
    liftIO $ atomically $ modifyTVar' peers (`Set.difference` Set.fromList del)

  addExcluded p excl = do
    liftIO $ atomically $ modifyTVar' (exclude p)  (<> Set.fromList excl)

  bestPeers = knownPeers
