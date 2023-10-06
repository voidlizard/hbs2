{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Peers where

import HBS2.Actors.Peer
import HBS2.Data.Types.Peer
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Service
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated

import HBS2.Net.Proto.Definition()

import PeerTypes

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import Control.Monad
import Lens.Micro.Platform
import Data.Maybe


instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcPeers where
  type instance Input RpcPeers = ()
  type instance Output RpcPeers = [(PubKey 'Sign HBS2Basic, PeerAddr L4Proto)]

  handleMethod _ = do
   co <- getRpcContext @PeerAPI
   withPeerM (rpcPeerEnv co) $ do
      ps <-  getKnownPeers @L4Proto
      r <- forM ps $ \p -> do
              mpde <- find (KnownPeerKey p) id
              maybe1 mpde (pure Nothing) $ \pde -> do
                pa <- toPeerAddr p
                let k = view peerSignKey pde
                pure $ Just (k, pa)

      pure $ catMaybes r


