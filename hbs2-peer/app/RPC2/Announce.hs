{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Announce where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Service

import SendBlockAnnounce

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import PeerLogger


instance (MonadIO m,HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcAnnounce where

  handleMethod href = do
    co <- getRpcContext @PeerAPI
    debug $ "rpc.announce:"  <+> pretty href
    sendBlockAnnounce (rpcPeerEnv co) (rpcLocalMultiCast co) (fromHashRef href)



