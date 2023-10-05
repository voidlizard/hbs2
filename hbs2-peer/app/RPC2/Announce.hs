{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Announce where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import SendBlockAnnounce

import HBS2.Peer.RPC.Internal.Types
import RPC2.Peer.API


instance (MonadIO m,HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcAnnounce where
  type instance Input RpcAnnounce = HashRef
  type instance Output RpcAnnounce = ()

  handleMethod href = do
    co <- getRpcContext @PeerAPI
    debug $ "rpc2.announce:"  <+> pretty href
    sendBlockAnnounce (rpcPeerEnv co) (rpcLocalMultiCast co) (fromHashRef href)



