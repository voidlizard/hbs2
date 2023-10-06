{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module RPC2.Ping where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
-- import HBS2.Actors.Peer.Types
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import HBS2.Peer.RPC.Internal.Types

import PeerTypes
import HBS2.Peer.RPC.API.Peer


instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcPing where
  type instance Input RpcPing = PeerAddr L4Proto
  type instance Output RpcPing = Bool

  handleMethod pa = do
    co <- getRpcContext @PeerAPI
    debug $ "rpc2.ping:"  <+> pretty pa
    liftIO $ withPeerM (rpcPeerEnv co) $ do
      pingPeerWait pa


