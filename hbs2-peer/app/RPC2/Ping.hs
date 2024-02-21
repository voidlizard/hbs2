{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module RPC2.Ping where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Net.Proto.Service

import HBS2.Peer.RPC.Internal.Types

import PeerTypes
import HBS2.Peer.RPC.API.Peer


instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcPing where

  handleMethod pa = do
    co <- getRpcContext @PeerAPI
    debug $ "rpc.ping:"  <+> pretty pa
    liftIO $ withPeerM (rpcPeerEnv co) $ do
      pingPeerWait pa


