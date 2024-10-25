{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module RPC2.Probes where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Internal.Types

import UnliftIO

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcGetProbes where

  handleMethod _ = do
    probes <- getRpcContext @PeerAPI
               <&> rpcProbes
               >>= readTVarIO

    debug $ "rpc.getProbes"
    probeSnapshot probes

