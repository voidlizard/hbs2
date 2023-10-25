{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module RPC2.ByPassStat where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Internal.Types

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcByPassInfo where

  handleMethod _ = do
    co <- getRpcContext @PeerAPI
    debug $ "rpc.byPassInfo"
    liftIO $ rpcByPassInfo co



