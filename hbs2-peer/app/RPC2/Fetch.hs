{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Fetch where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcFetch where

  handleMethod href = do
    co <- getRpcContext @PeerAPI
    debug $ "rpc.fetch:"  <+> pretty href
    liftIO $ rpcDoFetch co href


