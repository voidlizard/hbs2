module RPC2.Fetch where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import Fetch
import RPC2.Types

data RpcFetch

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcFetch where
  type instance Input RpcFetch = HashRef
  type instance Output RpcFetch = ()

  handleMethod href = do
    co <- getRpcContext @RPC2Context
    debug $ "rpc2.fetch:"  <+> pretty href
    fetch (rpcPeerEnv co) (rpcDownloadEnv co) href



