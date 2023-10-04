module RPC2.Announce where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import SendBlockAnnounce
import RPC2.Types

data RpcAnnounce

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcAnnounce where
  type instance Input RpcAnnounce = HashRef
  type instance Output RpcAnnounce = ()

  handleMethod href = do
    co <- getRpcContext @RPC2Context
    debug $ "rpc2.announce:"  <+> pretty href
    sendBlockAnnounce (rpcPeerEnv co) (rpcLocalMultiCast co) (fromHashRef href)



