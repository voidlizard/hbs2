module RPC2.Poke where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple
import Data.Config.Suckless.KeyValue

import RPC2.Types

data RpcPoke

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcPoke where
  type instance Input RpcPoke = ()
  type instance Output RpcPoke = String

  handleMethod n = do
    co <- getRpcContext @RPC2Context
    debug $ "rpc2.poke: alive and kicking!" <+> pretty n
    pure $ rpcPokeAnswer co



