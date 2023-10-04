module RPC2.PexInfo where


import HBS2.Actors.Peer
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Service
import HBS2.Prelude.Plated

import HBS2.Net.Proto.PeerExchange

import RPC2.Types

data RpcPexInfo

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcPexInfo where
  type instance Input RpcPexInfo = ()
  type instance Output RpcPexInfo = [PeerAddr L4Proto]

  handleMethod _ = do
   co <- getRpcContext @RPC2Context
   withPeerM (rpcPeerEnv co) getAllPex2Peers



