module RPC2.Ping where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import PeerTypes
import RPC2.Types

data RpcPing

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcPing where
  type instance Input RpcPing = PeerAddr L4Proto
  type instance Output RpcPing = Bool

  handleMethod pa = do
    co <- getRpcContext @RPC2Context
    debug $ "rpc2.ping:"  <+> pretty pa
    liftIO $ withPeerM (rpcPeerEnv co) $ do
      pingPeerWait pa


