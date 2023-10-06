{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Poke where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer


instance ( MonadIO m
         , HasRpcContext PeerAPI RPC2Context m)

  => HandleMethod m RpcPoke where
  type instance Input RpcPoke = ()
  type instance Output RpcPoke = String

  handleMethod n = do
    co <- getRpcContext @PeerAPI
    debug $ "rpc2.poke: alive and kicking!" <+> pretty n
    pure $ rpcPokeAnswer co



