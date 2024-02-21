{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Poke where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import PeerLogger

instance ( MonadIO m
         , HasRpcContext PeerAPI RPC2Context m)

  => HandleMethod m RpcPoke where

  handleMethod n = do
    co <- getRpcContext @PeerAPI
    debug $ "rpc.poke: alive and kicking!" <+> pretty n
    pure $ rpcPokeAnswer co



