{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.PexInfo where

import HBS2.Actors.Peer
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Service
import HBS2.Prelude.Plated
import HBS2.Net.Proto.Definition()

import HBS2.Net.Proto.PeerExchange

import HBS2.Peer.RPC.Internal.Types
import RPC2.Peer.API

instance ( MonadIO m
         , HasRpcContext PeerAPI RPC2Context m
         ) => HandleMethod m RpcPexInfo where
  type instance Input RpcPexInfo = ()
  type instance Output RpcPexInfo = [PeerAddr L4Proto]

  handleMethod _ = do
   co <- getRpcContext @PeerAPI
   withPeerM (rpcPeerEnv co) getAllPex2Peers



