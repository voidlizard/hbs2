{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.PexInfo where

import HBS2.Peer.Prelude
import HBS2.Actors.Peer
import HBS2.Net.Proto.Service

import HBS2.Peer.Proto
-- import HBS2.Peer.Proto.PeerExchange

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import Codec.Serialise

instance ( MonadIO m
         , HasRpcContext PeerAPI RPC2Context m
         , Serialise                        (Output RpcPexInfo)
         ) => HandleMethod m RpcPexInfo where

  handleMethod _ = do
   co <- getRpcContext @PeerAPI
   withPeerM (rpcPeerEnv co) getAllPex2Peers



