{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Poll where

import HBS2.Peer.Prelude
import HBS2.Net.Proto.Service
import HBS2.Peer.Brains

import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Internal.Types

import PeerLogger

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcPollList where

  handleMethod _ = do
    brains <- getRpcContext @PeerAPI <&> rpcBrains
    debug $ "rpc.pollList"
    listPolledRefs @L4Proto brains Nothing


instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcPollAdd where

  handleMethod (r,t,i) = do
    brains <- getRpcContext @PeerAPI <&> rpcBrains
    debug $ "rpc.pollAdd"
    polled <- isPolledRef @L4Proto brains t r
    unless polled do
      addPolledRef @L4Proto brains r t i

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcPollDel where

  handleMethod r = do
    brains <- getRpcContext @PeerAPI <&> rpcBrains
    debug $ "rpc.pollDel"
    delPolledRef @L4Proto brains r


