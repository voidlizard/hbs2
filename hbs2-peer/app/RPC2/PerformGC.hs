{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.PerformGC where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import PeerLogger

import System.Mem

instance ( MonadIO m
         , HasRpcContext PeerAPI RPC2Context m)

  => HandleMethod m RpcPerformGC where

  handleMethod _ = do
    debug $ "rpc.performGC"
    liftIO performGC
    pure ()


