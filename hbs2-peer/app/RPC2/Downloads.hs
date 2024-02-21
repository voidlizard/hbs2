{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.Downloads where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service
import HBS2.Peer.Brains
import HBS2.Net.Proto.Definition()

import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Internal.Types

import PeerLogger

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcDownloadList where

  handleMethod _ = do
    brains <- getRpcContext @PeerAPI <&> rpcBrains
    debug $ "rpc.downloadList"
    listDownloads @L4Proto brains

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcDownloadDel where

  handleMethod href = do
    brains <- getRpcContext @PeerAPI <&> rpcBrains
    debug $ "rpc.downloadDel" <+> pretty href
    delDownload @L4Proto brains href

