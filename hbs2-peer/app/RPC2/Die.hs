{-# Language UndecidableInstances #-}
module RPC2.Die where

import HBS2.Prelude.Plated
import HBS2.Peer.RPC.Internal.Types
import HBS2.Clock
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import HBS2.Peer.RPC.API.Peer

import Control.Concurrent
import System.Exit qualified as Exit

instance (MonadIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcDie where

  handleMethod _ = do
    RPC2Context{..} <- getRpcContext @PeerAPI
    debug $ "rpc.die: exiting"
    void $ liftIO $ do
      killThread rpcSelf



