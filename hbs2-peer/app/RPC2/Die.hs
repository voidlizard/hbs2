module RPC2.Die where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import HBS2.Peer.RPC.API.Peer

import System.Exit qualified as Exit
import Control.Concurrent.Async


instance (MonadIO m) => HandleMethod m RpcDie where

  handleMethod _ = do
    debug $ "rpc.die: exiting"
    void $ liftIO $ do
      w <- async $ pause @'Seconds 0.5 >> Exit.exitSuccess
      link w



