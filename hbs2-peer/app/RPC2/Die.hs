module RPC2.Die where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple
import Data.Config.Suckless.KeyValue

import RPC2.Types
import System.Exit qualified as Exit
import Control.Concurrent.Async

data RpcDie

instance (MonadIO m) => HandleMethod m RpcDie where
  type instance Input RpcDie = ()
  type instance Output RpcDie = ()

  handleMethod _ = do
    debug $ "rpc2.die: exiting"
    void $ liftIO $ do
      w <- async $ pause @'Seconds 0.5 >> Exit.exitSuccess
      link w



