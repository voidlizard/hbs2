module Watchdogs (runRpcWatchDog) where

import HBS2.Prelude
import HBS2.Misc.PrettyStuff
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Client.Unix

import PeerTypes

import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Cont

import UnliftIO


data WState =
  WIdle | WCall Int

runRpcWatchDog :: MonadIO m => ThreadId -> FilePath -> m ()
runRpcWatchDog peer soname = do
    liftIO $ flip runContT pure do

      api <- ContT $ withRPC2 @PeerAPI soname

      flip fix WIdle $ \next -> \case

        WIdle -> do
          pause @'Seconds 10
          next (WCall 0)

        WCall n | n > 2 -> do
          err $ red "RpcWatchDog fired"
          throwTo peer GoAgainException

        WCall n -> do
          debug $ "RpcWatchDog" <+> pretty n
          liftIO (callRpcWaitMay @RpcPoke (TimeoutSec 2) api ()) >>= \case
            Just _ -> next WIdle
            Nothing -> next (WCall (succ n))

    throwTo peer GoAgainException


