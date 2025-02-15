module Monkeys where

import HBS2.Prelude
import HBS2.Net.Messaging.Encrypted.ByPass
import HBS2.Misc.PrettyStuff

import PeerTypes
import RPC2

import System.Mem
import Control.Monad.Trans.Cont
import UnliftIO


runMonkeys :: MonadUnliftIO m => RPC2Context -> m ()
runMonkeys RPC2Context{..} = flip runContT pure do

  pause @'Seconds 20

  void $ ContT $ withAsync idleMonkey
  forever do
    pause @'Seconds 300

  where

    idleSleep = 120

    idleMonkey = do
      flip fix 0 $ \next bytes0 -> do
        ByPassStat{..} <- liftIO rpcByPassInfo
        let bytes = statSentBytes + statReceived

        when ( bytes - bytes0 < 10 * 1024 * idleSleep ) do
          notice $ "Idle monkey says:" <+> green "IDLE"
          onIdle

        pause @'Seconds (realToFrac idleSleep)
        next bytes

    onIdle = do
      liftIO performMajorGC


