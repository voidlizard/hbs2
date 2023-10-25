module ByPassWorker where

import HBS2.Prelude
import HBS2.Clock
import HBS2.Actors.Peer
import HBS2.Net.Messaging.Encrypted.ByPass
import HBS2.System.Logger.Simple


import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerExchange
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types

import PeerTypes

import Control.Monad
import UnliftIO


byPassWorker :: ( ForByPass e
                , MonadUnliftIO m
                , MonadIO m
                , HasPeer e
                , HasPeerLocator e m
                , Sessions e (KnownPeer e) m
                , Expires (SessionKey e (KnownPeer e))
                )
             => ByPass e w
             -> PeerEnv e
             -> m ()

byPassWorker bp penv = do

  tstat <- async $ forever do
    stats <- getStat bp
    info $ "ByPass stats"
      <> line
      <> indent 2 (pretty stats)
      <> line

    pause @'Seconds 60

  link tstat

  gc <- async $ withPeerM penv $ forever do
          pips <- getKnownPeers
          cleanupByPassMessaging bp pips
          pause @'Seconds 600

  link gc

  void $ waitAnyCatchCancel [tstat, gc]




