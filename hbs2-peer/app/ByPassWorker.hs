module ByPassWorker where

import HBS2.Prelude
import HBS2.Clock
import HBS2.Actors.Peer
import HBS2.Net.Messaging.Encrypted.ByPass

import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.PeerExchange
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types

import PeerTypes

import Control.Monad
import UnliftIO
import Control.Monad.Trans.Cont

byPassWorker :: ( ForByPass e
                , MonadUnliftIO m
                , HasPeer e
                , HasPeerLocator e m
                , Sessions e (KnownPeer e) m
                , Expires (SessionKey e (KnownPeer e))
                )
             => ByPass e w
             -> PeerEnv e
             -> m ()

byPassWorker bp penv = do

  flip runContT pure do

    void $ ContT $ withAsync $ forever do
      stats <- getStat bp
      info $ "ByPass stats"
        <> line
        <> indent 2 (pretty stats)
        <> line

      pause @'Seconds 60

  forever do
    pips <- getKnownPeers
    cleanupByPassMessaging bp pips
    pause @'Seconds 600



