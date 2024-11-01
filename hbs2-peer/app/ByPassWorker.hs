{-# Language RecordWildCards #-}
module ByPassWorker where

import HBS2.Prelude
import HBS2.Clock
import HBS2.Actors.Peer
import HBS2.Net.Messaging.Encrypted.ByPass
import HBS2.Misc.PrettyStuff

import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.PeerExchange
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types

import PeerTypes

import Data.HashMap.Strict qualified as HM
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
             -> m ()

byPassWorker bp@ByPass{..} = do

  info $ green "byPassWorker started"

  flip runContT pure do

    void $ ContT $ withAsync $ forever do
      pause @'Seconds 60
      stats <- getStat bp
      info $ "ByPass stats"
        <> line
        <> indent 2 (pretty stats)
        <> line


    void $ ContT $ withAsync $ forever do
      pause @'Seconds 10
      p <- readTVarIO probe
      acceptReport p =<< do
        h <- readTVarIO heySent <&> ("heysSent",) . fromIntegral . HM.size
        n <- readTVarIO noncesByPeer <&> ("noncesByPeer",) .  fromIntegral . HM.size
        f <- readTVarIO flowKeys <&> ("flowKeys",) . fromIntegral . HM.size
        pure [h,n,f]

    forever do
      pips <- lift getKnownPeers
      cleanupByPassMessaging bp pips
      pause @'Seconds 600



