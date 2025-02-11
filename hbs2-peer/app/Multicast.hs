module Multicast where

import HBS2.Prelude

import PeerTypes

import HBS2.Actors.Peer
import HBS2.Defaults
import HBS2.Events
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Net.Messaging.UDP
import HBS2.Peer.Proto
import CheckBlockAnnounce (checkBlockAnnounce)
import PeerConfig

import HBS2.Misc.PrettyStuff
import HBS2.Peer.RPC.Internal.Types()
import HBS2.Peer.RPC.Internal.Storage()

import Data.Coerce
import Lens.Micro.Platform as Lens
import Control.Monad.Trans.Cont


import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception qualified as U
import UnliftIO.STM
import UnliftIO.Async
import UnliftIO.Concurrent (getNumCapabilities)

import Streaming.Prelude qualified as S


import Data.Kind

  -- menv <- newPeerEnv pl (AnyStorage s) (Fabriq mcast) (getOwnPeer mcast)
  -- do
  --   probe <- newSimpleProbe "PeerEnv_Announce"
  --   addProbe probe
  --   peerEnvSetProbe menv probe

  -- probesMenv <- liftIO $ async $ forever do
  --     pause @'Seconds 10
  --     peerEnvCollectProbes menv

  -- ann <- liftIO $ async $ runPeerM menv $ do

  --                  self <- ownPeer @e

  --                  subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
  --                   unless (p == self) do
  --                     pa <- toPeerAddr p
  --                     checkBlockAnnounce conf penv no pa (view biHash bi)

  --                  subscribe @e PeerAnnounceEventKey $ \pe@(PeerAnnounceEvent{}) -> do
  --                     -- debug $ "Got peer announce!" <+> pretty pip
  --                     emitToPeer penv PeerAnnounceEventKey pe

  --                  runProto @e
  --                    [ makeResponse blockAnnounceProto
  --                    , makeResponse peerAnnounceProto
  --                    ]


multicastWorker :: forall e s m . ( s ~ Encryption e
                                  , e ~ L4Proto
                                  , MonadUnliftIO m
                                  -- , HasStorage m
                                  -- , HasPeerLocator e m
                                  -- , HasPeerNonce L4Proto m
                                  )
                => PeerConfig -> PeerEnv e -> PeerM e m ()

multicastWorker conf penv = recover do

  debug $ red "multicastWorker started"

  sto <- getStorage
  pl <- getPeerLocator @e
  pnonce <- peerNonce @e

  localMCast_ <- liftIO newEmptyTMVarIO

  flip runContT pure do

    mcast' <- lift (newMessagingUDPMulticast defLocalMulticast)

    -- FIXME: log-on-exit
    mcast <- ContT $ maybe1 mcast' none

    messMcast <- ContT $ withAsync $ runMessagingUDP mcast

    menv <- newPeerEnv pl sto (Fabriq mcast) (getOwnPeer mcast)

    ann <- ContT $ withAsync $ do
            localMulticast <- atomically $ takeTMVar localMCast_
            forever do
              pips <- getKnownPeers @L4Proto
              let w = if null pips then 10 else defPeerAnnounceTime
              debug $ yellow "Sending local peer announce"
              request localMulticast (PeerAnnounce @e pnonce)
              pause w

    liftIO $ runPeerM menv $ do

      self <- ownPeer @e

      atomically $ putTMVar localMCast_ self

      subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
       unless (p == self) do
         pa <- toPeerAddr p
         checkBlockAnnounce (coerce conf) penv no pa (view biHash bi)

      subscribe @e PeerAnnounceEventKey $ \pe@(PeerAnnounceEvent p _) -> do
         debug $ green "Got peer announce from" <+> pretty p
         emitToPeer penv PeerAnnounceEventKey pe

      runProto @e
        [ makeResponse blockAnnounceProto
        , makeResponse peerAnnounceProto
        ]

  where
    recover m = U.catch (withPeerM penv m) \case
                  ( e :: IOError ) -> do
                    err $ "Multicast thread error" <+> viaShow e
                    let t = 10 :: Timeout 'Seconds
                    warn $ "Wait" <+> pretty t
                    pause @'Seconds 120
                    recover m



