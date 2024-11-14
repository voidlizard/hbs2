{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TypeOperators #-}
module PeerInfo where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Data.Types
import HBS2.Events
import HBS2.Net.PeerLocator
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.PeerExchange
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types
import HBS2.Prelude.Plated

import HBS2.Net.Messaging.TCP

import PeerConfig
import PeerTypes
import Brains

-- import Control.Concurrent.Async
import Control.Concurrent.STM qualified as STM
import Control.Monad
import Control.Monad.Reader
import Data.Foldable hiding (find)
import Data.List qualified as List
import Data.Maybe
import Lens.Micro.Platform
import Numeric (showGFloat)
import System.Random.Shuffle
import Data.HashMap.Strict qualified as HashMap
import Data.Either
import Control.Monad.Trans.Cont
import Control.Exception qualified as E

import UnliftIO


data PeerPingIntervalKey

-- TODO: ping-interval-specifically-for-peer
instance HasCfgKey PeerPingIntervalKey (Maybe Integer) where
  key = "ping-interval"


-- | Get the median RTT for a given peer.
medianPeerRTT :: MonadIO m => PeerInfo e -> m (Maybe Integer)
medianPeerRTT pinfo = do
  rttBuffer <- liftIO $ readTVarIO (view peerRTTBuffer pinfo)
  pure $ median rttBuffer

rttBufferCapacity :: Int
rttBufferCapacity = 1024

-- | New values are added to the head of the list, old values are discarded when the list is full.
insertRTT :: MonadIO m => Integer -> TVar [Integer] -> m ()
insertRTT x rttList = do
  liftIO $ atomically $ modifyTVar rttList (\xs ->
    if rttBufferCapacity < 1
      then xs
      else if length xs < rttBufferCapacity
        then x:xs
        else x:init xs
    )

pexLoop :: forall e brains m . ( HasPeerLocator e m
                               , HasPeer e
                               , HasBrains e brains
                               , Sessions e (KnownPeer e) m
                               , HasNonces (PeerExchange e) m
                               , Request e (PeerExchange e) m
                               , Sessions e (PeerExchange e) m
                               , MonadUnliftIO m
                               , e ~ L4Proto
                               ) => brains -> Maybe MessagingTCP -> m ()

pexLoop brains tcpEnv = forever do

  pause @'Seconds 5

  pl <- getPeerLocator @e

  -- TODO: investigate-tcp-session-cleanup
  --  Есть подозрения, что TCP сессии не чистятся
  --  надлежащим образом. Требуется расследовать.

  flip runContT pure do

    -- NOTE: tcpPexInfo
    --   Этот кусок говорит Brains о том,
    --   какие TCP сессии есть в наличии.
    --   Убирать пока нельзя
    void $ ContT $ withAsync $ forever do
      -- FIXME: fix-hardcode
      pause @'Seconds 20

      pips <- knownPeers @e pl
      onKnownPeers brains pips

      conns <- maybe1 (view tcpPeerConn <$> tcpEnv) (pure mempty)  $ \tconn -> do
                 try @_ @SomeException $ readTVarIO tconn <&> HashMap.toList
                 >>= either (const $ warn "tcpSessionWait issue" >> pause @'Seconds 1 >> pure mempty) pure

      ssids <- forM conns $ \(p,coo) -> do
                  trace $ "ACTUAL TCP SESSIONS" <+> pretty p <+> pretty coo
                  pa <- toPeerAddr p
                  pure (pa, coo)

      setActiveTCPSessions @e brains ssids

      pure ()


  --     pee <- knownPeers @e pl
  --     npi <- newPeerInfo

  --     for_ pee $ \p -> do
  --       pinfo <- fetch True npi (PeerInfoKey p) id
  --       updatePeerInfo False p pinfo

    void $ ContT $ withAsync $ forever do

      pips <- knownPeers @e pl

      peers' <- forM pips $ \p -> do
                  au <- find @e (KnownPeerKey p) id
                  pure $ maybe1 au mempty (const [p])

      peers <- liftIO (shuffleM (mconcat peers')) <&> take 10 -- FIXME: defaults

      for_ peers sendPeerExchangeGet

      pause @'Seconds 180  -- FIXME: defaults

    forever do
      pips <- knownPeers @e pl
      onKnownPeers brains pips
      pause @'Seconds 60

peerPingLoop :: forall e m . ( HasPeerLocator e m
                             , HasPeer e
                             , HasNonces (PeerHandshake e) m
                             , Nonce (PeerHandshake e) ~ PingNonce
                             , Request e (PeerHandshake e) m
                             , Sessions e (PeerHandshake e) m
                             , Sessions e (PeerInfo e) m
                             , Sessions e (KnownPeer e) m
                             , EventListener e (PeerExchangePeersEv e) m
                             , EventListener e (PeerHandshake e) m
                             , Pretty (Peer e)
                             , MonadIO m
                             , m ~ PeerM e IO
                             , e ~ L4Proto
                             )
             => PeerConfig -> PeerEnv e -> m ()
peerPingLoop (PeerConfig syn) penv = do

  e <- ask

  pl <- getPeerLocator @e

  let pingTime = runReader (cfgValue @PeerPingIntervalKey) syn
                      & fromMaybe 30
                      & realToFrac

  wake <- liftIO newTQueueIO

  pause @'Seconds 0.25

  subscribe @e PeerExchangePeersKey $ \(PeerExchangePeersData sas) -> do
    liftIO $ atomically $ writeTQueue wake sas

  -- subscribe @e AnyKnownPeerEventKey $ \(KnownPeerEvent p _) -> do
  --   liftIO $ atomically $ writeTQueue wake [p]


  flip runContT pure do

    -- TODO: peer info loop
    void $ ContT $ withAsync $ forever $ withPeerM e $ do
      pause @'Seconds 10
      pee <- knownPeers @e pl

      npi <- newPeerInfo

      now <- getTimeCoarse

      debug $ "known peers" <+> pretty pee

      for_ pee $ \p -> do
        pinfo <- fetch True npi (PeerInfoKey p) id
        burst  <- liftIO $ readTVarIO (view peerBurst pinfo)
        errors <- liftIO $ readTVarIO (view peerErrorsPerSec pinfo)
        downMiss  <- liftIO $ readTVarIO (view peerDownloadMiss pinfo)
        down      <- liftIO $ readTVarIO (view peerDownloadedBlk pinfo)
        rtt       <- liftIO $ medianPeerRTT pinfo <&> fmap realToFrac
        httpDownloaded <- liftIO $ readTVarIO (_peerHttpDownloaded pinfo)
        seen      <- liftIO $ readTVarIO (view peerLastWatched pinfo)
        let l = realToFrac (toNanoSecs $ now - seen) / 1e9

        let rttMs = (/1e6) <$> rtt <&> (\x -> showGFloat (Just 2) x "") <&> (<> "ms")
        let ls = showGFloat (Just 2) l "" <> "s"

        notice $ "peer" <+> pretty p <+> "burst:" <+> pretty burst
                                     <+> "errors:" <+> pretty errors
                                     <+> "down:" <+> pretty down
                                     <+> "miss:" <+> pretty downMiss
                                     <+> "rtt:" <+> pretty rttMs
                                     <+> "http:" <+> pretty httpDownloaded
                                     <+> "seen" <+> pretty ls
        pure ()


    void $ ContT $ withAsync $ liftIO $ forever $ withPeerM e $ do
               pause @'Seconds 120
               pips <- getKnownPeers @e
               now <- getTimeCoarse
               for_ pips $ \p -> do
                 pinfo' <- find (PeerInfoKey p) id
                 maybe1 pinfo' none $ \pinfo -> do
                  seen <- liftIO $ readTVarIO (view peerLastWatched pinfo)
                  -- FIXME: do-something-with-this-nanosec-boilerplate-everywhere
                  let l = realToFrac (toNanoSecs $ now - seen) / 1e9
                  -- FIXME: time-hardcode
                  when ( l > 300 ) do
                    mpeerData <- find (KnownPeerKey p) id
                    delPeers pl [p]
                    expire (PeerInfoKey p)
                    expire (KnownPeerKey p)
                    emit PeerExpiredEventKey (PeerExpiredEvent @e p {-mpeerData-})


    forever do

      -- FIXME: defaults
      r <- liftIO $ race (pause @'Seconds pingTime)
                         (atomically $ readTQueue wake)

      sas' <- liftIO $ atomically $ STM.flushTQueue wake <&> mconcat

      let sas = case r of
            Left{}   -> sas'
            Right sa -> sa <> sas'

      debug "peerPingLoop"

      pips <- knownPeers @e pl <&> (<> sas) <&> List.nub

      for_ pips $ \p -> do
        -- trace $ "SEND PING TO" <+> pretty p
        lift $ sendPing @e p
        -- trace $ "SENT PING TO" <+> pretty p

