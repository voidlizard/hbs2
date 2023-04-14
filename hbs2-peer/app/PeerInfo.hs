{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
module PeerInfo where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Events
import HBS2.Net.PeerLocator
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerExchange
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types
import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple

import PeerConfig
import PeerTypes

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Foldable hiding (find)
import Data.List qualified as List
import Data.Maybe
import Lens.Micro.Platform
import Numeric (showGFloat)
import System.Random.Shuffle


data PeerPingIntervalKey

-- TODO: ping-interval-specifically-for-peer
instance HasCfgKey PeerPingIntervalKey (Maybe Integer) where
  key = "ping-interval"


-- | Compute the median of a list
median :: (Ord a, Integral a) => [a] -> Maybe a
median [] = Nothing
median xs = Just
  if odd n
    then sorted !! half
    else ((sorted !! (half - 1)) + (sorted !! half)) `div` 2
  where n = length xs
        sorted = List.sort xs
        half = n `div` 2

-- | Get the median RTT for a given peer.
medianPeerRTT :: MonadIO m => PeerInfo e -> m (Maybe Integer)
medianPeerRTT pinfo = do
  rttBuffer <- liftIO $ readTVarIO (view peerRTTBuffer pinfo)
  pure $ median rttBuffer

rttBufferCapacity :: Int
rttBufferCapacity = 10

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

pexLoop :: forall e m . ( HasPeerLocator e m
                        , HasPeer e
                        , Sessions e (KnownPeer e) m
                        , HasNonces (PeerExchange e) m
                        , Request e (PeerExchange e) m
                        , Sessions e (PeerExchange e) m
                        , MonadIO m
                        ) => m ()

pexLoop = do

  pause @'Seconds 5

  pl <- getPeerLocator @e

  forever do

    pips <- knownPeers @e pl

    peers' <- forM pips $ \p -> do
                au <- find @e (KnownPeerKey p) id
                pure $ maybe1 au mempty (const [p])

    peers <- liftIO (shuffleM (mconcat peers')) <&> take 10 -- FIXME: defaults

    for_ peers sendPeerExchangeGet

    pause @'Seconds 180  -- FIXME: defaults

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
             => PeerConfig -> m ()
peerPingLoop cfg = do

  e <- ask

  pl <- getPeerLocator @e

  let pingTime = cfgValue @PeerPingIntervalKey cfg
                      & fromMaybe 30
                      & realToFrac

  wake <- liftIO newTQueueIO

  pause @'Seconds 0.25

  subscribe @e PeerExchangePeersKey $ \(PeerExchangePeersData sas) -> do
    liftIO $ atomically $ writeTQueue wake sas

  -- subscribe @e AnyKnownPeerEventKey $ \(KnownPeerEvent p _) -> do
  --   liftIO $ atomically $ writeTQueue wake [p]


  -- TODO: peer info loop
  void $ liftIO $ async $ forever $ withPeerM e $ do
    pause @'Seconds 10
    pee <- knownPeers @e pl

    npi <- newPeerInfo

    now <- getTimeCoarse

    debug $ "known peers" <+> pretty pee

    for_ pee $ \p -> do
      pinfo <- fetch True npi (PeerInfoKey p) id
      burst  <- liftIO $ readTVarIO (view peerBurst pinfo)
      buM    <- liftIO $ readTVarIO (view peerBurstMax pinfo)
      errors <- liftIO $ readTVarIO (view peerErrorsPerSec pinfo)
      downFails <- liftIO $ readTVarIO (view peerDownloadFail pinfo)
      downMiss  <- liftIO $ readTVarIO (view peerDownloadMiss pinfo)
      down      <- liftIO $ readTVarIO (view peerDownloadedBlk pinfo)
      rtt       <- liftIO $ medianPeerRTT pinfo <&> fmap realToFrac
      httpDownloaded <- liftIO $ readTVarIO (_peerHttpDownloaded pinfo)
      seen      <- liftIO $ readTVarIO (view peerLastWatched pinfo)
      let l = realToFrac (toNanoSecs $ now - seen) / 1e9

      let rttMs = (/1e6) <$> rtt <&> (\x -> showGFloat (Just 2) x "") <&> (<> "ms")
      let ls = showGFloat (Just 2) l "" <> "s"

      notice $ "peer" <+> pretty p <+> "burst:" <+> pretty burst
                                   <+> "burst-max:" <+> pretty buM
                                   <+> "errors:" <+> pretty (downFails + errors)
                                   <+> "down:" <+> pretty down
                                   <+> "miss:" <+> pretty downMiss
                                   <+> "rtt:" <+> pretty rttMs
                                   <+> "http:" <+> pretty httpDownloaded
                                   <+> "seen" <+> pretty ls
      pure ()


  watch <- liftIO $ async $ forever $ withPeerM e $ do
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
                  delPeers pl [p]
                  expire (PeerInfoKey p)
                  expire (KnownPeerKey p)

  liftIO $ link watch

  forever do

    -- FIXME: defaults
    r <- liftIO $ race (pause @'Seconds pingTime)
                       (atomically $ readTQueue wake)

    sas' <- liftIO $ atomically $ flushTQueue wake <&> mconcat

    let sas = case r of
          Left{}   -> sas'
          Right sa -> sa <> sas'

    debug "peerPingLoop"

    pips <- knownPeers @e pl <&> (<> sas) <&> List.nub

    for_ pips $ \p -> do
      trace $ "SEND PING TO" <+> pretty p
      sendPing @e p


