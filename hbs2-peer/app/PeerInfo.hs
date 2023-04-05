{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
module PeerInfo where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Defaults
import HBS2.Events
import HBS2.Net.Messaging.UDP
import HBS2.Net.PeerLocator
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerExchange
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types
import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple

import PeerConfig

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader
import Data.Foldable hiding (find)
import Data.IntSet (IntSet)
import Data.List qualified as List
import Data.Maybe
import Lens.Micro.Platform
import Numeric (showGFloat)
import System.Random.Shuffle


data PeerPingIntervalKey

-- TODO: ping-interval-specifically-for-peer
instance HasCfgKey PeerPingIntervalKey (Maybe Integer) where
  key = "ping-interval"

data PeerInfo e =
  PeerInfo
  { _peerBurst          :: TVar Int
  , _peerBurstMax       :: TVar (Maybe Int)
  , _peerBurstSet       :: TVar IntSet
  , _peerErrors         :: TVar Int
  , _peerErrorsLast     :: TVar Int
  , _peerErrorsPerSec   :: TVar Int
  , _peerLastWatched    :: TVar TimeSpec
  , _peerDownloaded     :: TVar Int
  , _peerDownloadedLast :: TVar Int
  , _peerPingFailed     :: TVar Int
  , _peerDownloadedBlk  :: TVar Int
  , _peerDownloadFail   :: TVar Int
  , _peerDownloadMiss   :: TVar Int
  , _peerUsefulness     :: TVar Double
  , _peerRTTBuffer      :: TVar [Integer] -- ^ Contains a list of the last few round-trip time (RTT) values, measured in nanoseconds.
                                          -- Acts like a circular buffer.
  }
  deriving stock (Generic,Typeable)

makeLenses 'PeerInfo

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

newPeerInfo :: MonadIO m => m (PeerInfo e)
newPeerInfo = liftIO do
  PeerInfo <$> newTVarIO defBurst
           <*> newTVarIO Nothing
           <*> newTVarIO mempty
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO []

type instance SessionData e (PeerInfo e) = PeerInfo e

newtype instance SessionKey e  (PeerInfo e) =
  PeerInfoKey (Peer e)

deriving newtype instance Hashable (SessionKey UDP (PeerInfo UDP))
deriving stock instance Eq (SessionKey UDP (PeerInfo UDP))

-- FIXME: this?
instance Expires (SessionKey UDP (PeerInfo UDP)) where
  expiresIn = const (Just defCookieTimeoutSec)

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

      let rttMs = (/1e6) <$> rtt <&> (\x -> showGFloat (Just 2) x "") <&> (<> "ms")

      notice $ "peer" <+> pretty p <+> "burst:" <+> pretty burst
                                   <+> "burst-max:" <+> pretty buM
                                   <+> "errors:" <+> pretty (downFails + errors)
                                   <+> "down:" <+> pretty down
                                   <+> "miss:" <+> pretty downMiss
                                   <+> "rtt:" <+> pretty rttMs
      pure ()



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
      npi <- newPeerInfo

      here <- find @e (KnownPeerKey p) id

      pinfo <- fetch True npi (PeerInfoKey p) id
      let pfails = view peerPingFailed pinfo
      let pdownfails = view peerDownloadFail pinfo

      -- FIXME: seems-like-a-bad-idea
      --   Кажется, вызывает гонки
      liftIO $ atomically $ modifyTVar pfails succ

      sendPing @e p

      fnum <- liftIO $ readTVarIO pfails
      fdown <- liftIO $ readTVarIO pdownfails

      when (fnum > 4) do -- FIXME: hardcode!
        warn $ "removing peer" <+> pretty p <+> "for not responding to our pings"
        delPeers pl [p]
        expire (PeerInfoKey p)
        expire (KnownPeerKey p)


