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

import Data.Maybe
import Data.Foldable hiding (find)
import Lens.Micro.Platform
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent.Async
import System.Random.Shuffle
import Prettyprinter

data PeerInfo e =
  PeerInfo
  { _peerBurst          :: TVar Int
  , _peerErrors         :: TVar Int
  , _peerErrorsLast     :: TVar Int
  , _peerErrorsPerSec   :: TVar Int
  , _peerLastWatched    :: TVar TimeSpec
  , _peerDownloaded     :: TVar Int
  , _peerDownloadedLast :: TVar Int
  , _peerPingFailed     :: TVar Int
  }
  deriving stock (Generic,Typeable)

makeLenses 'PeerInfo


newPeerInfo :: MonadIO m => m (PeerInfo e)
newPeerInfo = liftIO do
  PeerInfo <$> newTVarIO defBurst
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0

type instance SessionData e (PeerInfo e) = PeerInfo e

newtype instance SessionKey e  (PeerInfo e) =
  PeerInfoKey (Peer e)

deriving newtype instance Hashable (SessionKey UDP (PeerInfo UDP))
deriving stock instance Eq (SessionKey UDP (PeerInfo UDP))

instance Expires (SessionKey UDP (PeerInfo UDP)) where
  expiresIn = const (Just 600)


pexLoop :: forall e m . ( HasPeerLocator e m
                        , HasPeer e
                        , Sessions e (KnownPeer e) m
                        , HasNonces (PeerExchange e) m
                        , Request e (PeerExchange e) m
                        , Sessions e (PeerExchange e) m
                        , MonadIO m
                        ) => m ()

pexLoop = do

  pl <- getPeerLocator @e

  forever do
    pause @'Seconds 20 -- FIXME: defaults

    pips <- knownPeers @e pl

    peers' <- forM pips $ \p -> do
                au <- find @e (KnownPeerKey p) id
                pure $ maybe1 au mempty (const [p])

    peers <- liftIO (shuffleM (mconcat peers')) <&> take 10 -- FIXME: defaults

    for_ peers sendPeerExchangeGet


peerPingLoop :: forall e m . ( HasPeerLocator e m
                             , HasPeer e
                             , HasNonces (PeerHandshake e) m
                             , Nonce (PeerHandshake e) ~ PingNonce
                             , Request e (PeerHandshake e) m
                             , Sessions e (PeerHandshake e) m
                             , Sessions e (PeerInfo e) m
                             , Sessions e (KnownPeer e) m
                             , EventListener e (PeerExchangePeersEv e) m
                             , Pretty (Peer e)
                             , MonadIO m
                             )
             => m ()
peerPingLoop = do

  wake <- liftIO newTQueueIO

  subscribe @e PeerExchangePeersKey $ \(PeerExchangePeersData sas) -> do
    liftIO $ atomically $ writeTQueue wake sas
    pure ()

  forever do

    -- FIXME: defaults
    void $ liftIO $ race (pause @'Seconds 20)
                         (atomically $ readTQueue wake)

    sas <- liftIO $ atomically $ flushTQueue wake <&> mconcat

    debug "peerPingLoop"

    pl <- getPeerLocator @e
    pips <- knownPeers @e pl <&> (<> sas)

    for_ pips $ \p -> do
      npi <- newPeerInfo
      pfails <- fetch True npi (PeerInfoKey p) (view peerPingFailed)
      liftIO $ atomically $ modifyTVar pfails succ
      sendPing @e p
      pause @'Seconds 2 -- NOTE: it's okay?

      fnum <- liftIO $ readTVarIO pfails

      when (fnum > 3) do -- FIXME: hardcode!
        warn $ "removing peer" <+> pretty p <+> "for not responding to our pings"
        delPeers pl [p]
        expire (PeerInfoKey p)
        expire (KnownPeerKey p)

