{-# Language TemplateHaskell #-}
module PeerInfo where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Sessions
import HBS2.Net.Messaging.UDP
import HBS2.Clock
import HBS2.Defaults

import Lens.Micro.Platform
import Control.Concurrent.STM.TVar


data PeerInfo e =
  PeerInfo
  { _peerBurst          :: TVar Int
  , _peerErrors         :: TVar Int
  , _peerErrorsLast     :: TVar Int
  , _peerErrorsPerSec   :: TVar Int
  , _peerLastWatched    :: TVar TimeSpec
  , _peerDownloaded     :: TVar Int
  , _peerDownloadedLast :: TVar Int
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

type instance SessionData e (PeerInfo e) = PeerInfo e

newtype instance SessionKey e  (PeerInfo e) =
  PeerInfoKey (Peer e)

deriving newtype instance Hashable (SessionKey UDP (PeerInfo UDP))
deriving stock instance Eq (SessionKey UDP (PeerInfo UDP))

instance Expires (SessionKey UDP (PeerInfo UDP)) where
  expiresIn = const (Just 600)


