{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.PeerAnnounce where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Events

import Type.Reflection (someTypeRep)
import Data.Hashable
import Codec.Serialise (Serialise)

-- This subprotocol is assumed to work with a
-- multicast address for local peer discovery.
--
-- For single cast case seems that PeerHandshake
-- subprotocol is sufficient:
-- peer Bob pings peer Alice,
-- now both of them know each other.
--
-- For making life easier in a local network,
-- we introduce PeerAnnounce subprotocol.
--
-- The idea is following:
-- Peer sends PeerAnnounce to a multicast address,
-- all available peers send their pings and now
-- they all know this peer.
--

newtype PeerAnnounce e =
  PeerAnnounce PeerNonce
  deriving stock (Typeable, Generic)

deriving instance Show (Nonce ()) => Show (PeerAnnounce e)


peerAnnounceProto :: forall e m  . ( MonadIO m
                                   , EventEmitter e (PeerAnnounce e) m
                                   , Response e (PeerAnnounce e) m
                                   ) => PeerAnnounce e -> m ()
peerAnnounceProto =
  \case
    PeerAnnounce nonce -> do
      who <- thatPeer @(PeerAnnounce e)
      emit @e PeerAnnounceEventKey (PeerAnnounceEvent who nonce)


data instance EventKey e (PeerAnnounce e) =
  PeerAnnounceEventKey
  deriving stock (Typeable, Eq,Generic)

data instance Event e (PeerAnnounce e) =
  PeerAnnounceEvent (Peer e) PeerNonce
  deriving stock (Typeable)

instance Typeable (PeerAnnounce e) => Hashable (EventKey e (PeerAnnounce e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @(PeerAnnounce e)

instance EventType ( Event e ( PeerAnnounce e) ) where
  isPersistent = True


instance Serialise PeerNonce => Serialise (PeerAnnounce e)

