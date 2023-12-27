{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.PeerExchange where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.PeerLocator
import HBS2.Net.Proto.Sessions
import HBS2.Events
import HBS2.Clock
import HBS2.Defaults
import HBS2.Net.IP.Addr

import Control.Monad
import Data.Functor
import Data.Maybe
import Codec.Serialise
import Data.Hashable
import Type.Reflection
import Data.List qualified as L

import HBS2.System.Logger.Simple


data PexVersion = PEX1 | PEX2

data PeerExchange e =
    PeerExchangeGet (Nonce (PeerExchange e))
  | PeerExchangePeers (Nonce (PeerExchange e)) [IPAddrPort e]
  | PeerExchangeGet2 (Nonce (PeerExchange e))
  | PeerExchangePeers2 (Nonce (PeerExchange e)) [PeerAddr e]
  deriving stock (Generic, Typeable)

deriving instance
  ( Show (Nonce (PeerExchange e))
  , Show (PeerAddr e)
  ) => Show (PeerExchange e)

data PeerExchangePeersEv e



sendPeerExchangeGet :: forall e m . ( MonadIO m
                                    , HasNonces (PeerExchange e) m
                                    , Request e (PeerExchange e) m
                                    , Sessions e (PeerExchange e) m
                                    )
                  => Peer e -> m ()

sendPeerExchangeGet pip = do
  nonce <- newNonce @(PeerExchange e)
  update nonce (PeerExchangeKey @e nonce) id
  -- FIXME: about-to-delete
  request pip (PeerExchangeGet @e nonce)
  request pip (PeerExchangeGet2 @e nonce)

peerExchangeProto :: forall e m proto . ( MonadIO m
                                        , Response e proto m
                                        , HasPeerLocator e m
                                        , HasDeferred proto e m
                                        , HasNonces proto m
                                        , IsPeerAddr e m
                                        , Sessions e (KnownPeer e) m
                                        , Sessions e proto m
                                        , EventEmitter e (PeerExchangePeersEv e) m
                                        , Eq (Nonce proto)
                                        , Pretty (Peer e)
                                        , e ~ L4Proto
                                        , proto ~ PeerExchange e
                                        )
                  => PeerExchange e
                  -> m ()

peerExchangeProto msg = do
  case msg of
    PeerExchangeGet n -> peerExchangeGet PEX1 n
    PeerExchangeGet2 n -> peerExchangeGet PEX2 n
    PeerExchangePeers nonce pips -> peerExchangePeers1 nonce pips
    PeerExchangePeers2 nonce pips -> peerExchangePeers2 nonce pips

   where
    proto = Proxy @(PeerExchange e)

    fromPEXAddr1 = fromPeerAddr . L4Address UDP

    peerExchangePeers1 nonce pips = do
      pip <- thatPeer proto

      ok <- find (PeerExchangeKey @e nonce) id <&> isJust

      when ok do
        sa <- mapM fromPEXAddr1 pips
        debug $ "got pex" <+> "from" <+> pretty pip <+> pretty sa
        expire @e (PeerExchangeKey nonce)
        emit @e PeerExchangePeersKey (PeerExchangePeersData  sa)

    peerExchangePeers2 nonce pips = do
      pip <- thatPeer proto

      ok <- find (PeerExchangeKey @e nonce) id <&> isJust

      when ok do
        sa <- mapM fromPeerAddr pips
        debug $ "got pex" <+> "from" <+> pretty pip <+> pretty sa
        expire @e (PeerExchangeKey nonce)
        emit @e PeerExchangePeersKey (PeerExchangePeersData  sa)

    peerExchangeGet pex n = deferred @proto do
      that <- thatPeer proto

      debug $ "PeerExchangeGet" <+> "from" <+> pretty that

      case pex of
        PEX1 -> do
          pa <- take defPexMaxPeers <$> getAllPex1Peers
          response (PeerExchangePeers @e n pa)

        PEX2 -> do
          pa <- take defPexMaxPeers <$> getAllPex2Peers
          response (PeerExchangePeers2 @e n pa)

getAllPex1Peers :: forall e m .
  ( MonadIO m
  , Sessions e (KnownPeer e) m
  , HasPeerLocator L4Proto m
  , e ~ L4Proto
  )
  => m [IPAddrPort L4Proto]
getAllPex1Peers = do
    pl   <- getPeerLocator @e
    pips <- knownPeersForPEX @e pl
    -- TODO: tcp-peer-support-in-pex
    pa' <- forM pips $ \p -> do
                auth <- find (KnownPeerKey p) id <&> isJust
                pa <- toPeerAddr p
                case pa of
                    (L4Address UDP x) | auth -> pure [x]
                    _ -> pure mempty
    pure $ mconcat pa'

type PexInfoContext e m = ( Sessions e (KnownPeer e) m
                          , HasPeerLocator L4Proto m
                          )

getAllPex2Peers :: forall e m .
  ( MonadIO m
  , PexInfoContext e m
  , e ~ L4Proto
  )
  => m [PeerAddr L4Proto]
getAllPex2Peers = do
    pl   <- getPeerLocator @e
    pips <- knownPeersForPEX @e pl
    -- FIXME: random-shuffle
    forM pips toPeerAddr

newtype instance SessionKey e (PeerExchange e) =
  PeerExchangeKey (Nonce (PeerExchange e))
  deriving stock (Generic, Typeable)

type instance SessionData e (PeerExchange e) = Nonce (PeerExchange e)

data instance EventKey e (PeerExchangePeersEv e) =
  PeerExchangePeersKey
  deriving stock (Typeable, Eq,Generic)

deriving instance Eq (Nonce (PeerExchange e)) => Eq (SessionKey e (PeerExchange e))
instance Hashable (Nonce (PeerExchange e)) => Hashable (SessionKey e (PeerExchange e))

instance Expires (SessionKey e (PeerExchange e)) where
  expiresIn _ = Just 60

instance Typeable (PeerExchangePeersEv e)
  => Hashable (EventKey e (PeerExchangePeersEv e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @(PeerExchangePeersEv e)

instance EventType ( Event e ( PeerExchangePeersEv e) ) where
  isPersistent = True

instance Expires (EventKey e (PeerExchangePeersEv e)) where
  expiresIn _ = Nothing

newtype instance Event e (PeerExchangePeersEv e) =
  PeerExchangePeersData [Peer e]
  deriving stock (Typeable)

instance ( Serialise (PeerAddr e)
         , Serialise (Nonce (PeerExchange e)))

  => Serialise (PeerExchange e)


