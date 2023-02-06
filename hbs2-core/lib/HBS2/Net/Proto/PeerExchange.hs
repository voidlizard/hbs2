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

import Data.ByteString qualified as BS
import Data.Traversable
import Data.Functor
import Data.Maybe
import Codec.Serialise
import Data.Hashable
import Type.Reflection

import HBS2.System.Logger.Simple
import Prettyprinter

data PeerExchange e =
    PeerExchangeGet (Nonce (PeerExchange e))
  | PeerExchangePeers (Nonce (PeerExchange e)) [PeerAddr e]
  deriving stock (Generic, Typeable)

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
  request pip (PeerExchangeGet @e nonce)

peerExchangeProto :: forall e m . ( MonadIO m
                                  , Response e (PeerExchange e) m
                                  , HasPeerLocator e m
                                  , HasDeferred e (PeerExchange e) m
                                  , HasNonces (PeerExchange e) m
                                  , IsPeerAddr e m
                                  , Sessions e (KnownPeer e) m
                                  , Sessions e (PeerExchange e) m
                                  , EventEmitter e (PeerExchangePeersEv e) m
                                  , Eq (Nonce (PeerExchange e))
                                  , Pretty (Peer e)
                                  )
                  => PeerExchange e -> m ()

peerExchangeProto =
  \case
    PeerExchangeGet n -> deferred proto do
      -- TODO:  sort peers by their usefulness

      that <- thatPeer proto

      debug $ "PeerExchangeGet" <+> "from" <+> pretty that

      pl   <- getPeerLocator @e
      pips <- knownPeers @e pl

      pa'   <- forM pips $ \p -> do
                 auth <- find (KnownPeerKey p) id <&> isJust
                 if auth then do
                   a <- toPeerAddr p
                   pure [a]
                 else
                   pure mempty

      let pa = take defPexMaxPeers $ mconcat pa'

      response (PeerExchangePeers @e n pa)

    PeerExchangePeers nonce pips -> do

      pip <- thatPeer proto

      ok <- find (PeerExchangeKey @e nonce) id <&> isJust

      when ok do
        sa <- mapM (fromPeerAddr @e) pips
        debug $ "got pex" <+> "from" <+> pretty pip <+> pretty sa
        expire @e (PeerExchangeKey nonce)
        emit @e PeerExchangePeersKey (PeerExchangePeersData sa)

   where
    proto = Proxy @(PeerExchange e)


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
  expiresIn _ = Just 10


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


