{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.Peer where

-- import HBS2.Base58
import HBS2.Data.Types
import HBS2.Events
import HBS2.Net.Proto
import HBS2.Clock
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials

-- import HBS2.System.Logger.Simple

import Data.Maybe
import Codec.Serialise()
import Data.ByteString qualified as BS
import Data.Hashable
import Lens.Micro.Platform
import Type.Reflection (someTypeRep)

type PingSign  e = Signature e
type PingNonce = BS.ByteString

data PeerData e =
  PeerData
  { _peerSignKey  :: PubKey 'Sign e
  , _peerOwnNonce :: PeerNonce -- TODO: to use this field to detect if it's own peer to avoid loops
  }
  deriving stock (Typeable,Generic)

makeLenses 'PeerData

data PeerHandshake e =
    PeerPing  PingNonce
  | PeerPong  PingNonce (Signature e) (PeerData e)
  deriving stock (Generic)

newtype KnownPeer e = KnownPeer (PeerData e)
  deriving stock (Typeable,Generic)

newtype instance SessionKey e  (KnownPeer e) =
  KnownPeerKey (Peer e)
  deriving stock (Generic,Typeable)

type instance SessionData e (KnownPeer e) = PeerData e

newtype instance SessionKey e (PeerHandshake e) =
  PeerHandshakeKey (PingNonce, Peer e)
  deriving stock (Generic, Typeable)

type instance SessionData e (PeerHandshake e) = PingNonce


-- FIXME: enormous-request-amount-during-handshake-2
--  несмотря на то, что проблема решается введением ReqLimPeriod
--  и HasTimeLimits, хорошо бы разобраться, что именно вызывает
--  шквал пингов и в какой момент (Pex? PeerAnnounce?)
--  это не очень правильное поведение, возможно там нужно
--  что-то делать с peerNonce

sendPing :: forall e m . ( MonadIO m
                         , Request e (PeerHandshake e) m
                         , Sessions e (PeerHandshake e) m
                         , HasNonces (PeerHandshake e) m
                         , Nonce (PeerHandshake e) ~ PingNonce
                         , Pretty (Peer e)
                         )
         => Peer e -> m ()

sendPing pip = do
  nonce <- newNonce @(PeerHandshake e)
  update nonce (PeerHandshakeKey (nonce,pip)) id
  request pip (PeerPing @e nonce)

peerHandShakeProto :: forall e m . ( MonadIO m
                                   , Response e (PeerHandshake e) m
                                   , Request e (PeerHandshake e) m
                                   , Sessions e (PeerHandshake e) m
                                   , Sessions e (KnownPeer e) m
                                   , HasNonces (PeerHandshake e) m
                                   , HasPeerNonce e m
                                   , Nonce (PeerHandshake e) ~ PingNonce
                                   , Signatures e
                                   , Pretty (Peer e)
                                   , HasCredentials e m
                                   , EventEmitter e (PeerHandshake e) m
                                   , EventEmitter e (ConcretePeer e) m
                                   )
                   => PeerHandshake e -> m ()

peerHandShakeProto =
  \case
    PeerPing nonce  -> do
      pip <- thatPeer proto
      -- взять свои ключи
      creds <- getCredentials @e

      -- подписать нонс
      let sign = makeSign @e (view peerSignSk creds) nonce

      own <- peerNonce @e

      -- отправить обратно вместе с публичным ключом
      response (PeerPong @e nonce sign (PeerData (view peerSignPk creds) own))

      -- да и пингануть того самим

      se <- find (KnownPeerKey pip) id <&> isJust

      unless se $ do
        sendPing pip

    PeerPong nonce0 sign d -> do
      pip <- thatPeer proto

      se' <- find @e (PeerHandshakeKey (nonce0,pip)) id

      maybe1 se' (pure ()) $ \nonce -> do

        let pk = view peerSignKey d

        let signed = verifySign @e pk sign nonce

        when signed $ do

          expire (PeerHandshakeKey (nonce0,pip))

          -- FIXME: check if peer is blacklisted
          --        right here
          update d (KnownPeerKey pip) id

          emit AnyKnownPeerEventKey (KnownPeerEvent pip d)
          emit (ConcretePeerKey pip) (ConcretePeerData pip d)

  where
    proto = Proxy @(PeerHandshake e)

data ConcretePeer e = ConcretePeer

newtype instance EventKey e (ConcretePeer e) =
  ConcretePeerKey (Peer e)
  deriving stock (Generic)

deriving stock instance (Eq (Peer e)) => Eq (EventKey e (ConcretePeer e))
instance (Hashable (Peer e)) => Hashable (EventKey e (ConcretePeer e))

data instance Event e (ConcretePeer e) =
  ConcretePeerData (Peer e) (PeerData e)
  deriving stock (Typeable)

data instance EventKey e (PeerHandshake e) =
  AnyKnownPeerEventKey
  deriving stock (Typeable, Eq,Generic)

data instance Event e (PeerHandshake e) =
  KnownPeerEvent (Peer e) (PeerData e)
  deriving stock (Typeable)

instance ( Typeable (KnownPeer e)
         , Eq (EventKey e (KnownPeer e))
         ) => Hashable (EventKey e (KnownPeer e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @(KnownPeer e)

instance EventType ( Event e ( PeerHandshake e) ) where
  isPersistent = True

instance Expires (EventKey e (PeerHandshake e)) where
  expiresIn _ = Nothing

instance Expires (EventKey e (ConcretePeer e)) where
  expiresIn _ = Just 60

instance Hashable (Peer e) => Hashable (EventKey e (PeerHandshake e))

deriving instance Eq (Peer e) => Eq (SessionKey e (KnownPeer e))
instance Hashable (Peer e) => Hashable (SessionKey e (KnownPeer e))

deriving instance Eq (Peer e) => Eq (SessionKey e (PeerHandshake e))
instance Hashable (Peer e) => Hashable (SessionKey e (PeerHandshake e))

instance ( Serialise (PubKey 'Sign e)
         , Serialise (Signature e)
         , Serialise PeerNonce
         )

  => Serialise (PeerData e)

instance ( Serialise (PubKey 'Sign e)
         , Serialise (Signature e)
         , Serialise PeerNonce
         )

  => Serialise (PeerHandshake e)

