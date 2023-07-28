{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.Peer where

-- import HBS2.Base58
import HBS2.Actors.Peer
import HBS2.Data.Types
import HBS2.Events
import HBS2.Net.Proto
import HBS2.Net.Proto.Types
import HBS2.Clock
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.System.Logger.Simple

import Control.Monad
import Crypto.Saltine.Core.Box qualified as Encrypt
import Data.Maybe
import Codec.Serialise()
import Data.ByteString qualified as BS
import Data.Hashable
import Data.String.Conversions (cs)
import Lens.Micro.Platform
import Type.Reflection (someTypeRep)

data PeerHandshake e =
    PeerPing  PingNonce
  | PeerPong  PingNonce (Signature (Encryption e)) (PeerData e)
  deriving stock (Generic)

deriving instance
    ( Show (PubKey 'Encrypt (Encryption e))
    , Show (Signature (Encryption e))
    , Show (PeerData e)
    )
    => Show (PeerHandshake e)

newtype KnownPeer e = KnownPeer (PeerData e)
  deriving stock (Typeable,Generic)

newtype instance SessionKey e  (KnownPeer e) =
  KnownPeerKey (Peer e)
  deriving stock (Generic,Typeable)

data PeerPingData e =
  PeerPingData
  { _peerPingNonce :: PingNonce
  , _peerPingSent  :: TimeSpec
  }
  deriving stock (Generic,Typeable)

makeLenses 'PeerPingData

type instance SessionData e (KnownPeer e) = PeerData e

newtype instance SessionKey e (PeerHandshake e) =
  PeerHandshakeKey (PingNonce, Peer e)
  deriving stock (Generic, Typeable)

type instance SessionData e (PeerHandshake e) = PeerPingData e


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
                         , e ~ L4Proto
                         )
         => Peer e -> m ()

sendPing pip = do
  nonce <- newNonce @(PeerHandshake e)
  tt <- liftIO $ getTimeCoarse
  let pdd = PeerPingData nonce tt
  update pdd (PeerHandshakeKey (nonce,pip)) id
  request pip (PeerPing @e nonce)

newtype PeerHandshakeAdapter e m =
  PeerHandshakeAdapter
  { onPeerRTT :: (Peer e, Integer) -> m ()
  }


peerHandShakeProto :: forall e s m . ( MonadIO m
                                     , Response e (PeerHandshake e) m
                                     , Request e (PeerHandshake e) m
                                     , Sessions e (PeerHandshake e) m
                                     , Sessions e (KnownPeer e) m
                                     , HasNonces (PeerHandshake e) m
                                     , HasPeerNonce e m
                                     , Nonce (PeerHandshake e) ~ PingNonce
                                     , Pretty (Peer e)
                                     , EventEmitter e (PeerHandshake e) m
                                     , EventEmitter e (ConcretePeer e) m
                                     , HasCredentials s m
                                     , Asymm s
                                     , Signatures s
                                     , Serialise (PubKey 'Encrypt (Encryption e))
                                     , s ~ Encryption e
                                     , e ~ L4Proto
                                     )
                   => PeerHandshakeAdapter e m
                   -> PeerEnv e
                   -> PeerHandshake e
                   -> m ()

peerHandShakeProto adapter penv =
  \case
    PeerPing nonce  -> do
      pip <- thatPeer proto
      -- взять свои ключи
      creds <- getCredentials @s

      -- подписать нонс
      let sign = makeSign @s (view peerSignSk creds) nonce

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

      maybe1 se' (pure ()) $ \(PeerPingData nonce t0) -> do

        let pk = view peerSignKey d

        let signed = verifySign @s pk sign nonce

        when signed $ do

          now <- liftIO getTimeCoarse
          let rtt = toNanoSecs $ now - t0

          onPeerRTT adapter (pip,rtt)

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

---

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

instance ( Serialise (PubKey 'Sign (Encryption e))
         , Serialise (PubKey 'Encrypt (Encryption e))
         , Serialise (Signature (Encryption e))
         , Serialise PeerNonce
         )

  => Serialise (PeerData e)

instance ( Serialise (PubKey 'Sign (Encryption e))
         , Serialise (PubKey 'Encrypt (Encryption e))
         , Serialise (Signature (Encryption e))
         , Serialise PeerNonce
         )

  => Serialise (PeerHandshake e)


---

data EncryptionKeyIDKey e =
  EncryptionKeyIDKey
  { ekeyIDPeerSignKey :: PubKey 'Sign (Encryption e)
  , ekeyIDPeerNonce :: PeerNonce
  }
  deriving (Generic)

deriving instance
  ( Show (PubKey 'Sign (Encryption e))
  , Show (Nonce ())
  ) => Show (EncryptionKeyIDKey e)

deriving instance
  ( Eq (PubKey 'Sign (Encryption e))
  , Eq (Nonce ())
  ) => Eq (EncryptionKeyIDKey e)

instance (
    Hashable (PubKey 'Sign (Encryption e))
  , Hashable (Nonce ())
  ) => Hashable (EncryptionKeyIDKey e) where
  hashWithSalt s EncryptionKeyIDKey {..} =
      hashWithSalt s (ekeyIDPeerSignKey, ekeyIDPeerNonce)

encryptionKeyIDKeyFromPeerData :: PeerData e -> EncryptionKeyIDKey e
encryptionKeyIDKeyFromPeerData PeerData{..} =
    EncryptionKeyIDKey
        { ekeyIDPeerSignKey = _peerSignKey
        , ekeyIDPeerNonce = _peerOwnNonce
        }
