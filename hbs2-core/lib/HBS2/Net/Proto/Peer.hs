{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.Peer where

import HBS2.Data.Types
import HBS2.Net.Proto
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated

import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS
import Lens.Micro.Platform
import Codec.Serialise()

type PingSign  e = Signature e
type PingNonce = BS.ByteString

newtype PeerData e =
  PeerData
  { _peerSignKey :: PubKey 'Sign e
  }
  deriving stock (Typeable,Generic)

makeLenses 'PeerData

newtype PeerAnnounce e =  PeerAnnounce (PeerData e)
                          deriving stock (Generic)

data PeerHandshake e =
    PeerPing  PingNonce
  | PeerPong  (PeerData e) (Signature e)
  deriving stock (Generic)

newtype KnownPeer e = KnownPeer (PeerData e)
  deriving stock (Typeable,Generic)

newtype instance SessionKey e  (KnownPeer e) =
  KnownPeerKey (Peer e)
  deriving stock (Generic,Typeable)

type instance SessionData e (KnownPeer e) = KnownPeer e

newtype instance SessionKey e (PeerHandshake e) =
  PeerHandshakeKey (Peer e)
  deriving stock (Generic, Typeable)

type instance SessionData e (PeerHandshake e) = (PingNonce, PeerData e)

peerHandShakeProto :: forall e m . ( MonadIO m
                                   , Response e (PeerHandshake e) m
                                   , Sessions e (PeerHandshake e) m
                                   , HasNonces (PeerHandshake e) m
                                   , Nonce (PeerHandshake e) ~ PingNonce
                                   , Signatures e
                                   , HasCredentials e m
                                   )
                   => PeerHandshake e -> m ()

peerHandShakeProto =
  \case
    PeerPing nonce  -> do
      pip <- thatPeer proto
      -- TODO: взять свои ключи
      -- TODO: подписать нонс
      -- TODO: отправить обратно вместе с публичным ключом
      --
      pure ()
      -- TODO: sign nonce
      -- se <- find @e (PeerHandshakeKey pip) id
      -- let signed = undefined
      -- TODO: answer
      -- response (PeerPong @e signed)

    PeerPong d sign -> do
      pure ()

      -- se' <- find @e (PeerHandshakeKey pip) id
      -- maybe1 se' (pure ()) $ \se -> do

      -- TODO: get peer data
      -- TODO: check signature

      -- ok <- undefined signed

      -- when ok $ do
        -- TODO: add peer to authorized peers
        -- pure ()

  where
    proto = Proxy @(PeerHandshake e)

deriving instance Eq (Peer e) => Eq (SessionKey e (KnownPeer e))
instance Hashable (Peer e) => Hashable (SessionKey e (KnownPeer e))

deriving instance Eq (Peer e) => Eq (SessionKey e (PeerHandshake e))
instance Hashable (Peer e) => Hashable (SessionKey e (PeerHandshake e))

instance ( Serialise (PubKey 'Sign e)
         , Serialise (Signature e) )

  => Serialise (PeerData e)

instance ( Serialise (PubKey 'Sign e)
         , Serialise (Signature e)
         )

  => Serialise (PeerHandshake e)

