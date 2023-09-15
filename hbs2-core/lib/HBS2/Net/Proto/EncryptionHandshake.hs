{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}

module HBS2.Net.Proto.EncryptionHandshake where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Data.Types
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple

import Crypto.Saltine.Core.Box qualified as Encrypt
import Data.ByteString qualified as BS
import Data.Hashable hiding (Hashed)
import Data.String.Conversions (cs)
import Lens.Micro.Platform

instance
    ( Show (PubKey 'Encrypt (Encryption e))
    , Show (PubKey 'Sign (Encryption e))
    , Show (Nonce ())
    )
    => Pretty (PeerData e) where
  pretty = viaShow

data EncryptionHandshake e =
    BeginEncryptionExchange (Signature (Encryption e)) (PubKey 'Encrypt (Encryption e))
  | AckEncryptionExchange (Signature (Encryption e)) (PubKey 'Encrypt (Encryption e))
  | ResetEncryptionKeys
  deriving stock (Generic)

sendResetEncryptionKeys :: forall e s m .
    ( MonadIO m
    , Request e (EncryptionHandshake e) m
    , e ~ L4Proto
    , s ~ Encryption e
    )
  => Peer e
  -> m ()

sendResetEncryptionKeys peer = do
    request peer (ResetEncryptionKeys @e)

sendBeginEncryptionExchange :: forall e s m .
    ( MonadIO m
    , Request e (EncryptionHandshake e) m
    , Sessions e (EncryptionHandshake e) m
    -- , HasCredentials s m
    , Asymm s
    , Signatures s
    , Serialise (PubKey 'Encrypt s)
    , Pretty (Peer e)
    , HasProtocol e (EncryptionHandshake e)
    , e ~ L4Proto
    , s ~ Encryption e
    )
  => PeerCredentials s
  -> PubKey 'Encrypt (Encryption e)
  -> Peer e
  -> m ()

sendBeginEncryptionExchange creds ourpubkey peer = do
    let sign = makeSign @s (view peerSignSk creds) ((cs . serialise) ourpubkey)
    request peer (BeginEncryptionExchange @e sign ourpubkey)

data EncryptionHandshakeAdapter e m s = EncryptionHandshakeAdapter
  { encHandshake_considerPeerAsymmKey :: Peer e -> Maybe Encrypt.PublicKey -> m ()

  , encAsymmetricKeyPair :: AsymmKeypair (Encryption e)

  , encGetEncryptionKey :: EncryptionKeyIDKey e -> m (Maybe (CommonSecret (Encryption e)))
  }


encryptionHandshakeProto :: forall e s m .
    ( MonadIO m
    , Response e (EncryptionHandshake e) m
    , Request e (EncryptionHandshake e) m
    , Sessions e (KnownPeer e) m
    , HasCredentials s m
    , Asymm s
    , Signatures s
    , Sessions e (EncryptionHandshake e) m
    , Serialise (PubKey 'Encrypt (Encryption e))
    , s ~ Encryption e
    , e ~ L4Proto
    , PubKey 'Encrypt s ~ Encrypt.PublicKey
    , Show (PubKey 'Sign s)
    , Show (Nonce ())
    )
  => EncryptionHandshakeAdapter e m s
  -> EncryptionHandshake e
  -> m ()

encryptionHandshakeProto EncryptionHandshakeAdapter{..} = \case

  ResetEncryptionKeys -> do
      peer <- thatPeer proto
      mpeerData <- find (KnownPeerKey peer) id
      -- TODO: check theirsign
      trace $ "ENCRYPTION EHSP ResetEncryptionKeys from" <+> viaShow (peer, mpeerData)

      -- сначала удалим у себя его прошлый ключ
      encHandshake_considerPeerAsymmKey peer Nothing

      creds <- getCredentials @s
      ourpubkey <- pure $ pubKeyFromKeypair @s $ encAsymmetricKeyPair
      sendBeginEncryptionExchange @e creds ourpubkey peer

  BeginEncryptionExchange theirsign theirpubkey -> do
      peer <- thatPeer proto
      mpeerData <- find (KnownPeerKey peer) id
      -- TODO: check theirsign

      trace $ "ENCRYPTION EHSP BeginEncryptionExchange from" <+> viaShow (peer, mpeerData)

      -- взять свои ключи
      creds <- getCredentials @s

      ourpubkey <- pure $ pubKeyFromKeypair @s $ encAsymmetricKeyPair

      -- подписать нонс
      let sign = makeSign @s (view peerSignSk creds) ((cs . serialise) ourpubkey)

      -- сначала удалим у себя его прошлый ключ
      encHandshake_considerPeerAsymmKey peer Nothing

      -- отправить обратно свой публичный ключ
      -- отправится пока ещё в плоском виде
      response (AckEncryptionExchange @e sign ourpubkey)

      -- Только после этого прописываем его ключ у себя
      encHandshake_considerPeerAsymmKey peer (Just theirpubkey)

  AckEncryptionExchange theirsign theirpubkey -> do
      peer <- thatPeer proto
      mpeerData <- find (KnownPeerKey peer) id
      -- TODO: check theirsign

      trace $ "ENCRYPTION EHSP AckEncryptionExchange from" <+> viaShow (peer, mpeerData)

      -- Он уже прописал у себя наш публичный ключ и готов общаться шифрованными сообщениями
      -- Прописываем его ключ у себя
      encHandshake_considerPeerAsymmKey peer (Just theirpubkey)

  where
    proto = Proxy @(EncryptionHandshake e)

-----

data PeerAsymmInfo e = PeerAsymmInfo

data instance EventKey e (PeerAsymmInfo e) = PeerAsymmInfoKey
  deriving stock (Generic)

deriving stock instance (Eq (Peer e)) => Eq (EventKey e (PeerAsymmInfo e))
instance (Hashable (Peer e)) => Hashable (EventKey e (PeerAsymmInfo e))

data instance Event e (PeerAsymmInfo e) =
  PeerAsymmPubKey (Peer e) (AsymmPubKey (Encryption e))
  deriving stock (Typeable)

instance Expires (EventKey e (PeerAsymmInfo e)) where
  expiresIn _ = Nothing

instance
  ( Serialise (PubKey 'Sign (Encryption e))
  , Serialise (PubKey 'Encrypt (Encryption e))
  , Serialise (Signature (Encryption e))
  )
  => Serialise (EncryptionHandshake e)

deriving instance
  ( Show (PubKey 'Encrypt (Encryption e))
  , Show (Signature (Encryption e))
  )
  => Show (EncryptionHandshake e)

type instance SessionData e (EncryptionHandshake e) = ()

newtype instance SessionKey e (EncryptionHandshake e) =
  KnownPeerAsymmInfoKey (Peer e)
  deriving stock (Generic, Typeable)

deriving instance Eq (Peer e) => Eq (SessionKey e (EncryptionHandshake e))
instance Hashable (Peer e) => Hashable (SessionKey e (EncryptionHandshake e))

data instance EventKey e (EncryptionHandshake e) =
  AnyKnownPeerEncryptionHandshakeEventKey
  deriving stock (Typeable, Eq,Generic)
