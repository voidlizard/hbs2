{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}

module HBS2.Net.Proto.EncryptionHandshake where

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Clock
import HBS2.Data.Types
import HBS2.Events
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple

import Codec.Serialise()
import Control.Monad
import Crypto.Saltine.Class qualified as Crypto
import Crypto.Saltine.Core.Box qualified as Encrypt
import Data.ByteString qualified as BS
import Data.Hashable
import Data.Maybe
import Data.String.Conversions (cs)
import Lens.Micro.Platform
import Type.Reflection (someTypeRep)

newtype EENonce = EENonce { unEENonce :: BS.ByteString }
  deriving stock (Generic)
  deriving newtype (Eq, Serialise, Hashable)
  deriving (Pretty, Show) via AsBase58 BS.ByteString

instance
    ( Show (PubKey 'Encrypt (Encryption e))
    , Show (PubKey 'Sign (Encryption e))
    , Show (Nonce ())
    )
    => Pretty (PeerData e) where
  pretty = viaShow

data EncryptionHandshake e =
    BeginEncryptionExchange EENonce (Signature (Encryption e)) (PubKey 'Encrypt (Encryption e))
  | AckEncryptionExchange EENonce (Signature (Encryption e)) (PubKey 'Encrypt (Encryption e))
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
    , HasNonces (EncryptionHandshake e) m
    -- , HasCredentials s m
    , Asymm s
    , Signatures s
    , Serialise (PubKey 'Encrypt s)
    , Nonce (EncryptionHandshake e) ~ EENonce
    , Pretty (Peer e)
    , HasProtocol e (EncryptionHandshake e)
    , e ~ L4Proto
    , s ~ Encryption e
    )
  => PeerEnv e
  -> PeerCredentials s
  -> Peer e
  -> PubKey 'Encrypt (Encryption e)
  -> m ()

sendBeginEncryptionExchange penv creds peer pubkey = do
    nonce0 <- newNonce @(EncryptionHandshake e)
    let ourpubkey = pubKeyFromKeypair @s $ view envAsymmetricKeyPair penv
    let sign = makeSign @s (view peerSignSk creds) (unEENonce nonce0 <> (cs . serialise) ourpubkey)
    request peer (BeginEncryptionExchange @e nonce0 sign pubkey)

data EncryptionHandshakeAdapter e m s = EncryptionHandshakeAdapter
  { encHandshake_considerPeerAsymmKey :: Peer e -> Maybe (PeerData e) -> Maybe Encrypt.PublicKey -> m ()
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
    , PubKey Encrypt s ~ Encrypt.PublicKey
    , Show (PubKey 'Sign s)
    , Show (Nonce ())
    )
  => EncryptionHandshakeAdapter e m s
  -> PeerEnv e
  -> EncryptionHandshake e
  -> m ()

encryptionHandshakeProto EncryptionHandshakeAdapter{..} penv = \case

  ResetEncryptionKeys -> do
      peer <- thatPeer proto
      mpeerData <- find (KnownPeerKey peer) id
      -- TODO: check theirsign
      trace $ "EHSP ResetEncryptionKeys from" <+> viaShow (peer, mpeerData)

      -- сначала удалим у себя его прошлый ключ
      encHandshake_considerPeerAsymmKey peer mpeerData Nothing

      creds <- getCredentials @s
      let ourpubkey = pubKeyFromKeypair @s $ view envAsymmetricKeyPair penv
      sendBeginEncryptionExchange @e penv creds peer ourpubkey

  BeginEncryptionExchange nonce0 theirsign theirpubkey -> do
      peer <- thatPeer proto
      mpeerData <- find (KnownPeerKey peer) id
      -- TODO: check theirsign

      trace $ "EHSP BeginEncryptionExchange from" <+> viaShow (peer, mpeerData)

      -- взять свои ключи
      creds <- getCredentials @s

      let ourpubkey = pubKeyFromKeypair @s $ view envAsymmetricKeyPair penv

      -- подписать нонс
      let sign = makeSign @s (view peerSignSk creds) (unEENonce nonce0 <> (cs . serialise) ourpubkey)

      -- сначала удалим у себя его прошлый ключ
      encHandshake_considerPeerAsymmKey peer mpeerData Nothing

      -- отправить обратно свой публичный ключ
      -- отправится пока ещё в плоском виде
      response (AckEncryptionExchange @e nonce0 sign ourpubkey)

      -- Только после этого прописываем его ключ у себя
      encHandshake_considerPeerAsymmKey peer mpeerData (Just theirpubkey)

  AckEncryptionExchange nonce0 theirsign theirpubkey -> do
      peer <- thatPeer proto
      mpeerData <- find (KnownPeerKey peer) id
      -- TODO: check theirsign

      trace $ "EHSP AckEncryptionExchange from" <+> viaShow (peer, mpeerData)

      -- Он уже прописал у себя наш публичный ключ и готов общаться шифрованными сообщениями
      -- Прописываем его ключ у себя
      encHandshake_considerPeerAsymmKey peer mpeerData (Just theirpubkey)

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

instance MonadIO m => HasNonces (EncryptionHandshake L4Proto) m where
  type instance Nonce (EncryptionHandshake L4Proto) = EENonce
  newNonce = EENonce . BS.take 32 . Crypto.encode <$> liftIO Encrypt.newNonce

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
  KnownPeerAsymmInfoKey (EENonce, Peer e)
  deriving stock (Generic, Typeable)

deriving instance Eq (Peer e) => Eq (SessionKey e (EncryptionHandshake e))
instance Hashable (Peer e) => Hashable (SessionKey e (EncryptionHandshake e))

data instance EventKey e (EncryptionHandshake e) =
  AnyKnownPeerEncryptionHandshakeEventKey
  deriving stock (Typeable, Eq,Generic)

instance Hashable (EventKey e (EncryptionHandshake e))
