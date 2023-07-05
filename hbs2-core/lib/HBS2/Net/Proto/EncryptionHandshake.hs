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

data EncryptionHandshake e =
    BeginEncryptionExchange EENonce (PubKey 'Encrypt (Encryption e))
  | AckEncryptionExchange EENonce (Signature (Encryption e)) (PubKey 'Encrypt (Encryption e))
  deriving stock (Generic)

sendEncryptionPubKey :: forall e m . ( MonadIO m
                         , Request e (EncryptionHandshake e) m
                         , Sessions e (EncryptionHandshake e) m
                         , HasNonces (EncryptionHandshake e) m
                         , Nonce (EncryptionHandshake e) ~ EENonce
                         , Pretty (Peer e)
                         , HasProtocol e (EncryptionHandshake e)
                         , e ~ L4Proto
                         )
         => Peer e -> PubKey 'Encrypt (Encryption e) -> m ()

sendEncryptionPubKey pip pubkey = do
  nonce <- newNonce @(EncryptionHandshake e)
  tt <- liftIO $ getTimeCoarse
  request pip (BeginEncryptionExchange @e nonce pubkey)

data EncryptionHandshakeAdapter e m s = EncryptionHandshakeAdapter
  { encHandshake_considerPeerAsymmKey :: PeerAddr e -> Encrypt.PublicKey -> m ()
  }


encryptionHandshakeProto :: forall e s m . ( MonadIO m
                                     , Response e (EncryptionHandshake e) m
                                     , Request e (EncryptionHandshake e) m
                                     , Sessions e (EncryptionHandshake e) m
                                     , HasNonces (EncryptionHandshake e) m
                                     , HasPeerNonce e m
                                     , Nonce (EncryptionHandshake e) ~ EENonce
                                     , Pretty (Peer e)
                                     , EventEmitter e (EncryptionHandshake e) m
                                     , EventEmitter e (PeerAsymmInfo e) m
                                     , HasCredentials s m
                                     , Asymm s
                                     , Signatures s
                                     , Serialise (PubKey 'Encrypt (Encryption e))
                                     , s ~ Encryption e
                                     , e ~ L4Proto
                                     , PubKey Encrypt s ~ Encrypt.PublicKey
                                     )
                   => EncryptionHandshakeAdapter e m s
                   -> PeerEnv e
                   -> EncryptionHandshake e
                   -> m ()

encryptionHandshakeProto EncryptionHandshakeAdapter{..} penv = \case

    BeginEncryptionExchange nonce theirpubkey -> do
      pip <- thatPeer proto
      trace $ "GOT BeginEncryptionExchange from" <+> pretty pip

      paddr <- toPeerAddr pip
      encHandshake_considerPeerAsymmKey paddr theirpubkey

      -- взять свои ключи
      creds <- getCredentials @s

      let ourpubkey = pubKeyFromKeypair @s $ view envAsymmetricKeyPair penv

      -- подписать нонс
      let sign = makeSign @s (view peerSignSk creds) (unEENonce nonce <> (cs . serialise) ourpubkey)

      -- отправить обратно вместе с публичным ключом
      response (AckEncryptionExchange @e nonce sign ourpubkey)

      -- Нужно ли запомнить его theirpubkey или достаточно того, что будет
      -- получено в обратном AckEncryptionExchange?
      -- Нужно!
      emit PeerAsymmInfoKey (PeerAsymmPubKey pip theirpubkey)

      -- se <- find (KnownPeerKey pip) id <&> isJust
      -- unless se $ do
      --   sendEncryptionPubKey pip ourpubkey

    AckEncryptionExchange nonce0 sign theirpubkey -> do
      pip <- thatPeer proto
      -- trace $ "AckEncryptionExchange" <+> pretty pip

      paddr <- toPeerAddr pip
      encHandshake_considerPeerAsymmKey paddr theirpubkey

      emit PeerAsymmInfoKey (PeerAsymmPubKey pip theirpubkey)

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
