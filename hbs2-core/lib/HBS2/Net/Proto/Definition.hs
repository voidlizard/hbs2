{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Net.Proto.Definition
  ( module HBS2.Net.Proto.BlockAnnounce
  , module HBS2.Net.Proto.BlockChunks
  , module HBS2.Net.Proto.BlockInfo
  )
  where

import HBS2.Clock
import HBS2.Defaults
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.UDP
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockAnnounce
import HBS2.Net.Proto.BlockChunks
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerAnnounce
import HBS2.Net.Proto.PeerExchange
import HBS2.Net.Proto.RefLinear
import HBS2.Prelude

import Data.Functor
import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS
import Codec.Serialise (deserialiseOrFail,serialise,Serialise(..))

import Crypto.Saltine.Core.Box qualified as Crypto
import Crypto.Saltine.Class qualified as Crypto
import Crypto.Saltine.Core.Sign qualified as Sign
import Crypto.Saltine.Core.Box qualified as Encrypt


type instance PubKey  'Sign e = Sign.PublicKey
type instance PrivKey 'Sign e = Sign.SecretKey
type instance PubKey  'Encrypt e = Encrypt.PublicKey
type instance PrivKey 'Encrypt e = Encrypt.SecretKey

-- FIXME: proper-serialise-for-keys
--   Возможно, нужно написать ручные инстансы Serialise
--   использовать encode/decode для каждого инстанса ниже $(c:end + 4)
--   и это будет более правильная сериализация.
--   но возможно, будет работать и так, ведь ключи
--   это же всего лишь байтстроки внутри.

instance Serialise Sign.PublicKey
instance Serialise Encrypt.PublicKey
instance Serialise Sign.SecretKey
instance Serialise Encrypt.SecretKey

instance HasProtocol UDP (BlockInfo UDP) where
  type instance ProtocolId (BlockInfo UDP) = 1
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

  -- FIXME: requestMinPeriod-breaks-fast-block-download
  --
  requestPeriodLim = ReqLimPerMessage 1

instance HasProtocol UDP (BlockChunks UDP) where
  type instance ProtocolId (BlockChunks UDP) = 2
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance Expires (SessionKey UDP (BlockChunks UDP)) where
  expiresIn _ = Just defCookieTimeoutSec

instance HasProtocol UDP (BlockAnnounce UDP) where
  type instance ProtocolId (BlockAnnounce UDP) = 3
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance HasProtocol UDP (PeerHandshake UDP) where
  type instance ProtocolId (PeerHandshake UDP) = 4
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

  requestPeriodLim = ReqLimPerProto 2

instance HasProtocol UDP (PeerAnnounce UDP) where
  type instance ProtocolId (PeerAnnounce UDP) = 5
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance HasProtocol UDP (PeerExchange UDP) where
  type instance ProtocolId (PeerExchange UDP) = 6
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance HasProtocol UDP (AnnLRef UDP) where
  type instance ProtocolId (AnnLRef UDP) = 7
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance Expires (SessionKey UDP (BlockInfo UDP)) where
  expiresIn _ = Just defCookieTimeoutSec

instance Expires (EventKey UDP (BlockInfo UDP)) where
  expiresIn _  = Just 600

instance Expires (EventKey UDP (BlockChunks UDP)) where
  expiresIn _ = Just 600

instance Expires (EventKey UDP (BlockAnnounce UDP)) where
  expiresIn _ = Nothing

instance Expires (SessionKey UDP (KnownPeer UDP)) where
  expiresIn _ = Just 3600

instance Expires (SessionKey UDP (PeerHandshake UDP)) where
  expiresIn _ = Just 10

instance Expires (EventKey UDP (PeerAnnounce UDP)) where
  expiresIn _ = Nothing

instance Expires (EventKey UDP (AnnLRef UDP)) where
  expiresIn _ = Nothing


instance MonadIO m => HasNonces (PeerHandshake UDP) m where
  type instance Nonce (PeerHandshake UDP) = BS.ByteString
  newNonce = do
    n <- liftIO ( Crypto.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance MonadIO m => HasNonces (PeerExchange UDP) m where
  type instance Nonce (PeerExchange UDP) = BS.ByteString
  newNonce = do
    n <- liftIO ( Crypto.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance MonadIO m => HasNonces () m where
  type instance Nonce () = BS.ByteString
  newNonce = do
    n <- liftIO ( Crypto.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance Serialise Sign.Signature

instance Signatures UDP where
  type Signature UDP = Sign.Signature
  makeSign = Sign.signDetached
  verifySign = Sign.signVerifyDetached

instance Signatures MerkleEncryptionType where
  type Signature MerkleEncryptionType = Sign.Signature
  makeSign = Sign.signDetached
  verifySign = Sign.signVerifyDetached



