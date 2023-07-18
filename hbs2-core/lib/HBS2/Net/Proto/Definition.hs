{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Net.Proto.Definition
  ( module HBS2.Net.Proto.BlockAnnounce
  , module HBS2.Net.Proto.BlockChunks
  , module HBS2.Net.Proto.BlockInfo
  )
  where

import HBS2.Clock
import HBS2.Data.Types.Crypto
import HBS2.Defaults
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockAnnounce
import HBS2.Net.Proto.BlockChunks
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.EncryptionHandshake
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerAnnounce
import HBS2.Net.Proto.PeerExchange
import HBS2.Net.Proto.PeerMeta
import HBS2.Net.Proto.RefLog
import HBS2.Prelude

import Control.Monad
import Data.Functor
import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS
import Codec.Serialise (deserialiseOrFail,serialise)

import Crypto.Saltine.Class qualified as Crypto
import Crypto.Saltine.Core.Sign qualified as Sign
import Crypto.Saltine.Core.Box qualified as Encrypt

import HBS2.Data.Types.Crypto


type instance Encryption L4Proto = HBS2Basic

-- FIXME: proper-serialise-for-keys
--   Возможно, нужно написать ручные инстансы Serialise
--   использовать encode/decode для каждого инстанса ниже $(c:end + 4)
--   и это будет более правильная сериализация.
--   но возможно, будет работать и так, ведь ключи
--   это же всего лишь байтстроки внутри.

deserialiseCustom :: (Serialise a, MonadPlus m) => ByteString -> m a
deserialiseCustom = either (const mzero) pure . deserialiseOrFail
-- deserialiseCustom = either (\msg -> trace ("deserialiseCustom: " <> show msg) mzero) pure . deserialiseOrFail
-- deserialiseCustom = either (error . show) pure . deserialiseOrFail

instance HasProtocol L4Proto (BlockInfo L4Proto) where
  type instance ProtocolId (BlockInfo L4Proto) = 1
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

  -- FIXME: requestMinPeriod-breaks-fast-block-download
  --
  requestPeriodLim = ReqLimPerMessage 1

instance HasProtocol L4Proto (BlockChunks L4Proto) where
  type instance ProtocolId (BlockChunks L4Proto) = 2
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

instance Expires (SessionKey L4Proto (BlockChunks L4Proto)) where
  expiresIn _ = Just defCookieTimeoutSec

instance HasProtocol L4Proto (BlockAnnounce L4Proto) where
  type instance ProtocolId (BlockAnnounce L4Proto) = 3
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

instance HasProtocol L4Proto (PeerHandshake L4Proto) where
  type instance ProtocolId (PeerHandshake L4Proto) = 4
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

  requestPeriodLim = ReqLimPerProto 0.5

instance HasProtocol L4Proto (PeerAnnounce L4Proto) where
  type instance ProtocolId (PeerAnnounce L4Proto) = 5
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

instance HasProtocol L4Proto (PeerExchange L4Proto) where
  type instance ProtocolId (PeerExchange L4Proto) = 6
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

instance HasProtocol L4Proto (RefLogUpdate L4Proto) where
  type instance ProtocolId (RefLogUpdate L4Proto) = 7
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

  requestPeriodLim = ReqLimPerMessage 600

instance HasProtocol L4Proto (RefLogRequest L4Proto) where
  type instance ProtocolId (RefLogRequest L4Proto) = 8
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

instance HasProtocol L4Proto (PeerMetaProto L4Proto) where
  type instance ProtocolId (PeerMetaProto L4Proto) = 9
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

  -- FIXME: real-period
  requestPeriodLim = ReqLimPerMessage 0.25

instance HasProtocol L4Proto (EncryptionHandshake L4Proto) where
  type instance ProtocolId (EncryptionHandshake L4Proto) = 10
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise

  requestPeriodLim = ReqLimPerProto 0.5

instance Expires (SessionKey L4Proto (BlockInfo L4Proto)) where
  expiresIn _ = Just defCookieTimeoutSec

instance Expires (EventKey L4Proto (BlockInfo L4Proto)) where
  expiresIn _  = Just 600

instance Expires (EventKey L4Proto (BlockChunks L4Proto)) where
  expiresIn _ = Just 600

instance Expires (EventKey L4Proto (BlockAnnounce L4Proto)) where
  expiresIn _ = Nothing

instance Expires (SessionKey L4Proto (KnownPeer L4Proto)) where
  expiresIn _ = Just 3600

instance Expires (SessionKey L4Proto (PeerHandshake L4Proto)) where
  expiresIn _ = Just 60

instance Expires (SessionKey L4Proto (EncryptionHandshake L4Proto)) where
  expiresIn _ = Just 60

instance Expires (EventKey L4Proto (PeerAnnounce L4Proto)) where
  expiresIn _ = Nothing

instance Expires (EventKey L4Proto (PeerMetaProto L4Proto)) where
  expiresIn _ = Just 600

-- instance Expires (EventKey L4Proto (EncryptionHandshake L4Proto)) where
--   expiresIn _ = Just 600

-- instance MonadIO m => HasNonces () m where
--   type instance Nonce (PeerHandshake L4Proto) = BS.ByteString
--   newNonce = do
--     n <- liftIO ( Encrypt.newNonce <&> Crypto.encode )
--     pure $ BS.take 32 n

instance MonadIO m => HasNonces (PeerHandshake L4Proto) m where
  type instance Nonce (PeerHandshake L4Proto) = BS.ByteString
  newNonce = do
    n <- liftIO ( Encrypt.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance MonadIO m => HasNonces (PeerExchange L4Proto) m where
  type instance Nonce (PeerExchange L4Proto) = BS.ByteString
  newNonce = do
    n <- liftIO ( Encrypt.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance MonadIO m => HasNonces (RefLogUpdate L4Proto) m where
  type instance Nonce (RefLogUpdate L4Proto) = BS.ByteString
  newNonce = do
    n <- liftIO ( Encrypt.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance MonadIO m => HasNonces () m where
  type instance Nonce () = BS.ByteString
  newNonce = do
    n <- liftIO ( Encrypt.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance Asymm HBS2Basic where
  type AsymmKeypair HBS2Basic = Encrypt.Keypair
  type AsymmPrivKey HBS2Basic = Encrypt.SecretKey
  type AsymmPubKey HBS2Basic = Encrypt.PublicKey
  type CommonSecret HBS2Basic = Encrypt.CombinedKey
  asymmNewKeypair = liftIO Encrypt.newKeypair
  privKeyFromKeypair = Encrypt.secretKey
  pubKeyFromKeypair = Encrypt.publicKey
  genCommonSecret = Encrypt.beforeNM

instance Hashed HbSync Sign.PublicKey where
  hashObject pk = hashObject (Crypto.encode pk)

