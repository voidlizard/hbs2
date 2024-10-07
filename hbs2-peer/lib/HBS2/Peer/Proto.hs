{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.Proto
  ( module HBS2.Peer.Proto.PeerMeta
  , module HBS2.Peer.Proto.BlockAnnounce
  , module HBS2.Peer.Proto.BlockChunks
  , module HBS2.Peer.Proto.BlockInfo
  , module HBS2.Peer.Proto.Peer
  , module HBS2.Peer.Proto.PeerAnnounce
  , module HBS2.Peer.Proto.PeerExchange
  , module HBS2.Peer.Proto.RefLog
  , module HBS2.Peer.Proto.RefChan
  , module HBS2.Peer.Proto.AnyRef
  , module HBS2.Net.Proto.Types
  , module HBS2.Net.Proto.Sessions
  , module HBS2.Net.Proto.Service
  ) where

import HBS2.Peer.Prelude
import HBS2.Net.Proto.Types
import HBS2.Peer.Proto.PeerMeta
import HBS2.Peer.Proto.BlockAnnounce
import HBS2.Peer.Proto.BlockChunks
import HBS2.Peer.Proto.BlockInfo
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.PeerAnnounce
import HBS2.Peer.Proto.PeerExchange
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.RefChan hiding (Notify)
import HBS2.Peer.Proto.AnyRef
import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.Proto.Mailbox

import HBS2.Actors.Peer.Types
import HBS2.Net.Messaging.Unix (UNIX)
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Service

import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS
import Codec.Serialise
import Crypto.Saltine.Core.Box qualified as Crypto
import Crypto.Saltine.Class qualified as Crypto

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

  -- TODO: find-out-optimal-max-safe-frequency
  requestPeriodLim = ReqLimPerMessage 600

instance HasProtocol L4Proto (RefLogRequest L4Proto) where
  type instance ProtocolId (RefLogRequest L4Proto) = 8
  type instance Encoded L4Proto = ByteString
  decode = deserialiseCustom
  encode = serialise


instance HasProtocol L4Proto (RefChanHead L4Proto) where
  type instance ProtocolId (RefChanHead L4Proto) = 11001
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

  -- TODO: find-out-optimal-max-frequency
  requestPeriodLim = ReqLimPerMessage 60


instance HasProtocol L4Proto (RefChanUpdate L4Proto) where
  type instance ProtocolId (RefChanUpdate L4Proto) = 11002
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

  -- мы не можем рассылать одинаковые сообщения никогда,
  -- ну или хотя бы не чаще, чем раз в 10 минут.
  -- но poll у нас в минутах, и с минимальным периодом 1 минута
  requestPeriodLim = ReqLimPerMessage 60

instance HasProtocol L4Proto (RefChanRequest L4Proto) where
  type instance ProtocolId (RefChanRequest L4Proto) = 11003
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

  -- мы не можем рассылать одинаковые сообщения никогда,
  -- ну или хотя бы не чаще, чем раз в 10 минут.
  -- но poll у нас в минутах, и с минимальным периодом 1 минута
  requestPeriodLim = ReqLimPerMessage 1


instance HasProtocol L4Proto (RefChanNotify L4Proto) where
  type instance ProtocolId (RefChanNotify L4Proto) = 11004
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

  -- не чаще раза в секуду, хотя бы.
  -- или сколько? минуту? минуты мало.
  -- но сообщения должны быть разные,
  -- тогда и минута нормально.
  -- возьмем пока 10 секунд
  requestPeriodLim = NoLimit

instance HasProtocol L4Proto (LWWRefProto L4Proto) where
  type instance ProtocolId (LWWRefProto L4Proto) = 12001
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise
  requestPeriodLim = ReqLimPerMessage 1


instance HasProtocol L4Proto (MailBoxProto HBS2Basic L4Proto) where
  type instance ProtocolId (MailBoxProto HBS2Basic L4Proto) = 13001
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

  -- TODO: limit-request-period
  requestPeriodLim = NoLimit -- ReqLimPerMessage 1

instance Serialise (RefChanValidate UNIX) => HasProtocol UNIX (RefChanValidate UNIX) where
  type instance ProtocolId (RefChanValidate UNIX) = 0xFFFA0001
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


instance Serialise (RefChanNotify UNIX) => HasProtocol UNIX (RefChanNotify UNIX) where
  type instance ProtocolId (RefChanNotify UNIX) = 0xFFFB0001
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise
  requestPeriodLim = NoLimit

instance MonadIO m => HasNonces (RefChanValidate UNIX) m where
  type instance Nonce (RefChanValidate UNIX) = BS.ByteString
  newNonce = do
    n <- liftIO ( Crypto.newNonce <&> Crypto.encode )
    pure $ BS.take 8 n


instance HasTimeLimits UNIX (RefChanValidate UNIX) IO where
  tryLockForPeriod _ _ = pure True

instance HasTimeLimits UNIX (RefChanNotify UNIX) IO where
  tryLockForPeriod _ _ = pure True

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

instance Expires (EventKey L4Proto (PeerAnnounce L4Proto)) where
  expiresIn _ = Nothing

instance MonadIO m => HasNonces (PeerHandshake L4Proto) m where
  type instance Nonce (PeerHandshake L4Proto) = BS.ByteString
  newNonce = do
    n <- liftIO ( Crypto.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance MonadIO m => HasNonces (PeerExchange L4Proto) m where
  type instance Nonce (PeerExchange L4Proto) = BS.ByteString
  newNonce = do
    n <- liftIO ( Crypto.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance MonadIO m => HasNonces (RefLogUpdate L4Proto) m where
  type instance Nonce (RefLogUpdate L4Proto) = BS.ByteString
  newNonce = do
    n <- liftIO ( Crypto.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

instance MonadIO m => HasNonces () m where
  type instance Nonce () = BS.ByteString
  newNonce = do
    n <- liftIO ( Crypto.newNonce <&> Crypto.encode )
    pure $ BS.take 32 n

