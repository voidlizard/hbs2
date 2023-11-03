{-# Language UndecidableInstances #-}
module HBS2.Peer.Notify
  ( RefChanEvents(..)
  , RefLogEvents(..)
  , newNotifyEnvServer
  , runNotifyWorkerServer
  , runNotifyWorkerClient
  , makeNotifyServer
  , makeNotifyClient
  , newSomeNotifySource
  , newNotifySink
  , emitNotify
  , NotifyKey(..)
  , NotifyData(..)
  , HasProtocol(..)
  ) where

import HBS2.Prelude
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Actors.Peer.Types
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Notify
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.RefLog
import HBS2.Net.Messaging.Unix (UNIX)
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.Definition()

import Codec.Serialise
import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS


data RefChanEvents e =
  RefChanOnNotify

instance HasProtocol UNIX  (NotifyProto (RefChanEvents L4Proto) UNIX) where
  type instance ProtocolId (NotifyProto (RefChanEvents L4Proto) UNIX) = 0x20e14bfa0ca1db8e
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise
  requestPeriodLim = NoLimit

-- FIXME: move-this-definitions-somewhere
newtype instance NotifyKey (RefChanEvents e) =
  RefChanNotifyKey (RefChanId e)
  deriving (Generic)

deriving newtype instance ForRefChans e => Hashable (NotifyKey (RefChanEvents e))
deriving newtype instance ForRefChans e => Eq (NotifyKey (RefChanEvents e))

data instance NotifyData (RefChanEvents e) =
  RefChanNotifyData (RefChanId e) (SignedBox BS.ByteString e)
  deriving Generic

instance ForRefChans e => Serialise (NotifyKey (RefChanEvents e))
instance ForRefChans e => Serialise (NotifyData (RefChanEvents e))

data RefLogEvents s =
  RefLogUpdated

newtype instance NotifyKey (RefLogEvents e) =
  RefLogNotifyKey (RefLogKey (Encryption e))
  deriving (Generic)

data instance NotifyData (RefLogEvents e) =
  RefLogUpdateNotifyData (RefLogKey (Encryption e)) HashRef
  deriving (Generic)

type ForRefLogEvents e = ( Serialise (PubKey 'Sign (Encryption e))
                         , Serialise (RefLogKey (Encryption e))
                         , FromStringMaybe (PubKey 'Sign (Encryption e))
                         , Hashable (PubKey 'Sign (Encryption e))
                         , Pretty  (AsBase58 (PubKey 'Sign (Encryption e)))
                         )

deriving newtype instance ForRefLogEvents e => Hashable (NotifyKey (RefLogEvents e))
deriving newtype instance ForRefLogEvents e => Eq (NotifyKey (RefLogEvents e))

instance ForRefLogEvents e => Serialise (NotifyKey (RefLogEvents e))

instance ForRefLogEvents e => Serialise (NotifyData (RefLogEvents e))

instance ForRefLogEvents L4Proto => HasProtocol UNIX  (NotifyProto (RefLogEvents L4Proto) UNIX) where
  type instance ProtocolId (NotifyProto (RefLogEvents L4Proto) UNIX) = 0x65A9ECE2A182216
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise
  requestPeriodLim = NoLimit




