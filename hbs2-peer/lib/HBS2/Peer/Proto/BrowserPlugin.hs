module HBS2.Peer.Proto.BrowserPlugin
  ( module HBS2.Peer.Proto.BrowserPlugin
  , module HBS2.Net.Proto.Service
  , PIPE
  ) where

import HBS2.Net.Messaging.Pipe
import HBS2.Net.Proto.Service

import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise

data RpcChannelQuery

-- API definition
type BrowserPluginAPI = '[ RpcChannelQuery ]

-- API endpoint definition
type instance Input RpcChannelQuery = [(Text,Text)]
type instance Output RpcChannelQuery = Maybe ByteString


-- Codec for protocol
instance HasProtocol PIPE (ServiceProto BrowserPluginAPI PIPE) where
  type instance ProtocolId (ServiceProto BrowserPluginAPI PIPE) = 3103959867
  type instance Encoded PIPE = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

