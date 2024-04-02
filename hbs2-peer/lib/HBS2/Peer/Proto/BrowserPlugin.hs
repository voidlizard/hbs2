{-# Language TemplateHaskell #-}
module HBS2.Peer.Proto.BrowserPlugin
  ( module HBS2.Peer.Proto.BrowserPlugin
  , module HBS2.Net.Proto.Service
  , PIPE
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Messaging.Pipe
import HBS2.Net.Proto.Service

import Data.ByteString.Lazy (ByteString)
import Codec.Serialise
import Lens.Micro.Platform

data RpcChannelQuery

-- API definition
type BrowserPluginAPI = '[ RpcChannelQuery ]

data PluginMethod =
    Get { _getPath :: [Text]
        , _getArgs :: [(Text,Text)]
        }
    deriving stock Generic

makeLenses 'Get

instance Serialise PluginMethod

-- API endpoint definition
type instance Input RpcChannelQuery = PluginMethod
type instance Output RpcChannelQuery = Maybe ByteString


-- Codec for protocol
instance HasProtocol PIPE (ServiceProto BrowserPluginAPI PIPE) where
  type instance ProtocolId (ServiceProto BrowserPluginAPI PIPE) = 3103959867
  type instance Encoded PIPE = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

