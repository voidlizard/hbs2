{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language PatternSynonyms #-}
module HBS2.Peer.Proto.BrowserPlugin
  ( module HBS2.Net.Proto.Service
  , PIPE
  , getPath
  , getArgs
  , RpcChannelQuery
  , BrowserPluginAPI
  , PluginMethod
  , CreatePluginMethod(..)
  , filterKW
  , pattern Method
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Messaging.Pipe
import HBS2.Net.Proto.Service

import Data.Kind
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.ByteString.Lazy (ByteString)
import Data.Text qualified as Text
import Codec.Serialise
import Lens.Micro.Platform

data RpcChannelQuery

-- API definition
type BrowserPluginAPI = '[ RpcChannelQuery ]

pattern Method :: [Text] -> HashMap Text Text -> PluginMethod
pattern Method p a = Get p a
{-# COMPLETE Method #-}

data PluginMethod =
    Get { _getPath :: [Text]
        , _getArgs :: HashMap Text Text
        }
    deriving stock (Show,Generic)

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


class CreatePluginMethod a where
 type family Dict a :: Type
 createPluginMethod :: [a] -> Dict a -> PluginMethod


filterKW :: [Text] -> PluginMethod -> PluginMethod
filterKW kw = over getArgs (HM.filterWithKey filt)
  where
    filt k _ = k `elem` kw

instance CreatePluginMethod String where
  type instance Dict String = [(String,String)]
  createPluginMethod path dict =
    Get (fmap Text.pack path)
        (HM.fromList (fmap (over _1 Text.pack . over _2 Text.pack) dict))

instance CreatePluginMethod Text where
  type instance Dict Text = [(Text,Text)]
  createPluginMethod path dict =
    Get path (HM.fromList dict)

