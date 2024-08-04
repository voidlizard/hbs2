module HBS2.Peer.RPC.Client.Internal
  ( module HBS2.Peer.RPC.Client.Internal
  , module Exported
  ) where

import HBS2.Peer.Prelude

import HBS2.Hash as Exported
import HBS2.Data.Types.Refs as Exported

import HBS2.Net.Proto.Service as Exported

import Data.Kind
import Control.Exception

data RpcClientError =
    RpcNotConnectedError
  | RpcTimeoutError
  deriving (Eq,Typeable,Show)

instance Exception RpcClientError

class Monad m => HasClientAPI (api :: [Type]) proto m where
  getClientAPI :: m (ServiceCaller api proto)

