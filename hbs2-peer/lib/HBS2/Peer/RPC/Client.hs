module HBS2.Peer.RPC.Client where

import HBS2.Net.Proto.Service

import Data.Kind

class Monad m => HasClientAPI (api :: [Type]) proto m where
  getClientAPI :: m (ServiceCaller api proto)




