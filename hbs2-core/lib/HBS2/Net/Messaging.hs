{-# Language FunctionalDependencies #-}
module HBS2.Net.Messaging where

import HBS2.Net.Proto

import Control.Monad.IO.Class

newtype From a = From (Peer a)

newtype To a = To (Peer a)

class IsPeer addr => Messaging bus addr msg  | bus -> addr, bus -> msg where

  sendTo  :: MonadIO m => bus -> To addr -> From addr -> msg -> m ()
  receive :: MonadIO m => bus -> To addr -> m [msg]




