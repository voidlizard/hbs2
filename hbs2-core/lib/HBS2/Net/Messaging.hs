{-# Language FunctionalDependencies #-}
module HBS2.Net.Messaging where

import HBS2.Net.Proto

import Control.Monad.IO.Class

newtype From a = From (Peer a)

newtype To a = To (Peer a)

class HasPeer proto => Messaging bus proto msg  | bus -> proto, bus -> msg where

  sendTo  :: MonadIO m => bus -> To proto -> From proto -> msg -> m ()
  receive :: MonadIO m => bus -> To proto -> m [(From proto, msg)]

