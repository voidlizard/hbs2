{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Net.Messaging
  ( module HBS2.Net.Messaging
  , module HBS2.Net.Proto
  ) where

import HBS2.Net.Proto

import Control.Monad.IO.Class

newtype From a = From (Peer a)
deriving instance Show (Peer a) => Show (From a)

newtype To a = To (Peer a)

-- class Messaging bus e msg  => MessagingHasPeer e where

-- class HasPeer proto => Messaging bus proto msg  | bus -> proto, bus -> msg where
class HasPeer proto => Messaging bus proto msg  where

  sendTo  :: MonadIO m => bus -> To proto -> From proto -> msg -> m ()
  receive :: MonadIO m => bus -> To proto -> m [(From proto, msg)]


