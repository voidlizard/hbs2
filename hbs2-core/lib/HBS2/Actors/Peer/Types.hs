{-# Language AllowAmbiguousTypes #-}
module HBS2.Actors.Peer.Types where

import HBS2.Prelude
import HBS2.Storage
import HBS2.Net.Proto.Types
import HBS2.Net.Messaging
import HBS2.Hash

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)


class HasProtocol e p => HasTimeLimits e p m where
  tryLockForPeriod :: Peer e -> p -> m Bool

instance {-# OVERLAPPABLE #-}
  (MonadIO (t m), Monad m, MonadTrans t, HasProtocol e p, HasTimeLimits e p m) => HasTimeLimits e p (t m) where
  tryLockForPeriod p m = lift (tryLockForPeriod p m)
    -- pure True
    -- liftIO $ print "LIMIT DOES NOT WORK"
    -- pure True

-- instance HasConf m => HasConf (ResponseM e m)



class (Monad m, HasProtocol e p) => HasGossip e p m where
  gossip :: p -> m ()


class Monad m => HasOwnPeer e m where
  ownPeer :: m (Peer e)


data Fabriq e = forall bus . (Messaging bus e (Encoded e)) => Fabriq bus


class HasFabriq e m where
  getFabriq :: m (Fabriq e)

data AnyMessage enc e = AnyMessage !Integer !(Encoded e)
                       deriving stock (Generic)

type PeerMessaging e = ( Messaging (Fabriq e) e (AnyMessage (Encoded e) e)
                       , Eq (Encoded e)
                       , Hashable (Encoded e)
                       )

