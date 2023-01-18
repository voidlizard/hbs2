{-# Language TypeFamilyDependencies #-}
{-# Language FunctionalDependencies #-}
module HBS2.Net.Proto.Types
  ( module HBS2.Net.Proto.Types
  ) where

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.Hashable
import Control.Monad.IO.Class

-- e -> Transport (like, UDP or TChan)
-- p -> L4 Protocol (like Ping/Pong)

class (Hashable (Peer e), Eq (Peer e)) => HasPeer e where
  data family (Peer e) :: Type


class (MonadIO m, HasProtocol e p) => Response e p m | p -> e where
  response :: p -> m ()
  deferred :: Proxy p -> m () -> m ()

class Request e p (m :: Type -> Type) | p -> e where
  request :: Peer e -> p -> m ()


class (KnownNat (ProtocolId p), HasPeer e) => HasProtocol e p | p -> e  where
  type family ProtocolId p = (id :: Nat) | id -> p
  type family Encoded e :: Type

  protoId :: forall . KnownNat (ProtocolId p) => Proxy p -> Integer
  protoId _ = natVal (Proxy @(ProtocolId p))

  decode :: Encoded e -> Maybe p
  encode :: p -> Encoded e

