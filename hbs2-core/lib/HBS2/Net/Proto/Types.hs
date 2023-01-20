{-# Language TypeFamilyDependencies #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes #-}
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

class Monad m => GenCookie e m where
  genCookie :: Hashable salt => salt -> m (Cookie e)

class HasCookie e p | p -> e where
  type family Cookie e :: Type
  getCookie :: p -> Maybe (Cookie e)
  getCookie = const Nothing

data WithCookie e p = WithCookie (Cookie e) p

class (Hashable (Peer e), Eq (Peer e)) => HasPeer e where
  data family (Peer e) :: Type


class (MonadIO m, HasProtocol e p) => Response e p m | p -> e where
  response :: p -> m ()
  deferred :: Proxy p -> m () -> m ()
  thatPeer :: Proxy p -> m (Peer e)

class Request e p (m :: Type -> Type) | p -> e where
  request :: Peer e -> p -> m ()


data family SessionKey  p :: Type
data family SessionData p :: Type

class ( Monad m
      , HasProtocol e p
      , Eq (SessionKey p)
      ) => Sessions e p m | p -> e where

  fetch  :: SessionData p          -- ^ default value in case it's not found
         -> SessionKey p           -- ^ session key
         -> (SessionData p -> a )  -- ^ modification function, i.e. lens
         -> m a

  update :: SessionKey p                      -- ^ session key
         -> (SessionData p -> SessionData p)  -- ^ modification function, i.e. lens
         -> m ()


class (KnownNat (ProtocolId p), HasPeer e) => HasProtocol e p | p -> e  where
  type family ProtocolId p = (id :: Nat) | id -> p
  type family Encoded e :: Type

  protoId :: forall . KnownNat (ProtocolId p) => Proxy p -> Integer
  protoId _ = natVal (Proxy @(ProtocolId p))

  decode :: Encoded e -> Maybe p
  encode :: p -> Encoded e

