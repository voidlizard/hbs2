{-# Language TypeFamilyDependencies #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.Types
  ( module HBS2.Net.Proto.Types
  ) where

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.Hashable
import Control.Monad.IO.Class
import Data.Typeable
import System.Random qualified as Random
import Data.Digest.Murmur32

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


-- we probably can not separate sessions
-- by sub-protocol types without
-- really crazy types.
--
-- And if we really need this, it may be done
-- by injecting a protocol type into 'e' or
-- introducing a common ADT for all session types
-- for common 'e' i.e. 'engine' or 'transport'
--
-- So it is that it is.

data family SessionKey  e p :: Type
type family SessionData e p :: Type


class ( Monad m
      , HasProtocol e p
      , Eq (SessionKey e p)
      , Hashable (SessionKey e p)
      , Typeable (SessionData e p)
      -- , Typeable e
      -- , Typeable p
      ) => Sessions e p m  | p -> e where


  -- | Session fetch function.
  -- | It will insert a new session, if default value is Just something.

  fetch  :: Bool                     -- ^ do add new session if not exists
         -> SessionData e p          -- ^ default value in case it's not found
         -> SessionKey e p           -- ^ session key
         -> (SessionData e p -> a )  -- ^ modification function, i.e. lens
         -> m a

  -- | Session update function
  -- | If will create a new session if it does not exist.
  -- | A modified value (or default) value will we saved.

  update :: SessionData e p                      -- ^ default value in case it's not found
         -> SessionKey e p                       -- ^ session key
         -> (SessionData e p -> SessionData e p) -- ^ modification function, i.e. lens
         -> m ()

  expire :: SessionKey e p -> m ()

class (KnownNat (ProtocolId p), HasPeer e) => HasProtocol e p | p -> e  where
  type family ProtocolId p = (id :: Nat) | id -> p
  type family Encoded e :: Type

  protoId :: forall . KnownNat (ProtocolId p) => Proxy p -> Integer
  protoId _ = natVal (Proxy @(ProtocolId p))

  decode :: Encoded e -> Maybe p
  encode :: p -> Encoded e


-- FIXME: slow and dumb
instance {-# OVERLAPPABLE #-} (MonadIO m, Num (Cookie e)) => GenCookie e m where
  genCookie salt = do
    r <- liftIO $ Random.randomIO @Int
    pure $ fromInteger $ fromIntegral $ asWord32 $ hash32 (hash salt + r)

