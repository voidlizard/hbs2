{-# Language TypeFamilyDependencies #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
module HBS2.Net.Proto.Types
  ( module HBS2.Net.Proto.Types
  ) where

import HBS2.Clock

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.Hashable
import Control.Monad.IO.Class
import System.Random qualified as Random
import Data.Digest.Murmur32
import Data.ByteString (ByteString)
import Lens.Micro.Platform
import Data.Text (Text)

-- e -> Transport (like, UDP or TChan)
-- p -> L4 Protocol (like Ping/Pong)

class Monad m => GenCookie e m where
  genCookie :: Hashable salt => salt -> m (Cookie e)


class Monad m => HasNonces p m where
  type family Nonce p :: Type
  newNonce :: m (Nonce p)



class HasCookie e p | p -> e where
  type family Cookie e :: Type
  getCookie :: p -> Maybe (Cookie e)
  getCookie = const Nothing

type PeerNonce = Nonce ()

class HasPeerNonce e m where
  peerNonce :: m PeerNonce



data WithCookie e p = WithCookie (Cookie e) p

class (Hashable (Peer e), Eq (Peer e)) => HasPeer e where
  data family (Peer e) :: Type

class Monad m => IsPeerAddr e m where
  type family PeerAddr e :: Type

  toPeerAddr   :: Peer e -> m (PeerAddr e)
  fromPeerAddr :: PeerAddr e -> m (Peer e)

class (Monad m, HasProtocol e p) => HasThatPeer e p (m :: Type -> Type) where
  thatPeer :: Proxy p -> m (Peer e)

class (MonadIO m, HasProtocol e p) => HasDeferred e p m | p -> e where
  deferred :: Proxy p -> m () -> m ()


class ( MonadIO m
      , HasProtocol e p
      , HasThatPeer e p m
      ) => Response e p m | p -> e where

  response :: p -> m ()

class Request e p (m :: Type -> Type) | p -> e where
  request :: Peer e -> p -> m ()

data ReqLimPeriod = NoLimit
                  | ReqLimPerProto   (Timeout 'Seconds)
                  | ReqLimPerMessage (Timeout 'Seconds)

class (KnownNat (ProtocolId p), HasPeer e) => HasProtocol e p | p -> e  where
  type family ProtocolId p = (id :: Nat) | id -> p
  type family Encoded e :: Type

  protoId :: forall . KnownNat (ProtocolId p) => Proxy p -> Integer
  protoId _ = natVal (Proxy @(ProtocolId p))

  decode :: Encoded e -> Maybe p
  encode :: p -> Encoded e

  requestPeriodLim :: ReqLimPeriod
  requestPeriodLim = NoLimit

-- FIXME: slow and dumb
instance {-# OVERLAPPABLE #-} (MonadIO m, Num (Cookie e)) => GenCookie e m where
  genCookie salt = do
    r <- liftIO $ Random.randomIO @Int
    pure $ fromInteger $ fromIntegral $ asWord32 $ hash32 (hash salt + r)

