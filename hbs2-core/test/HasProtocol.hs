{-# Language TypeFamilyDependencies  #-}
module HasProtocol where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

class HasProtocol a where
  type family ProtocolId  a = (id :: Nat) | id -> a
  type family Encoded a :: Type
  type family Peer a :: Type

  protoId :: forall . KnownNat (ProtocolId a) => Proxy a -> Integer
  protoId _ = natVal (Proxy @(ProtocolId a))

  decode :: Encoded a -> Maybe a
  encode :: a -> Encoded a

