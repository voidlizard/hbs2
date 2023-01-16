{-# Language TypeFamilyDependencies  #-}
module HasProtocol where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

class HasProtocol a where
  type family ProtocolId  a = (id :: Nat) | id -> a
  type family Encoded a :: Type

  protoId :: forall . KnownNat (ProtocolId a) => Proxy a -> Integer
  protoId _ = natVal (Proxy @(ProtocolId a))

  decode :: String -> Maybe a
  encode :: a -> String
