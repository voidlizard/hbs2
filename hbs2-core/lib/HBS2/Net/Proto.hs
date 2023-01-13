module HBS2.Net.Proto where

import HBS2.Prelude.Plated

import Data.Kind
import Data.Hashable

class (Hashable (Peer a), Eq (Peer a)) => IsPeer a where
  data family Peer a :: Type


newtype BlockInfo = BlockInfo Integer
                    deriving stock (Eq, Data)


