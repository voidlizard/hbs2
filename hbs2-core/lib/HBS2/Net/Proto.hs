module HBS2.Net.Proto where

import Data.Kind
import Data.Hashable

class (Hashable (Peer a), Eq (Peer a)) => IsPeer a where
  data family Peer a :: Type

