module HBS2.Net.Proto where

import Data.Hashable

class (Hashable a, Eq a) => IsPeer a where

