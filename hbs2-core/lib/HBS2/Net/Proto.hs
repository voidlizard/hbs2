module HBS2.Net.Proto
  ( module HBS2.Net.Proto
  , module HBS2.Net.Proto.Types
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types



newtype BlockInfo = BlockInfo Integer
                    deriving stock (Eq, Data)


