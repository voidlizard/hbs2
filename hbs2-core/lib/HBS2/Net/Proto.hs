module HBS2.Net.Proto
  ( module HBS2.Net.Proto
  , module HBS2.Net.Proto.Types
  ) where

import HBS2.Hash
import HBS2.Net.Proto.Types

type GetBlockSize h m = Hash h -> m (Maybe Integer)

