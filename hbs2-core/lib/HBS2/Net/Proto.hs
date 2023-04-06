module HBS2.Net.Proto
  ( module HBS2.Net.Proto
  , module HBS2.Net.Proto.Types
  ) where

import HBS2.Hash
import HBS2.Net.Proto.Types


dontHandle :: Applicative f => a -> f ()
dontHandle = const $ pure ()

type GetBlockSize h m = Hash h -> m (Maybe Integer)

