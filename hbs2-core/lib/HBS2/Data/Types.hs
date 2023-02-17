module HBS2.Data.Types
  ( module HBS2.Hash
  , module HBS2.Data.Types.Refs
  -- , module HBS2.Data.Types.Crypto
  , AsSyntax(..)
  )
  where

import HBS2.Hash
import HBS2.Data.Types.Refs
-- import HBS2.Data.Types.Crypto

-- import Data.Config.Suckless

-- newtype FromSyntax c = FromSyntax [Syntax c]

newtype AsSyntax c = AsSyntax c

