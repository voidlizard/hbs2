module HBS2.Data.Types
  ( module X
  -- , module HBS2.Data.Types.Crypto
  , AsSyntax(..)
  )
  where

import HBS2.Hash as X
import HBS2.Data.Types.Refs as X
import HBS2.Data.Types.Peer as X
-- import HBS2.Data.Types.Crypto

-- import Data.Config.Suckless

-- newtype FromSyntax c = FromSyntax [Syntax c]

newtype AsSyntax c = AsSyntax c


