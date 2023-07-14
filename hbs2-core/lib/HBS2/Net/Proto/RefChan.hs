{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module HBS2.Net.Proto.RefChan where

import HBS2.Prelude.Plated
-- import HBS2.Hash
-- import HBS2.Clock
-- import HBS2.Net.Proto
-- import HBS2.Net.Auth.Credentials
-- import HBS2.Base58
-- import HBS2.Events
-- import HBS2.Net.Proto.Peer
-- import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs

-- import HBS2.System.Logger.Simple

-- import Data.Maybe
-- import Data.Hashable
-- import Data.ByteString (ByteString)
-- import Type.Reflection (someTypeRep)
import Lens.Micro.Platform


data RefChanACL e =
  RefChanACLImmediate
  deriving stock (Generic)

data RefChanHead e =
  RefChanHead
  { _refChanHeadPrev    :: Maybe HashRef
  , _refChanHeadVersion :: Integer
  , _refChanHeadHistory :: Maybe HashRef
  , _refChanHeadACL     :: RefChanACL e
  }
  deriving stock (Generic)


makeLenses ''RefChanHead

