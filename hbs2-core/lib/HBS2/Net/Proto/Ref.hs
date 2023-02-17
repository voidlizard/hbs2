{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.Ref where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Net.Auth.Credentials

import Data.ByteString (ByteString)
import Lens.Micro.Platform
import Codec.Serialise
import Data.Set (Set)

data Ref e = Ref
  { _refNonce :: ByteString
  , _refSign  :: Signature e
  , _refACB   :: HashRef
  , _refData  :: HashRef
  }
  deriving stock (Generic)

makeLenses 'Ref


type IsRef e = (Eq (Signature e), Serialise (Signature e))

deriving stock instance IsRef e => Eq (Ref e)

instance IsRef e => Serialise (Ref e)


