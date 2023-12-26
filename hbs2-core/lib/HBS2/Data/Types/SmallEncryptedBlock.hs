module HBS2.Data.Types.SmallEncryptedBlock where


import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.Data.Types.EncryptedBox

import Data.ByteString (ByteString)
import Codec.Serialise

data SmallEncryptedBlock t =
  SmallEncryptedBlock
  { sebGK0   :: HashRef     -- ^ gk0
  , sebNonce :: ByteString
  , sebBox   :: EncryptedBox t
  }
  deriving stock (Generic)

instance Serialise  (SmallEncryptedBlock t)

