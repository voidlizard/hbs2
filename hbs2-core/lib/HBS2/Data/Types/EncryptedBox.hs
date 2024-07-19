module HBS2.Data.Types.EncryptedBox where

import HBS2.Prelude.Plated

import Codec.Serialise
import Data.ByteString (ByteString)

-- TODO: encryption-type-into-tags
-- FIXME: show-scrambled?
newtype EncryptedBox t = EncryptedBox { unEncryptedBox :: ByteString }
  deriving stock (Eq,Generic,Show,Data)

instance Serialise (EncryptedBox t)

