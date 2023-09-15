{-# LANGUAGE UndecidableInstances #-}
module HBS2.Data.Types.SignedBox where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Credentials

import Codec.Serialise
import Data.Hashable
import Data.ByteString (ByteString)

data SignedBox p e =
  SignedBox (PubKey 'Sign (Encryption e)) ByteString (Signature (Encryption e))
  deriving stock (Generic)

deriving stock instance
  ( Eq (PubKey 'Sign (Encryption e))
  , Eq (Signature (Encryption e))
  ) => Eq (SignedBox p e)

instance ( Eq (PubKey 'Sign (Encryption e))
         , Eq (Signature (Encryption e))
         , Serialise (SignedBox p e)
         ) =>  Hashable (SignedBox p e) where
  hashWithSalt salt box = hashWithSalt salt (serialise box)


type ForSignedBox e = ( Serialise ( PubKey 'Sign (Encryption e))
                      , FromStringMaybe (PubKey 'Sign (Encryption e))
                      , Serialise (Signature (Encryption e))
                      , Hashable (PubKey 'Sign (Encryption e))
                      )

instance ForSignedBox e => Serialise (SignedBox p e)

