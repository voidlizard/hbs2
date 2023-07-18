module HBS2.Data.Types.Crypto where

import Codec.Serialise
import Crypto.Saltine.Core.Box qualified as Encrypt
import Crypto.Saltine.Core.Sign qualified as Sign

import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types
import HBS2.Prelude

-- type SignPubKey    = ()
-- type EncryptPubKey = ()

type instance PubKey  'Sign HBS2Basic = Sign.PublicKey
type instance PrivKey 'Sign HBS2Basic = Sign.SecretKey
type instance PubKey  'Encrypt HBS2Basic = Encrypt.PublicKey
type instance PrivKey 'Encrypt HBS2Basic = Encrypt.SecretKey

instance Serialise Sign.PublicKey
instance Serialise Encrypt.PublicKey
instance Serialise Sign.SecretKey
instance Serialise Encrypt.SecretKey

instance Serialise Sign.Signature

instance Signatures HBS2Basic where
  type Signature HBS2Basic = Sign.Signature
  makeSign = Sign.signDetached
  verifySign = Sign.signVerifyDetached
