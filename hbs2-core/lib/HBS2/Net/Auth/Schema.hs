{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
module HBS2.Net.Auth.Schema
  ( module HBS2.Net.Auth.Schema
  , module HBS2.Net.Proto.Types
  ) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Net.Proto.Types
import HBS2.Hash
import HBS2.Net.Messaging.Unix

import Data.Word
import Crypto.Error
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.KDF.HKDF qualified as HKDF
import Crypto.Saltine.Class qualified as Saltine
import Crypto.Saltine.Class (IsEncoding(..))
import Crypto.Saltine.Core.Sign qualified as Sign
import Crypto.Saltine.Core.Box qualified as Encrypt

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString (ByteString)
import Data.ByteArray ( convert)


-- type ForSignatures s = ( Serialise ( PubKey 'Sign s)
--                        , FromStringMaybe (PubKey 'Sign s)
--                        , Signatures s
--                        )

type instance Encryption L4Proto = 'HBS2Basic

type instance Encryption UNIX = 'HBS2Basic

type ForDerivedKey s = (IsEncoding (PrivKey 'Sign s), IsEncoding (PubKey 'Sign s))

type instance PubKey  'Sign 'HBS2Basic = Sign.PublicKey
type instance PrivKey 'Sign 'HBS2Basic = Sign.SecretKey
type instance PubKey  'Encrypt 'HBS2Basic = Encrypt.PublicKey
type instance PrivKey 'Encrypt 'HBS2Basic = Encrypt.SecretKey

-- type PrivKey 'Encrypt s

-- type instance PubKey 'Sign

instance (MonadIO m, ForDerivedKey s, s ~ 'HBS2Basic) => HasDerivedKey s 'Sign Word64 m where
  derivedKey nonce sk = do

    sk0 <- liftIO $ throwCryptoErrorIO (Ed.secretKey k0)
    let pk0 = Ed.toPublic sk0

    let bs0 = convert sk0 :: ByteString
    let bs1 = convert pk0 :: ByteString

    sk1 <- Saltine.decode (bs0 <> bs1)
            & orThrow CryptoError_SecretKeySizeInvalid

    pk1 <- Saltine.decode bs1
            & orThrow CryptoError_PublicKeySizeInvalid

    pure (pk1, sk1)

    where
       ikm = Saltine.encode sk
       salt = LBS.toStrict (serialise nonce)
       prk = HKDF.extract @(HashType HbSync) salt ikm
       k0  = HKDF.expand @_ @_ @ByteString prk salt Ed.secretKeySize




