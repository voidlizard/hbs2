module HBS2.Crypto where

import Control.Monad
import Crypto.Saltine.Class as SCl
import Crypto.Saltine.Core.Box qualified as Encrypt
import Crypto.Saltine.Internal.Box qualified as Encrypt

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.String.Conversions (cs)


combineNonceBS :: Encrypt.Nonce -> ByteString -> ByteString
combineNonceBS n = (SCl.encode n <>)

extractNonce :: ByteString -> Maybe (Encrypt.Nonce, ByteString)
extractNonce bs = do
    let (p,bs') = BS.splitAt Encrypt.box_noncebytes bs
    guard (BS.length p == Encrypt.box_noncebytes)
    nonce <- SCl.decode p
    pure (nonce, bs')

boxAfterNMLazy :: Encrypt.CombinedKey -> Encrypt.Nonce -> LBS.ByteString -> LBS.ByteString
boxAfterNMLazy k n = cs . combineNonceBS n . Encrypt.boxAfterNM k n . cs

boxOpenAfterNMLazy :: Encrypt.CombinedKey -> Encrypt.Nonce -> ByteString -> Maybe LBS.ByteString
boxOpenAfterNMLazy k n = fmap cs . Encrypt.boxOpenAfterNM k n
