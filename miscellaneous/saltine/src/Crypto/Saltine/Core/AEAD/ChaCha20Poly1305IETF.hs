-- |
-- Module      : Crypto.Saltine.Core.AEAD.ChaCha20Poly1305IETF
-- Copyright   : (c) Thomas DuBuisson 2017
--               (c) Max Amanshauser 2021
-- License     : MIT
--
-- Maintainer  : max@lambdalifting.org
-- Stability   : experimental
-- Portability : non-portable
--
-- Secret-key authenticated encryption with additional data (AEAD):
-- "Crypto.Saltine.Core.AEAD.ChaCha20Poly1305IETF"
--
-- Generating nonces for the functions in this module randomly
-- is not recommended, due to the risk of generating collisions.

module Crypto.Saltine.Core.AEAD.ChaCha20Poly1305IETF (
  Key, Nonce,
  aead, aeadOpen,
  aeadDetached, aeadOpenDetached,
  newKey, newNonce
  ) where

import Crypto.Saltine.Internal.AEAD.ChaCha20Poly1305IETF
        ( c_aead
        , c_aead_open
        , c_aead_detached
        , c_aead_open_detached
        , Key(..)
        , Nonce(..)
        )
import Crypto.Saltine.Internal.Util as U
import Data.ByteString              (ByteString)
import Foreign.Ptr

import qualified Crypto.Saltine.Internal.AEAD.ChaCha20Poly1305IETF  as Bytes
import qualified Data.ByteString                                    as S

-- | Creates a random 'ChaCha20Poly1305IETF' key
newKey :: IO Key
newKey = Key <$> randomByteString Bytes.aead_chacha20poly1305_ietf_keybytes

-- | Creates a random 'ChaCha20Poly1305IETF' nonce
newNonce :: IO Nonce
newNonce = Nonce <$> randomByteString Bytes.aead_chacha20poly1305_ietf_npubbytes


-- | Encrypts a message. It is infeasible for an attacker to decrypt
-- the message so long as the 'Nonce' is never repeated.
aead
    :: Key
    -> Nonce
    -> ByteString
    -- ^ Message
    -> ByteString
    -- ^ AAD
    -> ByteString
    -- ^ Ciphertext
aead (Key key) (Nonce nonce) msg aad =
  snd . buildUnsafeByteString clen $ \pc ->
    constByteStrings [key, msg, aad, nonce] $ \
      [(pk, _), (pm, _), (pa, _), (pn, _)] ->
          c_aead pc nullPtr pm (fromIntegral mlen) pa (fromIntegral alen) nullPtr pn pk
  where mlen    = S.length msg
        alen    = S.length aad
        clen    = mlen + Bytes.aead_chacha20poly1305_ietf_abytes

-- | Decrypts a message. Returns 'Nothing' if the keys and message do
-- not match.
aeadOpen
    :: Key
    -> Nonce
    -> ByteString
    -- ^ Ciphertext
    -> ByteString
    -- ^ AAD
    -> Maybe ByteString
    -- ^ Message
aeadOpen (Key key) (Nonce nonce) cipher aad = do
  let clen   = S.length cipher
      alen   = S.length aad
  mlen <- clen `safeSubtract` Bytes.aead_chacha20poly1305_ietf_abytes
  let (err, vec) = buildUnsafeByteString mlen $ \pm ->
        constByteStrings [key, cipher, aad, nonce] $ \
          [(pk, _), (pc, _), (pa, _), (pn, _)] ->
            c_aead_open pm nullPtr nullPtr pc (fromIntegral clen) pa (fromIntegral alen) pn pk
  hush . handleErrno err $ vec

-- | Encrypts a message. It is infeasible for an attacker to decrypt
-- the message so long as the 'Nonce' is never repeated.
aeadDetached
    :: Key
    -> Nonce
    -> ByteString
    -- ^ Message
    -> ByteString
    -- ^ AAD
    -> (ByteString,ByteString)
    -- ^ Tag, Ciphertext
aeadDetached (Key key) (Nonce nonce) msg aad =
  buildUnsafeByteString clen $ \pc ->
   fmap snd . buildUnsafeByteString' tlen $ \pt ->
    constByteStrings [key, msg, aad, nonce] $ \
      [(pk, _), (pm, _), (pa, _), (pn, _)] ->
          c_aead_detached pc pt nullPtr pm (fromIntegral mlen) pa (fromIntegral alen) nullPtr pn pk
  where mlen    = S.length msg
        alen    = S.length aad
        clen    = mlen
        tlen    = Bytes.aead_chacha20poly1305_ietf_abytes

-- | Decrypts a message. Returns 'Nothing' if the keys and message do
-- not match.
aeadOpenDetached
    :: Key
    -> Nonce
    -> ByteString
    -- ^ Tag
    -> ByteString
    -- ^ Ciphertext
    -> ByteString
    -- ^ AAD
    -> Maybe ByteString
    -- ^ Message
aeadOpenDetached (Key key) (Nonce nonce) tag cipher aad
    | S.length tag /= tlen = Nothing
    | otherwise =
  let (err, vec) = buildUnsafeByteString len $ \pm ->
        constByteStrings [key, tag, cipher, aad, nonce] $ \
          [(pk, _), (pt, _), (pc, _), (pa, _), (pn, _)] ->
            c_aead_open_detached pm nullPtr pc (fromIntegral len) pt pa (fromIntegral alen) pn pk
  in hush . handleErrno err $ vec
  where len    = S.length cipher
        alen   = S.length aad
        tlen   = Bytes.aead_chacha20poly1305_ietf_abytes
