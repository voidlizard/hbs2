-- |
-- Module      : Crypto.Saltine.Core.Auth
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
--
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Secret-key message authentication:
-- "Crypto.Saltine.Core.Auth"
--
-- The 'auth' function authenticates a message 'ByteString' using a
-- secret key. The function returns an authenticator. The 'verify'
-- function checks if it's passed a correct authenticator of a message
-- under the given secret key.
--
-- The 'auth' function, viewed as a function of the message for a
-- uniform random key, is designed to meet the standard notion of
-- unforgeability. This means that an attacker cannot find
-- authenticators for any messages not authenticated by the sender,
-- even if the attacker has adaptively influenced the messages
-- authenticated by the sender. For a formal definition see, e.g.,
-- Section 2.4 of Bellare, Kilian, and Rogaway, \"The security of the
-- cipher block chaining message authentication code,\" Journal of
-- Computer and System Sciences 61 (2000), 362–399;
-- <http://www-cse.ucsd.edu/~mihir/papers/cbc.html>.
--
-- Saltine does not make any promises regarding \"strong\"
-- unforgeability; perhaps one valid authenticator can be converted
-- into another valid authenticator for the same message. NaCl also
-- does not make any promises regarding \"truncated unforgeability.\"
--
-- "Crypto.Saltine.Core.Auth" is currently an implementation of
-- HMAC-SHA-512-256, i.e., the first 256 bits of
-- HMAC-SHA-512. HMAC-SHA-512-256 is conjectured to meet the standard
-- notion of unforgeability.
--
-- This is version 2010.08.30 of the auth.html web page.
module Crypto.Saltine.Core.Auth (
  Key, Authenticator,
  newKey,
  auth, verify
  ) where

import Crypto.Saltine.Internal.Auth
            ( c_auth
            , c_auth_verify
            , Key(..)
            , Authenticator(..)
            )
import Crypto.Saltine.Internal.Util as U
import Data.ByteString              (ByteString)

import qualified Crypto.Saltine.Internal.Auth as Bytes

-- | Creates a random key of the correct size for 'auth' and 'verify'.
newKey :: IO Key
newKey = Key <$> randomByteString Bytes.auth_keybytes

-- | Computes an keyed authenticator 'ByteString' from a message. It
-- is infeasible to forge these authenticators without the key, even
-- if an attacker observes many authenticators and messages and has
-- the ability to influence the messages sent.
auth :: Key
     -> ByteString
     -- ^ Message
     -> Authenticator
auth (Key key) msg =
  Au . snd . buildUnsafeByteString Bytes.auth_bytes $ \pa ->
    constByteStrings [key, msg] $ \[(pk, _), (pm, mlen)] ->
    c_auth pa pm (fromIntegral mlen) pk

-- | Checks to see if an authenticator is a correct proof that a
-- message was signed by some key.
verify :: Key
       -> Authenticator
       -> ByteString
       -- ^ Message
       -> Bool
       -- ^ Is this message authentic?
verify (Key key) (Au a) msg =
  unsafeDidSucceed $ constByteStrings [key, msg, a] $ \[(pk, _), (pm, mlen), (pa, _)] ->
    return $ c_auth_verify pa pm (fromIntegral mlen) pk
