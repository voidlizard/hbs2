module TestDerivedKey where

import HBS2.Prelude
import HBS2.OrDie
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Schema
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox

import Test.Tasty.HUnit

import Lens.Micro.Platform
import Data.Word

testDerivedKeys1 :: IO ()
testDerivedKeys1 = do

  cred <- newCredentials @'HBS2Basic

  let _ = view peerSignPk cred
  let sk = view peerSignSk cred

  let nonce = 0x123456780928934 :: Word64
  (pk1,sk1) <- derivedKey @'HBS2Basic @'Sign nonce sk

  let box = makeSignedBox @L4Proto pk1 sk1 (42 :: Word32)

  (pk, n) <- pure (unboxSignedBox0 box)
              `orDie` "can not unbox"

  assertEqual "signed-box-unpacked" n 42

  print $ "ZBS!" <+> pretty n

  pure ()
