module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Base58 as B58
import HBS2.Hash
import HBS2.Net.Proto.Types
import HBS2.Peer.Proto.RefLog
import HBS2.Net.Auth.Schema
import HBS2.Misc.PrettyStuff

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe
import Data.ByteString
import Data.ByteString.Lazy qualified as LBS
import Codec.Serialise
import Crypto.Saltine.Core.Sign qualified as Sign


newtype W a = W a
              deriving stock Generic

instance Serialise a => Serialise (W a)


newtype X a = X a
              deriving stock Generic

instance Serialise a => Serialise (X a)

newtype VersionedPubKey = VersionedPubKey { versionedPubKey :: ByteString }
                          deriving stock (Show,Generic)

data RefLogRequestVersioned e =
  RefLogRequestVersioned
  { refLogRequestVersioned :: VersionedPubKey
  }
  deriving stock (Show,Generic)

instance Serialise VersionedPubKey

instance Serialise (RefLogRequestVersioned e)

testVersionedKeysHashes :: IO ()
testVersionedKeysHashes = do

  keypart <- fromBase58 "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
               & orThrowUser "bad base58"
               <&> LBS.fromStrict

  pk <- fromStringMay @(PubKey 'Sign 'HBS2Basic) "BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP"
          & orThrowUser "key decode"

  let pks = serialise pk

  pks2 <- deserialiseOrFail @(PubKey 'Sign 'HBS2Basic) (pks <> "12345")
           & orThrowUser "key decode error"

  let rfk = serialise (RefLogKey @'HBS2Basic pk)
  let wrfk = serialise $ W (RefLogKey @'HBS2Basic pk)
  let xrfk = serialise $ X (RefLogKey @'HBS2Basic pk)

  print $ pretty (AsHexSparse keypart)
  print $ pretty (AsHexSparse pks)
  print $ pretty (AsHexSparse rfk)
  print $ pretty (AsHexSparse wrfk)
  print $ pretty (AsHexSparse xrfk)

  let req1 = RefLogRequest @L4Proto pk

  let req2 = RefLogRequestVersioned @L4Proto ( VersionedPubKey (LBS.toStrict keypart <> "AAA") )

  print $ yellow "okay"

  let req1s = serialise req1
  let req2s = serialise req2

  print $ pretty "---"

  print $ pretty (AsHexSparse req1s)
  print $ pretty (AsHexSparse req2s)

  rq0 <- deserialiseOrFail @(RefLogRequestVersioned L4Proto) req1s
           & orThrowUser "failed simple -> versioned"

  rq1 <- deserialiseOrFail @(RefLogRequest L4Proto) req2s
           & orThrowUser "failed versioned -> simple"

  print $ viaShow rq0
  print $ viaShow req1

  print $ viaShow rq1

  pure ()

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "testVersionedKeys" testVersionedKeysHashes
      ]



