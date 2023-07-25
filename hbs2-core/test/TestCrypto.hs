{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestCrypto where

import Test.QuickCheck.Instances.ByteString
import Test.Tasty
import Test.Tasty.QuickCheck as QC

-- import Control.Monad.Trans.Maybe
import Control.Monad
import Crypto.Saltine.Class qualified as Saltine
import Crypto.Saltine.Core.Box qualified as Encrypt
import Crypto.Saltine.Internal.Box qualified as Encrypt
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe
import Data.String.Conversions (cs)

import HBS2.Crypto


testCrypto :: TestTree
testCrypto = testGroup "testCrypto"
  [ QC.testProperty "roundtripCombineExtractNonce" prop_roundtripCombineExtractNonce
  , QC.testProperty "roundtripEncodingAfterNM" prop_roundtripEncodingAfterNM
  ]

instance Arbitrary Encrypt.Nonce where
  arbitrary = Encrypt.Nonce . BS.pack <$> vectorOf Encrypt.box_noncebytes arbitrary

instance Arbitrary Encrypt.SecretKey where
  arbitrary = (fromMaybe (error "Should be Just value") . Saltine.decode)
      . BS.pack <$> vectorOf Encrypt.box_beforenmbytes arbitrary

instance Arbitrary Encrypt.PublicKey where
  arbitrary = (fromMaybe (error "Should be Just value") . Saltine.decode)
      . BS.pack <$> vectorOf Encrypt.box_beforenmbytes arbitrary

prop_roundtripCombineExtractNonce :: (Encrypt.Nonce, ByteString) -> Bool
prop_roundtripCombineExtractNonce (n, b) =
    extractNonce (combineNonceBS n b) == Just (n, b)

prop_roundtripEncodingAfterNM :: (Encrypt.SecretKey, Encrypt.PublicKey, Encrypt.Nonce, ByteString) -> Bool
prop_roundtripEncodingAfterNM (sk, pk, n, b) = fromMaybe False do
    let
        ck = Encrypt.beforeNM sk pk

    let box = boxAfterNMLazy ck n (cs b)

    (n', x) <- extractNonce (cs box)
    guard (n' == n)
    b'' <- boxOpenAfterNMLazy ck n' x

    pure (cs b'' == b)
