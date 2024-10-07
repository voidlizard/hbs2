{-# LANGUAGE OverloadedStrings #-}

module AuthProperties (
  testAuth
  ) where

import Util
import Crypto.Saltine.Core.Auth

import Test.Framework.Providers.QuickCheck2
import Test.Framework

testAuth :: Test
testAuth = buildTest $ do
  k <- newKey
  return $ testGroup "...Internal.Auth" [

    testProperty "Authenticates message"
    $ \(Message bs) -> verify k (auth k bs) bs == True

    ]
