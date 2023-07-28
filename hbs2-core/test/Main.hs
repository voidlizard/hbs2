module Main where

import TestFakeMessaging
import TestActors
import DialogSpec
-- import TestUniqProtoId
import TestCrypto

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "testFakeMessaging1" testFakeMessaging1
      , testCase "testActorsBasic" testActorsBasic
      -- , testCase "testUniqProtoId" testUniqProtoId
      , testCrypto
      , testDialog
      ]


