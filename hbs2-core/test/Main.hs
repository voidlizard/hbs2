module Main where

import TestFakeMessaging
import TestActors
import DialogSpec
import PrototypeGenericService
-- import TestUniqProtoId
-- import TestCrypto

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "testFakeMessaging1" testFakeMessaging1
      , testCase "testActorsBasic" testActorsBasic
      , testDialog
      , testCase "protoGenericService" protoGenericService
      ]


