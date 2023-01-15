module Main where

import TestFakeMessaging
import TestActors
import TestBlockInfoActor
import TestAbstractDispatch

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "testFakeMessaging1" testFakeMessaging1
      , testCase "testActorsBasic" testActorsBasic
      , testCase "testBlockInfoActor" testBlockInfoActor
      , testCase "testAbstractDispatch" testAbstractDispatch
      ]


