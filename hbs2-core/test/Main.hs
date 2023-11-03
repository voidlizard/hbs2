module Main where

import TestFakeMessaging
import TestActors
import DialogSpec
import TestFileLogger
import TestScheduled

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "testFakeMessaging1" testFakeMessaging1
      , testCase "testActorsBasic" testActorsBasic
      , testCase "testFileLogger" testFileLogger
      , testCase "testScheduledActions" testScheduled

      -- FIXME does-not-finish
      -- , testDialog
      ]


