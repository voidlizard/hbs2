module Main where

import TestFakeMessaging
import TestActors
import DialogSpec
import TestFileLogger
import TestScheduled
import TestDerivedKey

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
      , testCase "testDerivedKeys1" testDerivedKeys1

      -- FIXME does-not-finish
      -- , testDialog
      ]


