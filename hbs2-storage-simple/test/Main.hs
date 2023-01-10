module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TestSimpleStorage

import HBS2.Storage

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "testSimpleStorageRandomReadWrite" testSimpleStorageRandomReadWrite
      , testCase "testSimpleStorageNoKeys" testSimpleStorageNoKeys
      ]





