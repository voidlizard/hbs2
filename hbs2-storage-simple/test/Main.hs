module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TestSimpleStorage

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "testSimpleStorageRandomReadWrite" testSimpleStorageRandomReadWrite
      , testCase "testSimpleStorageNoKeys" testSimpleStorageNoKeys
      , testCase "testSimpleStorageRefs" testSimpleStorageRefs
      , testCase "testSimpleStorageBundles" testSimpleStorageBundles
      , testCase  "testSimpleStorageSymmEncryption" testSimpleStorageSymmEncryption
      ]


