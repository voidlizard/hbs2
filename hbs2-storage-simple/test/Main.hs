module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TestSimpleStorage
import TestCompactStorage

main :: IO ()
main =
  defaultMain do
    testGroup "root"
      [
        testCase "testSimpleStorageRandomReadWrite" testSimpleStorageRandomReadWrite
      , testCase "testSimpleStorageNoKeys" testSimpleStorageNoKeys
      , testCase "testSimpleStorageRefs" testSimpleStorageRefs
      , testCase "testSimpleStorageBundles" testSimpleStorageBundles
      , testCase  "testSimpleStorageSymmEncryption" testSimpleStorageSymmEncryption
      , testCase  "testCompactStorage" testCompactStorageBasic
      ]
    -- testGroup "compact"
    --   [ testCase  "testCompactStorage" testCompactStorageBasic
    --   ]


