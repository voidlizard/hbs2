module Main where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup "root"
      [
        testCase "test1" test1
      ]


test1 :: IO ()
test1 = do
  pure ()
