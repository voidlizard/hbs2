module Main where

import HBS2.Prelude
import HBS2.Net.Proto.Ref
import HBS2.Net.Proto.ACB
import HBS2.Data.Types
import HBS2.OrDie
import HBS2.Net.Proto.Definition()
import HBS2.Net.Auth.Credentials()
import HBS2.Net.Messaging.UDP

import Lens.Micro.Platform
import Text.InterpolatedString.Perl6 (qc)
import Test.Tasty.HUnit
import Prettyprinter

type T = UDP

r1 :: String
r1 = [qc|

;; reference creation script
;; there is always a seed ACB
;; it may or may not change further

define-acb a1

acb-root a1 "sRyP45vd7wnopdLP6MLxUJAFGJu5wGVHyzF64mKwBbH"
acb-owner a1 "EJgvBg9bL2yKXk3GvZaYJgqpHy5kvpXdtEnAgoi4B5DN"
acb-owner a1 "sRyP45vd7wnopdLP6MLxUJAFGJu5wGVHyzF64mKwBbH"
acb-reader a1 "5k9rLmFdXCP4RncG9WHEaXXEjxvnxmBvvMUqcKkoY45q"
acb-reader a1 "FpZbzEbdFBztGUSXy5yCoWgkYUbJYDuCmSVxFTTrHx7D"
acb-writer a1 "EJgvBg9bL2yKXk3GvZaYJgqpHy5kvpXdtEnAgoi4B5DN"
acb-writer a1 "sRyP45vd7wnopdLP6MLxUJAFGJu5wGVHyzF64mKwBbH"

define-ref g1 a1

|]



main :: IO ()
main = do
  ref1 <- pure (fromStringMay @(RefSeed T) r1) `orDie` "can't parse"
  let r1s = show $ pretty (AsSyntax (DefineRef "g1" "a" ref1))

  putStrLn r1s

  ref2 <- pure (fromStringMay @(RefSeed T) r1s) `orDie` "can't parse"

  assertBool "1" $ ref2 == ref1


