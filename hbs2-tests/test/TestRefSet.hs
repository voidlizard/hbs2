module Main where

import HBS2.Prelude
import HBS2.Net.Proto.Ref
import HBS2.Net.Proto.ACB
import HBS2.Data.Types.Refs
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

class MonadIO m => IsRef a e m where
  createRef :: a -> m (Ref e)

r1 :: String
r1 = [qc|

define-ref g1
ref-nonce g1 "bcc72c0f-98ac-4938-a5b4-aa5e2c2f2027"
ref-acb-hash g1 "CFyjiywEM252G5B6Pa9M9CjcpG1b5buiHNUK4M4mL2JW"
ref-data g1 "FM9R1MHPwYZQGezShEVhB1v3FTKVpjjrs2SDQa4vEvAc"

|]


r2 :: String
r2 = [qc|

define-ref g1
ref-nonce g1 "bcc72c0f-98ac-4938-a5b4-aa5e2c2f2027"
; ref-acb-hash g1 "CFyjiywEM252G5B6Pa9M9CjcpG1b5buiHNUK4M4mL2JW"
; ref-data g1 "FM9R1MHPwYZQGezShEVhB1v3FTKVpjjrs2SDQa4vEvAc"
ref-acb-id g1 a1

define-acb a1

acb-root a1 "sRyP45vd7wnopdLP6MLxUJAFGJu5wGVHyzF64mKwBbH"
acb-owner a1 "EJgvBg9bL2yKXk3GvZaYJgqpHy5kvpXdtEnAgoi4B5DN"
acb-owner a1 "sRyP45vd7wnopdLP6MLxUJAFGJu5wGVHyzF64mKwBbH"
acb-reader a1 "5k9rLmFdXCP4RncG9WHEaXXEjxvnxmBvvMUqcKkoY45q"
acb-reader a1 "FpZbzEbdFBztGUSXy5yCoWgkYUbJYDuCmSVxFTTrHx7D"
acb-writer a1 "EJgvBg9bL2yKXk3GvZaYJgqpHy5kvpXdtEnAgoi4B5DN"
acb-writer a1 "sRyP45vd7wnopdLP6MLxUJAFGJu5wGVHyzF64mKwBbH"

|]


main :: IO ()
main = do
  ref1 <- pure (fromStringMay @(Ref T) r1) `orDie` "can't parse"
  let r1s = show $ pretty (AsSyntax (DefineRef "g1" ref1))

  putStrLn r1s

  ref2 <- pure (fromStringMay @(Ref T) r1s) `orDie` "can't parse"

  assertBool "1" $ ref2 == ref1

  ref2 <- pure (fromStringMay @(Ref T) r2) `orDie` "can't parse"

  let r2s = show $ pretty (AsSyntax (DefineRef "g1" ref2))

  a1 <- pure (fromStringMay @(ACBSimple T) r2) `orDie` "can't parse"

  putStrLn r2s

  assertBool "2" $ view refACB ref2 == HashRef (hashObject @HbSync (serialise a1))

  print $ "acb hash:" <+> pretty (hashObject @HbSync (serialise a1))

  pure ()


