module Main where

import HBS2.Prelude
import HBS2.Net.Proto.Ref
import HBS2.Data.Types.Refs
import HBS2.Data.Types
import HBS2.OrDie
import HBS2.Net.Proto.Definition
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.UDP

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


main :: IO ()
main = do
  ref1 <- pure (fromStringMay @(Ref T) r1) `orDie` "can't parse"
  let r1s = show $ pretty (AsSyntax (DefineRef "g1" ref1))

  putStrLn r1s

  -- ref2 <- pure (fromStringMay @(Ref T) r1s) `orDie` "can't parse"

  -- assertBool "1" $ ref2 == ref1

  pure ()


