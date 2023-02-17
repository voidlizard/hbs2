{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language PatternSynonyms #-}
module Main where

import HBS2.Prelude
import HBS2.Net.Proto.ACB
import HBS2.Data.Types
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Definition
import HBS2.OrDie
-- import HBS2.Net.Messaging.UDP

import Test.Tasty.HUnit
import Data.Config.Suckless

import Data.Maybe
import Prettyprinter
import System.IO
import Lens.Micro.Platform
import Data.Either
import Data.Text qualified as Text
import Safe

data T

type SK = PubKey 'Sign T



main :: IO ()
main = do

  let pek1 = fromStringMay "5k9rLmFdXCP4RncG9WHEaXXEjxvnxmBvvMUqcKkoY45q"
  let pek2 = fromStringMay "FpZbzEbdFBztGUSXy5yCoWgkYUbJYDuCmSVxFTTrHx7D"

  let root   = fromStringMay @SK "sRyP45vd7wnopdLP6MLxUJAFGJu5wGVHyzF64mKwBbH"
  let owners = catMaybes [ fromStringMay "EJgvBg9bL2yKXk3GvZaYJgqpHy5kvpXdtEnAgoi4B5DN" ]

  let acb =   set acbRoot root
            . set acbOwners  ( owners <> maybeToList root )
            . set acbWriters ( owners <> maybeToList root )
            . set acbReaders ( catMaybes [pek1, pek2 ] )
            $  mempty :: ACBSimple T

  let s = show $ pretty (AsSyntax (DefineACB "a1" acb))

  putStrLn s

  let macb2 = fromStringMay s :: Maybe (ACBSimple T)

  acb2 <- pure macb2 `orDie` "can't load ACB"

  print $ pretty (AsSyntax (DefineACB "a1" acb2))

  assertBool "1" $ view acbRoot acb == view acbRoot acb2
  assertBool "2" $ view acbOwners acb == view acbOwners acb2
  assertBool "3" $ view acbReaders acb == view acbReaders acb2
  assertBool "4" $ view acbWriters acb == view acbWriters acb2
  assertBool "5" $ view acbPrev acb == view acbPrev acb2

  assertBool "6" $ acb == acb2

  -- TODO: acbPrev test

  pure ()


