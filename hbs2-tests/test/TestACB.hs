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
import Data.Set qualified as Set
import Safe

data T

type SK = PubKey 'Sign T


maybeToSet = maybe mempty Set.singleton


main :: IO ()
main = do

  let pek1 = fromStringMay "5k9rLmFdXCP4RncG9WHEaXXEjxvnxmBvvMUqcKkoY45q"
  let pek2 = fromStringMay "FpZbzEbdFBztGUSXy5yCoWgkYUbJYDuCmSVxFTTrHx7D"

  let root   = maybeToSet $ fromStringMay @SK "sRyP45vd7wnopdLP6MLxUJAFGJu5wGVHyzF64mKwBbH"
  let owners = Set.fromList $ catMaybes [ fromStringMay "EJgvBg9bL2yKXk3GvZaYJgqpHy5kvpXdtEnAgoi4B5DN" ]
  let readers = Set.fromList $ catMaybes [pek1, pek2 ]

  let acb =   set acbRoot root
            . set acbOwners  ( owners <> root )
            . set acbWriters ( owners <> root )
            . set acbReaders readers
            $  mempty :: ACBSimple T

  let s = show $ pretty (AsSyntax (DefineACB "a1" acb))

  putStrLn s

  let macb2 = fromStringMay s :: Maybe (ACBSimple T)

  acb2 <- pure macb2 `orDie` "can't load ACB"

  print $ pretty (AsSyntax (DefineACB "a2" acb2))

  assertBool "1" $ view acbRoot acb == view acbRoot acb2
  assertBool "2" $ view acbOwners acb == view acbOwners acb2
  assertBool "3" $ view acbReaders acb == view acbReaders acb2
  assertBool "4" $ view acbWriters acb == view acbWriters acb2
  assertBool "5" $ view acbPrev acb == view acbPrev acb2

  assertBool "6" $ acb == acb2

  acb3' <- pure (fromStringMay @[(Id, ACBSimple T)] s) `orDie` "can't parse ACB"

  let acb3 = snd $ head acb3'

  print $ pretty (AsSyntax (DefineACB "a3" acb3))

  assertBool "7" (acb2 == acb)

  print $ "acb hash:" <+> pretty (hashObject @HbSync (serialise acb))

  -- TODO: acbPrev test



