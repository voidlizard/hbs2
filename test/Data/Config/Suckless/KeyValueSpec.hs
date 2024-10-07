{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Config.Suckless.KeyValueSpec (spec) where

import Data.Function
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Config.Suckless.KeyValue
import Data.Config.Suckless.Parse
import Data.Config.Suckless.Syntax
import Data.Functor
import Data.Scientific
import Data.Text.IO qualified as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter
import Data.Aeson
import Text.InterpolatedString.Perl6 (qc,q)
import Test.Hspec


data FirstKey

data SecondKey

data ThirdKey

data Int1
data Int2
data Int3
data Int4
data Int5
data Int6

data Sci1
data Sci2
data Sci3
data Sci4
data Sci5

data O1
data O2

instance HasCfgKey FirstKey (Maybe String) where
  key = "foo"

instance HasCfgKey SecondKey (Set String) where
  key = "bar"

instance HasCfgKey ThirdKey (Maybe String) where
  key = "baz"

instance HasCfgKey Int1 b where
  key = "int1"

instance HasCfgKey Int2 b where
  key = "int2"

instance HasCfgKey Int3 b where
  key = "int3"

instance HasCfgKey Int4 b  where
  key = "int4"

instance HasCfgKey Int5 b where
  key = "int5"

instance HasCfgKey Int6 b where
  key = "int6"

instance HasCfgKey Sci1 b where
  key = "sci1"

instance HasCfgKey Sci2 b where
  key = "sci2"

instance HasCfgKey Sci3 b where
  key = "sci3"

instance HasCfgKey Sci4 b where
  key = "sci4"

instance HasCfgKey Sci5 b where
  key = "sci5"

instance HasCfgKey O1 b where
  key = "some-object"

instance HasCfgKey O2 b where
  key = "another-object"

instance HasConf IO where
  getConf = liftIO readConfig

readConfig :: IO [Syntax C]
readConfig = do
  let configFilePath = "t/key-value-test-config"
  f <- Text.readFile configFilePath <&> parseTop <&> either mempty id
  print $ pretty f
  pure f

spec :: Spec
spec = do
  describe "config parsing" $ do

    it "reads string" $ do
      firstValue <- cfgValue @FirstKey @(Maybe String)
      firstValue `shouldBe` Just "a"

    it "reads a set of strings" $ do
      secondValue <- cfgValue @SecondKey @(Set String)
      secondValue `shouldBe` Set.fromList ["a", "b"]

    it "reads nothing" $ do
      thridValue <- cfgValue @ThirdKey @(Maybe String)
      thridValue `shouldBe` Nothing

    it "reads ints" $ do
      x1 <- cfgValue @Int1 @(Maybe Integer)
      x1 `shouldBe` Just 122

      x2 <- cfgValue @Int2
      x2 `shouldBe` Just (0 :: Integer)

      x3 <- cfgValue @Int3
      x3 `shouldBe` Just (-22 :: Integer)

      x4 <- cfgValue @Int4 @(Maybe Integer)
      x4 `shouldBe` Just 0xFAFA

      x5 <- cfgValue @Int5 @(Maybe Integer)
      x5 `shouldBe` Just 255

      x6 <- cfgValue @Int6 @(Maybe Integer)
      x6 `shouldBe` Just (-0xFAFA)

    it "reads scientifics" $ do
      x1 <- cfgValue @Sci1 @(Maybe Scientific)
      x1 `shouldBe` Just 1e9

      x2 <- cfgValue @Sci2 @(Maybe Scientific)
      x2 `shouldBe` Just 0.003

      -- x3 <- cfgValue @Sci3 @(Maybe Scientific)
      -- x3 `shouldBe` Just (-0.001)

      x4 <- cfgValue @Sci4 @(Maybe Scientific)
      x4 `shouldBe` Just (-2e11)

      x5 <- cfgValue @Sci5 @(Maybe Scientific)
      x5 `shouldBe` Just (-2e-3)

    it "reads objects" $ do
      o1 <- cfgValue @O1 @(Maybe Value)
      let wtf1 = [q|{ "key" : 42 }|]
      o1 `shouldBe` decode wtf1
      let wtf2 = [q|
        {   "key1" : 42
          , "key2" : false
          , "key3" : [ 1, 2, 3, 4]
        }
      |]
      o2 <- cfgValue @O2 @(Maybe Value)
      o2 `shouldBe` decode wtf2


    it "works in Reader" $ do

      let cfg = [qc|
int1 123

|]
      let conf = parseTop cfg & either mempty id

      let v = runReader (cfgValue @Int1 @(Maybe Int)) conf

      v `shouldBe` Just 123



