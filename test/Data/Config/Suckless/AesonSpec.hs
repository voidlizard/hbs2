{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}


{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Config.Suckless.AesonSpec (spec) where

import Data.Config.Suckless.KeyValue
import Data.Config.Suckless.Parse
import Data.Config.Suckless.Syntax
import Data.Functor
import Data.Function
import Data.Scientific
import Data.Text (Text)
import Data.Text.IO qualified as Text

import GHC.Generics hiding (C)
import Text.InterpolatedString.Perl6 (qc,q)
import Data.Aeson
import Data.Maybe
import Control.Monad.Reader
import Test.Hspec
import Test.Tasty.HUnit
import Prettyprinter


readConfig :: Text -> IO [Syntax C]
readConfig s = do
  pure $ parseTop s & either mempty id
  -- print $ pretty f
  -- pure f

data SomeData =
  SomeData
  { someDataKey1 :: Int
  , someDataKey2 :: String
  , someDataKey3 :: [Scientific]
  }
  deriving stock (Generic,Show,Eq)

instance ToJSON SomeData
instance FromJSON SomeData

data Port
data Users

instance HasCfgKey Port (Maybe Int)
  where key = "port"

instance HasCfgKey Users [Value]
  where key = "basic-users"

spec :: Spec
spec = do
  describe "toJSON" $ do

    it "reads int" $ do
      c <- readConfig [qc|1|] <&> toJSON
      c `shouldBe` toJSON [[1::Int]]

    it "reads scientific" $ do
      c <- readConfig [qc|1.00|] <&> toJSON
      c `shouldBe` toJSON [[1.00 :: Scientific]]

    it "reads bool" $ do
      t <- readConfig [qc|#t|]  <&> toJSON . head
      t `shouldBe` toJSON [Bool True]
      f <- readConfig [qc|#f|] <&> toJSON . head
      f `shouldBe` toJSON [Bool False]

    it "reads string" $ do
      s <- readConfig [qc|"somestring"|] <&> toJSON
      s `shouldBe` toJSON [["somestring" :: String]]

    it "reads array" $ do
      s <- readConfig [qc|(1 2 3 4)|] <&> toJSON . head
      print s
      s `shouldBe` toJSON [1::Int,2,3,4]

    it "reads simple object" $ do
      s <- readConfig [qc|
        (object
            (key1 : 22)
            (key2 : #f)
            (key3 : [1 2 3 4])
            (key4 : (object (o1 : "bebe")) )
            ("fafa" : "fifa")
            (none : #nil)
        )
      |] <&> toJSON . head

      let s1 = decode @Value [q|
        {
            "key1": 22,
            "key2": false,
            "key3": [1, 2, 3, 4],
            "key4": {
                "o1": "bebe"
            },
            "fafa" : "fifa",
            "none" : null
        }

      |]

      print s
      print s1
      Just s `shouldBe` s1


    it "serializes object to syntax"  $ do
      let some = SomeData 1 "some-data" [1, 2, 3, 4, 5, 10]

      let someSyn = case fromJSON @(Syntax ()) (toJSON some) of
                      Success syn -> Just syn
                      _           -> Nothing

      print $ pretty someSyn

      let json = fromJust $ someSyn <&> toJSON

      let someObject = fromJSON @SomeData json

      print someObject
      someObject `shouldBe` Success some

    it "read-real-config" do
      let cfg = [q|

port 3000

hbs2-url "http://localhost:5001"

default-token-name "LCOIN"

hbs2-keyring "/home/hbs2/lcoin-adapter/secrets/hbs2.key"

; old test thermoland reflog
hbs2-keyring "/home/hbs2/lcoin-adapter/secrets/termoland-reflog-GX8gmPi2cAxxgnaKmLmR5iViup1BNkwpdCCub3snLT1y.key"

; new test thermoland reflog
hbs2-keyring "/home/hbs2/lcoin-adapter/secrets/termoland-reflog-AdowWzo4iW1JejHFRnPnxQWNot8uL5sciFup6RHx2gZG.key"



hbs2-keyring "/home/hbs2/keys/lcoin-belorusskaya-JAiAjKzfWfTGXjuSf4GXaj44cWfDQ8vifxoQU3tq5hn7.key"
hbs2-keyring "/home/hbs2/keys/lcoin-krymskaya-CEDBX2niVK3YL7WxzLR3xj8iUNHa9GU2EfXUqDU7fSGK.key"
hbs2-keyring "/home/hbs2/keys/lcoin-ushakova-GyTXGiCUJu81CMXYZzs7RhHu4vxJnLYgT3n2neXG5uaY.key"
hbs2-keyring "/home/hbs2/keys/lcoin-zelenopark-4fFvFGzQRp2WSXtDHepeJvMtCfQdSASq9qmsELWRJDgv.key"



jwk-path "/home/hbs2/lcoin-adapter/secrets/jwk/public_key.jwk"

jwk-path "/home/hbs2/lcoin-adapter/secrets/jwk/public-key-2023-11-03.jwk"

lcoin-rate 5

db-path "/home/hbs2/.local/share/lcoin-adapter/state.db"

registration-bonus 500

log-file "/home/hbs2/lcoin-adapter/log.txt"

; qblf-socket  "/tmp/qblf.socket"

qblf-treasure "64zvWqGUf57WmGCTFWrVaNEqXikUocGyKFtg5mhyWCiB"

reports-ignore-key "DyKWNLvpRSsTsJfVxTciqxnCJ6UhF4Mf6WoMw5qkftG4"
reports-ignore-key "3MjGvpffawUijHxbbsaF9J6wt4YReRdArUCTfHo1RhSm"


;; v2
db-journal "/tmp/lcoin-adapter-journal.sqlite"
db-cache "/tmp/lcoin-adapter-cache-db.sqlite"
hbs2-store "/tmp/hbs2-store"

treasure "64zvWqGUf57WmGCTFWrVaNEqXikUocGyKFtg5mhyWCiB"

keybox "http://localhost:8034/"
dev-env false
(jwk-keys (
    "/home/hbs2/lcoin-adapter/secrets/jwk/public_key.jwk"
    "/home/hbs2/lcoin-adapter/secrets/jwk/public-key-2023-11-03.jwk"
    ))

(basic-users (
    (object (name "mobile") (pass "mobile-pass"))
    (object (name "termo") (pass "termo-pass"))
    ))

(client-creator "BYVqWJdn18Q3AjmJBPw2yusZ5ouNmgiRydWQgBEh684J")
(client-creator-keyring "/home/hbs2/keys/journal/client-creator_BYVqWJdn18Q3AjmJBPw2yusZ5ouNmgiRydWQgBEh684J.key")

(coin-minter "4Gnno5yXUbT5dwfphKtDW7dWeq4uBvassSdbVvB3y67p")
(coin-minter-keyring "/home/hbs2/keys/journal/coin-minter_4Gnno5yXUbT5dwfphKtDW7dWeq4uBvassSdbVvB3y67p.key")
      |] :: Text


      let what = parseTop cfg & either (error.show) id

      let pno = runReader (cfgValue @Port @(Maybe Int)) what
      -- what

      assertEqual "pno" pno (Just 3000)



