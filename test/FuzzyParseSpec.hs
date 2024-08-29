{-# LANGUAGE   OverloadedStrings
             , QuasiQuotes
             , ExtendedDefaultRules
             , LambdaCase
             , ImportQualifiedPost
             , DerivingStrategies
             , PatternSynonyms
             , ViewPatterns
             , MultiWayIf
             , TemplateHaskell
#-}

module FuzzyParseSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Text.InterpolatedString.Perl6 (q)

import Data.Text.Fuzzy.Tokenize
import Data.Data
import Data.Generics.Uniplate.Data()
import GHC.Generics

data TTok = TChar Char
          | TSChar Char
          | TPunct Char
          | TText Text
          | TStrLit Text
          | TKeyword Text
          | TEmpty
          | TIndent Int
          deriving stock (Eq,Ord,Show,Data,Generic)

instance IsToken TTok where
  mkChar = TChar
  mkSChar = TSChar
  mkPunct = TPunct
  mkText = TText
  mkStrLit = TStrLit
  mkKeyword = TKeyword
  mkEmpty = TEmpty
  mkIndent = TIndent



spec :: Spec
spec = do
  describe "csv-like" $ do
    it "splits text using ':' delimeter" $ do
      let toks = tokenize (delims ":") "aaa : bebeb : qqq ::::" :: [Text]
      toks `shouldBe` ["aaa "," bebeb "," qqq "]

    it "splits text using ':' delimeter with single-quotes string and empty fields" $ do
      let toks = tokenize (delims ":"<>sq<>emptyFields ) "aaa : bebeb : qqq ::::" :: [Text]
      toks `shouldBe` ["aaa "," bebeb "," qqq ","","","",""]

    it "splits text using ':' delimeter with single-quotes string and empty fields" $ do
      let toks = tokenize (delims ":"<>sq<>emptyFields ) "aaa : 'bebeb:colon inside' : qqq ::::" :: [Maybe Text]
      toks `shouldBe` [Just "aaa ",Just " ",Just "bebeb:colon inside",Just " ",Just " qqq ",Nothing,Nothing,Nothing,Nothing]

    it "splits text using ':' delimeter with single-quotes string and empty fields with noslits" $ do
      let spec = sl<>delims ":"<>sq<>emptyFields<>noslits
      let toks =  tokenize spec "   aaa :   'bebeb:colon inside' : qqq ::::" :: [Maybe Text]
      toks `shouldBe` [Just "aaa ",Just "bebeb:colon inside ",Just "qqq ",Nothing,Nothing,Nothing,Nothing]

    it "splits text using ':' delimeter with single-quotes string and empty fields with noslits and uw" $ do
      let spec = delims ":"<>sq<>emptyFields<>uw<>noslits
      let toks = tokenize spec "  a  b  c  : 'bebeb:colon inside' : qqq ::::"  :: [Maybe Text]
      toks `shouldBe` [Just "a b c",Just "bebeb:colon inside",Just "qqq",Nothing,Nothing,Nothing,Nothing]

    it "uses punctuation tokens" $ do
      let spec = delims " \t"<>punct ",;()" <>emptyFields<>sq
      let toks = tokenize spec "( delimeters , are , important, 'spaces are not');" :: [Text]
      toks `shouldBe` ["(","delimeters",",","are",",","important",",","spaces are not",")",";"]


    it "tokenize simple lisp-like text with keywords" $ do
      let spec = delims " \n\t" <> comment ";"
                                <> punct "{}()[]<>"
                                <> sq <> sqq
                                <> uw
                                <> keywords ["define","apply","+"]

      let code = [q|
        (define add (a b ) ; define simple function
          (+ a b) )
        (define r (add 10 20))
|]

      let toks = tokenize spec code :: [TTok]

      let expected = [ TPunct '('
                     , TKeyword "define"
                     , TText "add" , TPunct '(', TText "a" , TText "b", TPunct ')'
                     , TPunct '(', TKeyword "+", TText "a",TText "b",TPunct ')',TPunct ')'
                     , TPunct '(',TKeyword "define"
                                  ,TText "r"
                                  ,TPunct '(',TText "add",TText "10",TText "20"
                                  ,TPunct ')',TPunct ')']

      toks `shouldBe` expected


    describe "Checks indentation support" $ do

      let spec = delims " \n\t" <> comment ";"
                                <> punct "{}()[]<>"
                                <> sq <> sqq
                                <> uw
                                <> indent
                                <> itabstops 8
                                <> keywords ["define"]



      it "parses some indented blocks" $ do

        let expected = [ TIndent 0, TKeyword "define", TText "a", TText "0"
                       , TIndent 2, TText "atom", TText "foo", TText "2"
                       , TIndent 2, TKeyword "define", TText "aq", TText "2"
                       , TIndent 4, TText "atom", TText "one", TText "4"
                       , TIndent 4, TText "atom", TText "two", TText "4"
                       , TIndent 0, TKeyword "define", TText "b", TText "0"
                       , TIndent 2, TText "atom", TText "baar", TText "2"
                       , TIndent 2, TText "atom", TText "quux", TText "2"
                       , TIndent 2, TKeyword "define", TText "new", TText "2"
                       , TIndent 6, TText "atom", TText "bar", TText "6"
                       , TIndent 4, TText "atom", TText "fuu", TText "4"
                       , TIndent 0
                       ]

        let pyLike = [q|
define a      0
  atom foo    2
  define aq   2
    atom one  4
    atom two  4

define  b       0
  atom baar     2
  atom quux     2
  define new    2
      atom bar  6
    atom fuu    4

|]
        let toks = tokenize spec pyLike :: [TTok]
        toks `shouldBe` expected


