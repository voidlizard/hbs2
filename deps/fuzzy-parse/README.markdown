# About

# Data.Text.Fuzzy.Tokenize

The lightweight and multi-functional text tokenizer allowing different types of text tokenization
depending on it's settings.

It may be used in different sutiations, for DSL, text markups or even for parsing simple grammars
easier and sometimes faster than in case of usage mainstream parsing combinators or parser
generators.

The primary goal of this package  is to parse unstructured text data, however it may be  used for
parsing  such data formats as CSV with ease.

Currently it supports the following types of entities: atoms, string literals (currently with the
minimal set of escaped characters), punctuation characters and delimeters.

## Examples

### Simple CSV-like tokenization

```haskell
tokenize (delims ":") "aaa : bebeb : qqq ::::" :: [Text]

["aaa "," bebeb "," qqq "]
```

```haskell
tokenize (delims ":"<>sq<>emptyFields ) "aaa : bebeb : qqq ::::" :: [Text]

["aaa "," bebeb "," qqq ","","","",""]
```

```haskell
tokenize (delims ":"<>sq<>emptyFields ) "aaa : bebeb : qqq ::::" :: [Maybe Text]

[Just "aaa ",Just " bebeb ",Just " qqq ",Nothing,Nothing,Nothing,Nothing]
```

```haskell
tokenize (delims ":"<>sq<>emptyFields ) "aaa : 'bebeb:colon inside' : qqq ::::" :: [Maybe Text]

[Just "aaa ",Just " ",Just "bebeb:colon inside",Just " ",Just " qqq ",Nothing,Nothing,Nothing,Nothing]
```

```haskell
let spec = sl<>delims ":"<>sq<>emptyFields<>noslits
tokenize spec "   aaa :   'bebeb:colon inside' : qqq ::::" :: [Maybe Text]

[Just "aaa ",Just "bebeb:colon inside ",Just "qqq ",Nothing,Nothing,Nothing,Nothing]
```

```haskell
let spec = delims ":"<>sq<>emptyFields<>uw<>noslits
tokenize spec "  a  b  c  : 'bebeb:colon inside' : qqq ::::"  :: [Maybe Text]

[Just "a b c",Just "bebeb:colon inside",Just "qqq",Nothing,Nothing,Nothing,Nothing]
```

### Primitive lisp-like language

```haskell
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

import Text.InterpolatedString.Perl6 (q)
import Data.Text.Fuzzy.Tokenize

data TTok = TChar Char
          | TSChar Char
          | TPunct Char
          | TText Text
          | TStrLit Text
          | TKeyword Text
          | TEmpty
          deriving(Eq,Ord,Show)

instance IsToken TTok where
 mkChar = TChar
 mkSChar = TSChar
 mkPunct = TPunct
 mkText = TText
 mkStrLit = TStrLit
 mkKeyword = TKeyword
 mkEmpty = TEmpty

main = do

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

   print toks
```

## Notes

### About the delimeter tokens
This type of tokens appears during a "delimited" formats processing and disappears in results.
Currenly you will never see it unless normalization is turned off by 'nn' option.

The delimeters make sense in case of processing the CSV-like formats, but in this case you probably
need only values in results.

This behavior may be changed later. But right now delimeters seem pointless in results. If you
process some sort of grammar where delimeter character is important, you may use punctuation
instead, i.e:

```haskell
let spec = delims " \t"<>punct ",;()" <>emptyFields<>sq
tokenize spec "( delimeters , are , important, 'spaces are not');" :: [Text]

["(","delimeters",",","are",",","important",",","spaces are not",")",";"]
```

### Other
For CSV-like formats it makes sense to split text to lines first, otherwise newline characters may
cause to weird results


# Authors

This library is written and maintained by Dmitry Zuikov, dzuikov@gmail.com

