module Data.Text.Fuzzy.Attoparsec.Month ( fuzzyMonth, fuzzyMonthFromText
                                        ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser,decimal,digit,letter,many1,parseOnly)
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar (Day,fromGregorian,gregorianMonthLength)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text


fuzzyMonth :: Parser Int
fuzzyMonth  = pMonthNum <|> pMonth

fuzzyMonthFromText :: Text -> Maybe Int
fuzzyMonthFromText = either (const Nothing) Just . parseOnly fuzzyMonth

pMonthNum :: Parser Int
pMonthNum = do
  n <- decimal
  if n >= 1 && n <= 13
    then pure n
    else fail "invalid months num"

pMonth :: Parser Int
pMonth = do
  mo <- many1 (Char.toLower <$> letter)
  maybe (fail "invalid month name") pure (Map.lookup mo months)
  where
    months :: Map String Int
    months = Map.fromList [ ("jan",  1), ("january"  ,  1)
                          , ("feb",  2), ("febuary"  ,  2)
                          , ("mar",  3), ("march"    ,  3)
                          , ("apr",  4), ("april"    ,  4)
                          , ("may",  5), ("may"      ,  5)
                          , ("jun",  6), ("june"     ,  6)
                          , ("jul",  7), ("july"     ,  7)
                          , ("aug",  8), ("august"   ,  8)
                          , ("sep",  9), ("september",  9)
                          , ("oct", 10), ("october"  , 10)
                          , ("nov", 11), ("november" , 11)
                          , ("dec", 12), ("december" , 12)
                          ]
