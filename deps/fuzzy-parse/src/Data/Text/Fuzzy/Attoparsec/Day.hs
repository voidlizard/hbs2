module Data.Text.Fuzzy.Attoparsec.Day (  dayDMY
                                       , dayYMD
                                       , dayYYYYMMDD
                                       , dayDMonY
                                       , day
                                       ) where

import Data.List (zipWith)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser,decimal,digit,count,satisfy,inClass,skipWhile)
import Data.Time.Calendar (Day,fromGregorian,gregorianMonthLength)
import qualified Data.Char as Char
import qualified Data.Text as Text



day :: Parser Day
day = dayDMonY <|> dayYYYYMMDD <|> dayYMD <|> dayDMY

skipDelim :: Parser ()
skipDelim = skipWhile (inClass " ./-")

dayDMY :: Parser Day
dayDMY = do
  d <- decimal :: Parser Int
  skipDelim
  m <- decimal :: Parser Int
  skipDelim
  y' <- decimal :: Parser Integer
  maybe (fail "bad date format") pure (makeDay y' m d)

dayYMD :: Parser Day
dayYMD = do
  y' <- decimal :: Parser Integer
  skipDelim
  m <- decimal :: Parser Int
  skipDelim
  d <- decimal :: Parser Int
  maybe (fail "bad date format") pure (makeDay y' m d)

dayYYYYMMDD :: Parser Day
dayYYYYMMDD = do
  y <- fromIntegral . num n4 . map o <$> count 4 digit
  m <- num n2 . map o <$> count 2 digit
  d <- num n2 . map o <$> count 2 digit
  maybe (fail "bad date format") pure (makeDay y m d)
  where n4 = [1000,100,10,1]
        n2 = [10,1]
        o x = Char.ord x - Char.ord '0'
        num n x = sum $ zipWith (*) x n

dayDMonY :: Parser Day
dayDMonY = do
  d <- decimal :: Parser Int
  skipDelim
  m <- pMon
  skipDelim
  y <- decimal :: Parser Integer
  maybe (fail "bad date format") pure (makeDay y m d)
  where
    pMon :: Parser Int
    pMon = do
      txt <- Text.toUpper . Text.pack  <$> count 3 (satisfy Char.isLetter)
      case txt of
        "JAN" -> pure 1
        "FEB" -> pure 2
        "MAR" -> pure 3
        "APR" -> pure 4
        "MAY" -> pure 5
        "JUN" -> pure 6
        "JUL" -> pure 7
        "AUG" -> pure 8
        "SEP" -> pure 9
        "OCT" -> pure 10
        "NOV" -> pure 11
        "DEC" -> pure 12
        _     -> fail "bad month name"


makeYear :: Integer -> Maybe Integer
makeYear y' = if y < 1900 && y' < 99
                then Nothing
                else pure y
  where
    y = if y' < 50
          then y' + 2000
          else (if y' >= 50 && y' <= 99
                  then y' + 1900
                  else y' )

makeDay :: Integer -> Int -> Int -> Maybe Day
makeDay y m d | m <= 12 && m > 0 =
  makeYear y >>= \yyyy -> if d <= gregorianMonthLength yyyy m
                            then pure $ fromGregorian yyyy m d
                            else Nothing

              | otherwise = Nothing
