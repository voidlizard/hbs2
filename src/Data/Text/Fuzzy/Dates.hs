-- |
-- Module      :  Data.Text.Fuzzy.Dates
-- Copyright   :  Dmitry Zuikov 2020
-- License     :  MIT
--
-- Maintainer  :  dzuikov@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Dates fuzzy parsing.
-- Supports a number of dates format and tries to recover
-- the incomplete dates from text, with use of some
-- reasonable assumptions. Does not support locales,
-- i.e assums only English for dates yet.
--
-- == Examples
--
-- > parseMaybeDay "01.01.1979"
-- > Just 1979-01-01
-- > parseMaybeDay "01.01.01"
-- > Just 2001-01-01
-- > parseMaybeDay "13/01/2019"
-- > Just 2019-01-13
-- > parseMaybeDay "2019-12-1"
-- > Just 2019-12-01
-- > parseMaybeDay "21-feb-79"
-- > Just 1979-02-21
-- > parseMaybeDay "21-feb-01"
-- > Just 2001-02-21
-- > parseMaybeDay "29feb04"
-- > Just 2004-02-29
-- > parseMaybeDay "21feb28"
-- > Just 2028-02-21

module Data.Text.Fuzzy.Dates where

import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
import Data.Text.Fuzzy.Attoparsec.Day
import Data.Text (Text)
import Data.Time.Calendar

-- | Tries to parse a date from the text.
parseMaybeDay :: Text -> Maybe Day
parseMaybeDay s = either (const Nothing) pure (parseOnly day s)

