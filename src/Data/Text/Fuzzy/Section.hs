module Data.Text.Fuzzy.Section (cutSectionBy, cutSectionOn) where

import Data.Text (Text)
import qualified Data.List as List


cutSectionOn :: Text -> Text -> [Text] -> [Text]
cutSectionOn a b txt = cutSectionBy ((==)a) ((==b)) txt

cutSectionBy :: (Text -> Bool) -> (Text -> Bool) -> [Text] -> [Text]
cutSectionBy a b txt = cutI
  where
    cutC = List.dropWhile (not . a) txt
    cutI = List.takeWhile (not . b) cutC

