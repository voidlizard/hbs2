{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Fixme.Scan (scanBlob) where

import Fixme.Prelude hiding (indent)
import Fixme.Types

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text

import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)

import Data.Int
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.HashSet qualified as HS
import Data.Coerce

{- HLINT ignore "Functor law" -}

data Sx =  S0      -- initial state
         | Sc      -- in-comment state
         | Scc     -- in-comment-after-space
         | Sf Int ByteString ByteString -- fixme-found-at-indent
         deriving stock (Show)

data E =
  E Int Int64 [ByteString]
  deriving stock (Show)

data S = S Sx E
         deriving stock (Show)

pattern CurrentChar :: Char -> S -> S
pattern CurrentChar c s <- s@(S _ ( currentChar -> Just c ))

pattern EndOfLine :: S -> S
pattern EndOfLine s <- s@(S _ ( currentChar -> Nothing ))

pattern EndOfInput :: S -> S
pattern EndOfInput s <- s@(S _ (E _ _ []))

currentChar :: E -> Maybe Char
currentChar = \case
  (E _ n (x:_)) -> LBS8.indexMaybe x n
  (E _ _ [])    -> Nothing

currentChunk :: E -> ByteString
currentChunk (E _ off (x:_)) = LBS8.drop off x
currentChunk _ = mempty

indent :: E -> Int
indent (E n _ _) = n

-- increase current indent level by i
shiftI :: Int -> E -> E
shiftI i (E j o s) = E (i+j) o s

setI :: Int -> E -> E
setI i (E _ o s) = E i o s

move :: Int64 -> E -> E
move i (E x p s) = E x (p+i) s

dropLine :: E -> E
dropLine (E x _ s) = E x 0 (drop 1 s)

scanBlob :: FixmePerks m
         => Maybe FilePath   -- ^ filename to detect type
         -> ByteString       -- ^ content
         -> FixmeM m [Fixme]

scanBlob fpath lbs = do

  tagz' <- asks fixmeEnvTags
              >>= readTVarIO
              <&> HS.toList
              <&> fmap (Text.unpack . coerce)
              <&> filter (not . null)

  let tagz = [ (head t, LBS8.pack t) | t <- tagz' ] & Map.fromList

  comments <- fixmeGetCommentsFor fpath
                <&> filter (not . null) . fmap Text.unpack

  let co = Map.fromList [ (head s, LBS8.pack s) | s <- comments ]

  let ls = LBS8.lines lbs

  -- TODO: ASAP-no-comment-case
  --   для текстовых файлов нет комментариев,
  --   этот кейс тоже надо обработать (из S0!)

  flip fix (S S0 (E 0 0 ls)) $ \next  -> \case

    EndOfInput{} -> pure ()

    -- S0
    CurrentChar ' ' (S S0 e) -> do
      next (S S0 (shiftI 1 $ move 1 $ e))

    CurrentChar '\t' (S S0 e) -> do
      next (S S0 (shiftI 8 $ move 1 $ e))

    -- maybe-start-of-comment
    CurrentChar c (S S0 e) | Map.member c co -> do
      let comm = Map.lookup c co & fromMaybe (LBS8.singleton c)

      if LBS8.isPrefixOf comm (currentChunk e) then
         next (S Sc (shiftI 1 $ move (max 1 (LBS8.length comm)) $ e))
      else do
         -- scanning-further
         next (S S0 (move 1 $ e))

    CurrentChar _ (S S0 e) -> do
      next (S S0 (move 1 $ e))

    -- Sc

    CurrentChar ' ' (S Sc e) -> do
      next (S Scc (shiftI 1 $ move 1 $ e))

    CurrentChar '\t' (S Sc e) -> do
      next (S Scc (shiftI 8 $ move 1 $ e))

    -- Scc

    CurrentChar ' ' (S Scc e) -> do
      next (S Scc (shiftI 1 $ move 1 $ e))

    CurrentChar '\t' (S Scc e) -> do
      next (S Scc (shiftI 8 $ move 1 $ e))

    CurrentChar c (S Scc e) | Map.member c tagz -> do

      let tag = Map.lookup c tagz & fromJust -- works, cause Map.member

      if LBS8.isPrefixOf tag (currentChunk e) then
         next (S (Sf (indent e) tag mempty) (move (max 1 (LBS8.length tag)) $ e))
        -- это тег
        -- переходим в обработку fixme
        -- запоминаем indent
      else
         next (S Scc (shiftI 1 $ move 1 $ e))
         -- это не тег, но и не пробел
         -- едем дальше по коменту
         -- двигаем ли indent? ну чо нет-то

    -- Sf
    -- жрём до
    --  1. Пока indent > indent (Sf ..)

    CurrentChar ' ' (S (Sf{}) e) -> do
      next (S Scc (shiftI 1 $ move 1 $ e))

    CurrentChar '\t' (S (Sf{}) e) -> do
      next (S Scc (shiftI 8 $ move 1 $ e))

    CurrentChar c (S (Sf{}) e) -> do
      error "WTF?"

    -- в любом случае жрём строку, что бы это закончилось
    EndOfLine (S _ e) -> do
      next (S S0 (setI 0 $ dropLine $ e))

    w -> error (show w)

  pure mempty


