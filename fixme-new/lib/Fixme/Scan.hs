{-# Language MultiWayIf #-}
module Fixme.Scan (scanBlob) where

import Fixme.Prelude hiding (indent)
import Fixme.Types

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)

import Data.Maybe
import Data.HashSet qualified as HS
import Data.Coerce

import GHC.Generics (Generic)
import Data.Generics.Product.Fields (field)
import Lens.Micro.Platform

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}


data SfEnv =
  SfEnv { lno :: Int
        , l0  :: Int
        , eln :: Int
        }

sfEnv0 = SfEnv 0 0 0

data Sx = S0 | Sf SfEnv

data S = S Sx [(Int,ByteString)]


data FixmePart = FixmePart Int FixmeWhat
                 deriving stock (Show,Data,Generic)

data FixmeWhat = FixmeHead Int Text Text
               | FixmeLine Text
                 deriving stock (Show,Data,Generic)

data P = P0 [FixmePart] | P1 Fixme [FixmePart]

scanBlob :: forall m . FixmePerks m
         => Maybe FilePath   -- ^ filename to detect type
         -> ByteString       -- ^ content
         -> FixmeM m [Fixme]

scanBlob fpath lbs = do

  tagz <- asks fixmeEnvTags
              >>= readTVarIO
              <&> HS.toList
              <&> fmap (Text.unpack . coerce)
              <&> filter (not . null)
              <&> fmap LBS8.pack

  comments <- fixmeGetCommentsFor fpath
                <&> filter (not . LBS8.null) . fmap (LBS8.pack . Text.unpack)

  let ls = LBS8.lines lbs & zip [0..]

  parts <- S.toList_ do

      flip fix (S S0 ls) $ \next -> \case
        S S0 ((lno,x):xs) -> do

          (l,bs)  <- eatPrefix comments x

          let mtag = headMay [ t | t <- tagz, LBS8.isPrefixOf t bs ]

          case mtag of
            Nothing ->
              next (S S0 xs)

            Just tag -> do
              emitFixmeStart lno l tag (LBS8.drop (LBS8.length tag) bs)
              next (S (Sf (SfEnv lno l 0)) xs)

        S sf@(Sf (SfEnv{..})) (x : xs) -> do

          (li,bs)  <- eatPrefix comments (snd x)

          if li <= l0 && not (LBS8.null bs) then do
            next (S S0 (x:xs))
          else do
            emitFixmeLine lno l0 bs -- (snd x)
            next (S sf xs)

        S _ [] -> pure ()

  debug $ vcat (fmap viaShow parts)

  S.toList_ do
    flip fix (P0 parts) $ \next -> \case

        (P0 (FixmePart _ h@FixmeHead{} : rs)) -> do
          next (P1 (fromHead h) rs)

        (P1 fx (FixmePart _ h@FixmeHead{} : rs)) -> do
          emitFixme fx
          next (P1 (fromHead h) rs)

        (P1 fx (FixmePart _ (FixmeLine what) : rs)) -> do
          next (P1 (over (field @"fixmePlain") (<> [FixmePlainLine what]) fx) rs)

        (P1 fx []) -> emitFixme fx
        (P0 ( _ : rs ) ) -> next (P0 rs)
        (P0 []) -> pure ()

  where

    emitFixme e = do
      S.yield $ over (field @"fixmePlain") dropEmpty e
      where
        dropEmpty = dropWhile $ \case
          FixmePlainLine "" -> True
          _                 -> False

    -- FIXME: jopakita
    fromHead = \case
      FixmeHead _ tag title -> Fixme (FixmeTag tag) (FixmeTitle title) Nothing mempty mempty Nothing
      _ -> Fixme mempty mempty Nothing mempty mempty Nothing

    emitFixmeStart lno lvl tagbs restbs = do
      debug $ red "emitFixmeStart"
      let tag  = decodeUtf8With ignore  (LBS8.toStrict tagbs) & Text.strip
      let rest = decodeUtf8With ignore  (LBS8.toStrict restbs) & Text.strip
      S.yield (FixmePart lno (FixmeHead lvl tag rest))

    emitFixmeLine lno l0 restbs = do
      debug $ red "emitFixmeLine"
      let rest = decodeUtf8With ignore  (LBS8.toStrict restbs) & Text.stripEnd
      S.yield (FixmePart lno (FixmeLine rest))

    eatPrefix comments x = do
      -- дропаем пробелы или табы
      let (pre1,s1) = LBS8.span (`elem` " \t") x

      -- дропаем токен коммента
      -- перебираем все коменты, пока не найдем первый
      let comm = headMay [ co | co <- comments, LBS8.isPrefixOf co s1 ]

      let pre2  = pre1 <> fromMaybe mempty comm
      let rest2 = LBS8.drop (maybe 0 LBS8.length comm) s1

      let (pre3,s2) = LBS8.span (`elem` " \t") rest2

      let pre = pre1 <> pre2 <> pre3

      l <- for (LBS8.unpack pre) $ \case
              ' '  -> pure 1
              '\t' -> pure 8
              _    -> pure 0

      let level = sum l + maybe 0 (const 1) comm

      pure (level, s2)

