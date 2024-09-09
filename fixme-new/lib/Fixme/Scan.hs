{-# Language MultiWayIf #-}
module Fixme.Scan (scanBlob,scanMagic,updateScanMagic) where

import Fixme.Prelude hiding (indent)
import Fixme.Types

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import Data.Char (isSpace)

import Data.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Coerce

import Data.Generics.Product.Fields (field)
import Lens.Micro.Platform
import Text.InterpolatedString.Perl6 (qc)

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}


data SfEnv =
  SfEnv { lno :: Int  -- ^ line number
        , l0  :: Int  -- ^ fixme indent
        , eln :: Int  -- ^ empty lines counter
        }
        deriving stock Generic


succEln :: SfEnv -> ByteString -> SfEnv
succEln f s | LBS8.null s = over (field @"eln") succ f
            | otherwise = set (field @"eln") 0 f

data Sx = S0 | Sf SfEnv

data S = S Sx [(Int,ByteString)]


data FixmePart = FixmePart Int FixmeWhat
                 deriving stock (Show,Data,Generic)

data FixmeWhat = FixmeHead Int Int Text Text
               | FixmeLine Text
               | FixmeAttr FixmeAttrName FixmeAttrVal
                 deriving stock (Show,Data,Generic)

data P = P0 [FixmePart] | P1 Int Fixme [FixmePart]


scanMagic :: FixmePerks m => FixmeM m HashRef
scanMagic = do
  env <- ask
  w <- atomically do
         tagz <- fixmeEnvTags env          & readTVar
         co   <- fixmeEnvDefComments  env  & readTVar
         fco  <- fixmeEnvFileComments env  & readTVar
         m    <- fixmeEnvFileMask env      & readTVar
         e    <- fixmeEnvFileExclude env   & readTVar
         a    <- fixmeEnvAttribs env       & readTVar
         v    <- fixmeEnvAttribValues env  & readTVar

         pure $ serialise (tagz, co, fco, m, e, a, v)
  pure $ HashRef $ hashObject w

updateScanMagic :: (FixmePerks m) => FixmeM m ()
updateScanMagic = do
  t <- asks fixmeEnvScanMagic
  magic <- scanMagic
  atomically $ writeTVar t (Just magic)

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

  anames  <- asks fixmeEnvAttribs >>= readTVarIO <&> HS.toList

  let setters = [ ( LBS8.pack [qc|${show $ pretty n}:|], n ) | n <- anames ]

  let ls = LBS8.lines lbs & zip [0..]

  parts <- S.toList_ do

      flip fix (S S0 ls) $ \next -> \case
        S S0 ((lno,x):xs) -> do

          (l,bs)  <- eatPrefix0 Nothing comments x

          let mtag = headMay [ t | t <- tagz, LBS8.isPrefixOf t bs ]

          case mtag of
            Nothing ->
              next (S S0 xs)

            Just tag -> do
              emitFixmeStart lno l tag (LBS8.drop (LBS8.length tag) bs)
              next (S (Sf (SfEnv lno l 0)) xs)

        S sf@(Sf env@(SfEnv{..})) (x : xs) -> do

          (li,bs)  <- eatPrefix0 (Just l0) comments (snd x)

          if | eln > 1 -> next (S S0 (x:xs))

             | li <= l0 && not (LBS8.null bs) -> next (S S0 (x:xs))

             | otherwise -> do

                let stripped = LBS8.dropWhile isSpace bs
                let attr = headMay [ (s, LBS8.drop (LBS8.length a) stripped)
                                   | (a,s) <- setters, LBS8.isPrefixOf a stripped
                                   ]

                case attr of
                  Just (a,v) -> do
                    let vv = LBS8.toStrict v & decodeUtf8With ignore & Text.strip
                    emitFixmeAttr (fst x) l0 a (FixmeAttrVal vv)
                  Nothing -> do
                    emitFixmeLine (fst x) l0 bs

                next (S (Sf (succEln env bs)) xs)

        S _ [] -> pure ()
  -- debug $ vcat (fmap viaShow parts)

  S.toList_ do
    flip fix (P0 parts) $ \next -> \case

        (P0 (FixmePart l h@FixmeHead{} : rs)) -> do
          next (P1 l (fromHead h) rs)

        (P1 _ fx (FixmePart l h@FixmeHead{} : rs)) -> do
          emitFixme fx
          next (P1 l (fromHead h) rs)

        (P1 _ fx (FixmePart lno (FixmeLine what) : rs)) -> do
          next (P1 lno (setLno lno $ over (field @"fixmePlain") (<> [FixmePlainLine what]) fx) rs)

        (P1 _ fx (FixmePart lno (FixmeAttr a v) : rs)) -> do
          next (P1 lno (setLno lno $ over (field @"fixmeAttr") (<> HM.singleton a v) fx) rs)

        (P1 _ fx []) -> emitFixme fx
        (P0 ( _ : rs ) ) -> next (P0 rs)
        (P0 []) -> pure ()

  where

    setLno lno fx@Fixme{} = do
      let lno1 = Just (FixmeOffset (fromIntegral lno))
      set (field @"fixmeEnd") lno1 fx

    emitFixme e = do
      S.yield $ over (field @"fixmePlain") dropEmpty e
      where
        dropEmpty = dropWhile $ \case
          FixmePlainLine "" -> True
          _                 -> False

    -- FIXME: jopakita
    fromHead = \case
      FixmeHead lno  _ tag title ->
          Fixme (FixmeTag tag)
                (FixmeTitle title)
                mempty
                Nothing
                (Just (FixmeOffset (fromIntegral lno)))
                Nothing
                mempty
                mempty

      _ -> mempty

    emitFixmeStart lno lvl tagbs restbs = do
      let tag  = decodeUtf8With ignore  (LBS8.toStrict tagbs) & Text.strip
      let rest = decodeUtf8With ignore  (LBS8.toStrict restbs) & Text.strip
      S.yield (FixmePart lno (FixmeHead lno lvl tag rest))

    emitFixmeAttr lno _ name val = do
      S.yield (FixmePart lno (FixmeAttr name val))

    emitFixmeLine lno _ restbs = do
      let rest = decodeUtf8With ignore  (LBS8.toStrict restbs) & Text.stripEnd
      S.yield (FixmePart lno (FixmeLine rest))

    eatPrefix0 lim' comments x = do
      over _2 LBS8.pack <$> do

        flip fix (0, LBS8.unpack x) $ \next w@(k, left) -> do

          let lim = fromMaybe (succ k) lim'

          if k > lim then
            pure (k, left)
          else
            case w of
              (n, ' '  : rest) -> next (n+1, if k == lim then ' ' : rest else rest)
              (n, '\t' : rest) -> next (n+8, if k == lim then '\t' : rest else rest)

              (n, rest) -> do
                let comm = headMay [ co | co <- comments, LBS8.isPrefixOf co (LBS8.pack rest) ]
                case comm of
                  Nothing -> pure (n, rest)
                  Just co -> next (n+1, drop (fromIntegral $ LBS8.length co) rest)

