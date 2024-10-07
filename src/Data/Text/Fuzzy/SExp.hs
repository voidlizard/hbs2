{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Text.Fuzzy.SExp where

import Data.Text (Text)

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.Text.Fuzzy.Tokenize
import Control.Monad.Reader
import Data.Typeable
import Control.Monad.Except
import Control.Monad.RWS
import Data.Maybe
import Data.Char (isSpace,digitToInt)
import Data.Generics.Uniplate.Data()
import Safe
import Data.Data
import GHC.Generics
import Lens.Micro.Platform
import Data.Text qualified as Text
import Data.Coerce
import Data.Scientific

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict  qualified as HM

import Prettyprinter hiding (braces,list)


import Streaming.Prelude qualified as S

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

newtype C0 = C0 (Maybe Int)
             deriving stock (Eq,Ord,Show,Data,Typeable,Generic)

data SExpParseError =
    ParensOver  C0
  | ParensUnder C0
  | ParensUnmatched C0
  | SyntaxError C0
  deriving stock (Show,Typeable)


data NumType =
    NumInteger Integer
  | NumDouble  Scientific
  deriving stock (Eq,Ord,Show,Data,Generic)

class Monoid c => ForMicroSexp c where

instance Monoid C0 where
  mempty = C0 Nothing

instance Semigroup C0 where
  (<>) (C0 a) (C0 b) = C0 (b <|> a)

instance ForMicroSexp C0 where


instance ForMicroSexp () where

data MicroSexp c =
    List_    c [MicroSexp c]
  | Symbol_  c Text
  | String_  c Text
  | Number_  c NumType
  | Boolean_ c Bool
  deriving stock (Show,Data,Generic)

pattern List :: ForMicroSexp c => [MicroSexp c] -> MicroSexp c
pattern List xs <- List_ _ xs where
  List xs = List_ mempty xs

pattern Symbol :: ForMicroSexp c => Text -> MicroSexp c
pattern Symbol xs <- Symbol_ _ xs where
  Symbol xs = Symbol_ mempty xs

pattern String :: ForMicroSexp c => Text -> MicroSexp c
pattern String x <- String_ _ x where
  String x = String_ mempty x

pattern Number :: ForMicroSexp c => NumType -> MicroSexp c
pattern Number n <- Number_ _ n where
  Number n = Number_ mempty n

pattern Boolean :: ForMicroSexp c => Bool -> MicroSexp c
pattern Boolean b <- Boolean_ _ b where
  Boolean b = Boolean_ mempty b

{-# COMPLETE List, Symbol, String, Number, Boolean #-}


contextOf :: Lens (MicroSexp c) (MicroSexp c) c c
contextOf = lens g s
  where
    s sexp c = case sexp of
      List_    _  a -> List_ c a
      Symbol_  _  a -> Symbol_ c a
      String_  _  a -> String_ c a
      Number_  _  a -> Number_ c a
      Boolean_ _  a -> Boolean_ c a

    g = \case
      List_ c    _ -> c
      Symbol_ c  _ -> c
      String_ c  _ -> c
      Number_ c  _ -> c
      Boolean_ c _ -> c

nil :: forall c . ForMicroSexp c => MicroSexp c
nil = List []

symbol :: forall c . ForMicroSexp c => Text -> MicroSexp c
symbol = Symbol

str :: forall c . ForMicroSexp c => Text -> MicroSexp c
str = String

newtype SExpEnv =
  SExpEnv
  { sexpTranslate :: Bool
  }

data SExpState =
  SExpState
  { _sexpLno    :: Int
  , _sexpBraces :: [Char]
  }

makeLenses 'SExpState

defEnv :: SExpEnv
defEnv = SExpEnv True

newtype SExpM m a = SExpM { fromSexpM :: RWST SExpEnv () SExpState m a }
                    deriving newtype
                      ( Applicative
                      , Functor
                      , Monad
                      , MonadState SExpState
                      , MonadReader SExpEnv
                      , MonadTrans
                      )


instance MonadError SExpParseError m => MonadError SExpParseError (SExpM m) where
  throwError = lift . throwError
  catchError w  = catchError (coerce $ fromSexpM w)

tokenizeSexp :: Text -> [TTok]
tokenizeSexp txt =  do
  let spec = delims " \r\t" <> comment ";"
                            <> punct "'{}()[]\n"
                            <> sqq
                            <> uw
  tokenize spec txt

runSexpM :: Monad m => SExpM m a -> m a
runSexpM f = evalRWST (fromSexpM f) defEnv (SExpState 0 []) <&> fst


parseSexp :: (ForMicroSexp c, MonadError SExpParseError m) => Text -> m (MicroSexp c)
parseSexp txt = do
  (s, _) <- runSexpM do
             (s,rest) <- sexp (tokenizeSexp txt)
             checkBraces
             pure (s,rest)

  pure s

checkBraces :: (MonadError SExpParseError m) => SExpM m ()
checkBraces = do
  braces <- gets (view sexpBraces)
  unless (null braces) $ raiseWith ParensUnder

succLno :: (MonadError SExpParseError m) => SExpM m ()
succLno = modify (over sexpLno succ)

parseTop :: (ForMicroSexp c, MonadError SExpParseError m) => Text -> m [MicroSexp c]
parseTop txt = do
  let tokens = tokenizeSexp txt
  S.toList_ $ runSexpM do
    flip fix (mempty,tokens) $ \next -> \case
      (acc, []) -> do
        emit acc
      (acc, TPunct '\n' : rest) -> do
        succLno
        emit acc
        next (mempty,rest)
      (acc, rest) -> do
        (s, xs) <- sexp rest
        next (acc <> [s],xs)

  where

    emit [] = pure ()
    emit wtf = case wtf of
      [List one] -> lift $ S.yield (List one)
      xs    -> lift $ S.yield (List xs)

sexp :: (ForMicroSexp c, MonadError SExpParseError m) => [TTok] -> SExpM m (MicroSexp c, [TTok])
sexp s = case s of
  [] -> do
    checkBraces
    pure (nil, mempty)

  (TText l : w) -> (,w) <$> trNum (Symbol l)

  (TStrLit l : w) -> pure (String l, w)

  -- so far ignored
  (TPunct '\'' : rest) -> sexp rest

  (TPunct '\n' : rest) -> succLno >> sexp rest

  (TPunct c : rest) | isSpace c  -> sexp rest

  (TPunct c : rest) | isBrace c  ->
    maybe (pure (nil, rest)) (`list` rest) (closing c)
                    | otherwise -> do
                        raiseWith ParensOver

  ( _ : _ ) -> raiseWith SyntaxError

  where

    setContext w = do
      co <- getC0
      pure $ over _2 (set contextOf co) w

    isBrace :: Char -> Bool
    isBrace c = HM.member c braces

    closing :: Char -> Maybe Char
    closing c = HM.lookup c braces

    braces :: HashMap Char Char
    braces = HM.fromList[ ('{', '}')
                        , ('(', ')')
                        , ('[', ']')
                        , ('<', '>')
                        ]

    cBraces :: [Char]
    cBraces = HM.elems braces

    trNum tok = do

      trans <- asks sexpTranslate

      case tok of
        Symbol s | trans -> do
          let s0 = Text.unpack s

          let what = Number . NumInteger <$> readMay @Integer s0
                    <|>
                    Number . NumInteger <$> parseBinary s0
                    <|>
                    Number . NumDouble <$> readMay @Scientific s0
                    <|>
                    ( case  s of
                        "#t" -> Just (Boolean True)
                        "#f" -> Just (Boolean False)
                        _    -> Nothing
                    )

          pure $ fromMaybe (Symbol s) what


        x        -> pure x
    {-# INLINE trNum #-}

    list :: (ForMicroSexp c, MonadError SExpParseError m)
         => Char
         -> [TTok]
         -> SExpM m (MicroSexp c, [TTok])

    list _ [] = raiseWith ParensUnder

    list cb tokens = do
      modify $ over sexpBraces (cb:)

      go cb mempty tokens

      where

        isClosingFor :: Char -> Bool
        isClosingFor c = c `elem` cBraces

        go _ _ [] = do
          checkBraces
          pure (List mempty, mempty)

        go cl acc (TPunct c : rest) | isSpace c = do
          go cl acc rest

        go cl acc (TPunct c : rest)
          | isClosingFor c && c == cl = do
              modify $ over sexpBraces (drop 1)
              pure (List (reverse acc), rest)

          | isClosingFor c && c /= cl = do
              raiseWith ParensUnmatched
              -- throwError =<< ParensUnmatched <$> undefined

        go cl acc rest = do
          (e,r) <- sexp rest
          go cl (e : acc) r


getC0 :: Monad m => SExpM m C0
getC0 = do
  lno <- gets (view sexpLno)
  pure (C0 (Just lno))

raiseWith :: (MonadError SExpParseError m)
          => (C0 -> SExpParseError) -> SExpM m b

raiseWith a = throwError =<< a <$> getC0

instance Pretty NumType where
   pretty = \case
    NumInteger n -> pretty n
    NumDouble  n -> viaShow n

instance ForMicroSexp c => Pretty (MicroSexp c) where

  pretty = \case
    List xs   -> parens (hsep (fmap pretty xs))
    String s  -> dquotes (pretty s)
    Symbol s  -> pretty s
    Number n  -> pretty n
    Boolean True -> pretty  "#t"
    Boolean False -> pretty  "#f"

isBinaryDigit :: Char -> Bool
isBinaryDigit c = c == '0' || c == '1'

parseBinary :: String -> Maybe Integer
parseBinary str =
  let
      withoutPrefix = case str of
                        '0':'b':rest -> Just rest
                        '0':'B':rest -> Just rest
                        _            -> Nothing
  in if isJust withoutPrefix && all isBinaryDigit (fromJust withoutPrefix)
     then Just $ foldl (\acc x -> acc * 2 + toInteger (digitToInt x)) 0 (fromJust withoutPrefix)
     else Nothing

