module Data.Config.Suckless.Parse.Fuzzy
  ( ParseSExp(..)
  ) where

import Data.Config.Suckless.Syntax
import Data.Text.Fuzzy.SExp qualified as P
import Data.Text.Fuzzy.SExp (C0(..),SExpParseError,ForMicroSexp(..))

import Data.Functor
import Data.Text as Text
import Control.Monad.Except
import Control.Monad.Identity

class ParseSExp what where
  parseTop :: what -> Either SExpParseError [Syntax C]
  parseSyntax :: what -> Either SExpParseError (Syntax C)

instance ParseSExp Text where
  parseTop what = runIdentity (runExceptT (P.parseTop  what)) <&> fmap toSyntax
  parseSyntax txt = runIdentity (runExceptT (P.parseSexp txt)) <&> toSyntax

instance ParseSExp String where
  parseTop what = runIdentity (runExceptT (P.parseTop  (Text.pack what))) <&> fmap toSyntax
  parseSyntax txt = runIdentity (runExceptT (P.parseSexp (Text.pack txt))) <&> toSyntax

toSyntax :: P.MicroSexp C0 -> Syntax C
toSyntax = \case
 P.List_    co  a -> List (toContext co) (fmap toSyntax a)
 P.Symbol_  co  a -> Symbol (toContext co) (Id a)
 P.String_  co  a -> Literal (toContext co) (LitStr a)
 P.Boolean_ co  a -> Literal (toContext co) (LitBool a)
 P.Number_  co  v -> case v of
  P.NumInteger n  -> Literal (toContext co) (LitInt n)
  P.NumDouble n   -> Literal (toContext co) (LitScientific (realToFrac n))

toContext :: C0 -> Context C
toContext (C0 what) = SimpleContext (fromIntegral <$> what)



