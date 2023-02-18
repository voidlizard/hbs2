{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.Ref where

import HBS2.Prelude.Plated
import HBS2.Data.Types.HashRef
import HBS2.Net.Proto.ACB
import HBS2.Net.Auth.Credentials
import HBS2.Hash
import HBS2.Data.Types

import Data.Config.Suckless

import Control.Applicative
import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B8
import Lens.Micro.Platform
import Codec.Serialise
import Data.Either
import Data.Map qualified as Map
import Data.Maybe
import Data.List qualified as L
import Prettyprinter
import Data.Set (Set)
import Data.Set qualified as Set

-- TODO: tagged-hash-ref
--   Сделать newtype HashRef t = HashRef
--   тегировать типом того, на что он указывает

newtype AttrId = AttrId Text
                 deriving stock (Eq,Ord,Generic)
                 deriving newtype (IsString)

-- Иммутабельный seed ссылки
data RefSeed e = Ref
  { _refACB       :: ACBSimple e
  , _refAttr      :: Set AttrId
  }
  deriving stock (Generic)

makeLenses 'Ref


type IsRef e = (Eq (Signature e), Serialise (Signature e))

deriving stock instance IsRef e => Eq (RefSeed e)

instance Serialise AttrId
instance IsRef e => Serialise (RefSeed e)

data DefineRef e  = DefineRef Text Text (RefSeed e)

instance (
         ) => Pretty (AsSyntax (DefineRef e)) where
  pretty (AsSyntax (DefineRef n a ref)) = vcat [
           "define-ref"   <+> pretty n <+> pretty a <> line
         , vcat refAttrs
    ] <> line <> line
      <> pretty (AsSyntax (DefineACB a (view refACB ref)))

    where
      refAttrs = prettyAttr <$> Set.toList (view refAttr ref)
      prettyAttr (AttrId s) = "define-ref-attr" <+> pretty n <+> pretty s

instance FromStringMaybe (RefSeed e) where
  fromStringMay s  = case defRe of
    Nothing -> Nothing
    Just (n,a) -> Ref <$> Map.lookup a acbDefs
                  <*> attrs n

    where
      parsed = parseTop s & fromRight mempty
      defRe  = headMay [ (re,a)
                       | (ListVal (Key "define-ref" [SymbolVal re, SymbolVal a]) ) <- parsed
                       ]

      allAttrs = Map.unionsWith (<>) [ mkAttr (ins,g,v)
                                     | (ListVal (Key ins [SymbolVal g, SymbolVal v]) ) <- parsed
                                     ]

      attrs g = Map.lookup g allAttrs

      mkAttr = \case
        ("define-ref-attr", g, Id v) -> Map.singleton g (Set.singleton (AttrId v))
        _                            -> mempty

      acbDefs = fromStringMay @[(Id, ACBSimple e)] s
                  & maybeToList
                  & mconcat
                  & Map.fromListWith (<>)


