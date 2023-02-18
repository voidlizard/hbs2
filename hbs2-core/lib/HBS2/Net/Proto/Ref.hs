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

data DefineRef e  = DefineRef Text (RefSeed e)

instance (
         ) => Pretty (AsSyntax (DefineRef e)) where
  pretty (AsSyntax (DefineRef n ref)) = vcat [
         "define-ref"   <+> pretty n
    ]
    -- where
--       rd = maybe mempty ( (("ref-data" <+> pretty n) <+>) . dquotes . pretty  ) (view refData ref)

-- instance FromStringMaybe (Ref e) where
--   fromStringMay s  = case defRe of
--     Nothing -> Nothing
--     Just n -> Ref <$> Map.lookup n refNon
--                   <*> (Map.lookup n refAcbR <|> Map.lookup n refAcbH)
--                   <*> pure (Map.lookup n refDat)

--     where
--       parsed = parseTop s & fromRight mempty
--       defRe  = headMay [ re | (ListVal (Key "define-ref" [SymbolVal re]) ) <- parsed ]

--       insn = [ (i,k,Text.unpack v) | (ListVal (Key i [SymbolVal k, LitStrVal v]) ) <- parsed ]
--       insn1 = [ (i,k,v) | (ListVal (Key i [SymbolVal k, SymbolVal v]) ) <- parsed ]

--       refDat  = Map.fromList $ foldMap mkRefData insn
--       refNon  = Map.fromList $ foldMap mkRefNonce insn
--       refAcbH  = Map.fromList $ foldMap mkAbcId insn1
--       refAcbR  = Map.fromList $ foldMap mkAbcRef insn

--       mkRefData = \case
--         ("ref-data",  n, v) -> maybeToList $ (n,) <$> fromStringMay @HashRef v
--         _                   -> mempty

--       mkRefNonce = \case
--         ("ref-nonce", n, v) -> L.singleton (n,fromString v)
--         _ -> mempty

--       mkAbcRef = \case
--         ("ref-acb-hash", n, v) -> maybeToList $ (n,) <$> fromStringMay @HashRef v
--         _ -> mempty

--       mkAbcId = \case
--         ("ref-acb-id", n, v) -> maybeToList $ (n,) <$> Map.lookup v acbDefs
--         _ -> mempty

--       acbDefs = fromStringMay @[(Id, ACBSimple e)] s
--                   & maybeToList
--                   & mconcat
--                   & Map.fromListWith (<>)
--                   & Map.map (HashRef . hashObject @HbSync . serialise)


