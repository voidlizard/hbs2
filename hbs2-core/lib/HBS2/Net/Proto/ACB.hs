{-# Language TemplateHaskell #-}
{-# Language PatternSynonyms #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.ACB where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.HashRef
import HBS2.Base58
import HBS2.Data.Types
import HBS2.Net.Proto.Definition
import HBS2.Net.Auth.AccessKey

import Data.Config.Suckless

import Control.Applicative
import Lens.Micro.Platform
import Codec.Serialise()
import Prettyprinter
import Data.List qualified as L
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Maybe
import Data.Either
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map qualified as Map


data family ACB ( s :: EncryptionSchema ) e

data DefineACB s e  = DefineACB Text (ACB s e)

type ACBSimple = ACB 'NaClAsymm

data instance ACB 'NaClAsymm e =
  ACB1
  { _acbRoot     :: !(Maybe (PubKey  'Sign e)) -- it's monoid. no choice but Maybe
  , _acbOwners   :: !(Set (PubKey 'Sign e))
  , _acbReaders  :: !(Set (PubKey 'Encrypt e))
  , _acbWriters  :: !(Set (PubKey 'Sign e))
  , _acbPrev     :: !(Maybe HashRef)
  }
  deriving stock (Generic)


makeLenses 'ACB1

type IsACB e = ( Serialise (PubKey 'Sign e)
               , Serialise (PubKey 'Encrypt e)
               , Eq (PubKey 'Sign e)
               , Eq (PubKey 'Encrypt e)
               )

deriving instance IsACB e => Eq (ACBSimple e)

instance IsACB e => Serialise (ACBSimple e)

instance IsACB e => Monoid (ACBSimple e) where
  mempty = ACB1 Nothing mempty mempty mempty Nothing

instance IsACB e => Semigroup (ACBSimple e) where
  (<>) a b = ACB1 (view acbRoot a <|> view acbRoot b)
                  (view acbOwners a <> view acbOwners b)
                  (view acbReaders a <> view acbReaders b)
                  (view acbWriters a <> view acbWriters b)
                  (view acbPrev a <|> view acbPrev b)


instance ( Pretty (AsBase58 (PubKey 'Sign e))
         , Pretty (AsBase58 (PubKey 'Encrypt e) )
         ) => Pretty (AsSyntax (DefineACB 'NaClAsymm e)) where
  pretty (AsSyntax (DefineACB nacb' acb)) = vcat [
         "define-acb"  <+> nacb
      ,  prev
      ,  root
      ,  owners
      ,  readers
      ,  writers
      ,  line
    ]

    where

      nacb = pretty nacb'

      wacb = (<+> nacb)

      prev = maybe mempty (dquotes . pretty . AsBase58) (view acbPrev acb)

      root    = maybe mempty ( (acbR <+>) . dquotes . pretty . AsBase58 ) (view acbRoot acb)
      owners  = vcat $ fmap owner (Set.toList $ view acbOwners acb)
      acbR    = "acb-root" <+> nacb
      readers = vcat $ fmap reader (Set.toList $ view acbReaders acb)
      writers = vcat $ fmap writer (Set.toList $ view acbWriters acb)

      owner  = (wacb "acb-owner"  <+>)  . dquotes . pretty . AsBase58
      reader = (wacb "acb-reader"  <+>) . dquotes . pretty . AsBase58
      writer = (wacb "acb-writer"  <+>) . dquotes . pretty . AsBase58



instance FromStringMaybe [(Id, ACB 'NaClAsymm e)] where
  fromStringMay s = Just $ Map.toList acbs
    where
      parsed = parseTop s & fromRight mempty
      acbNames = [ (acb, mempty) | (ListVal (Key "define-acb" [SymbolVal acb]) ) <- parsed ]
      acbIns =  [ (ins, a, Text.unpack e) | (ListVal (Key ins [SymbolVal a, LitStrVal e]) ) <- parsed ]

      acbs = Map.fromListWith (<>) $ acbNames <> foldMap mkAcb acbIns

      mkAcb =  \case
        ("acb-root", n, e)   -> mkv n acbRoot (fromStringMay e)
        ("acb-owner", n, e)  -> mkv n acbOwners (Set.fromList $ maybeToList (fromStringMay e))
        ("acb-reader", n, e) -> mkv n acbReaders (Set.fromList $ maybeToList (fromStringMay e))
        ("acb-writer", n, e) -> mkv n acbWriters (Set.fromList $ maybeToList (fromStringMay e))
        ("acb-prev", n, e)   -> mkv n acbPrev (fromStringMay e)
        _ -> mempty

      mkv n l v = L.singleton $ (n,) $ mempty & set l v


instance FromStringMaybe (ACB 'NaClAsymm e) where
  fromStringMay s = fmap snd . headMay  $ mconcat
                                        $ maybeToList
                                        $ fromStringMay @[(Id, ACB 'NaClAsymm e)] s

