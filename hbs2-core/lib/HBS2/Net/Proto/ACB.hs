{-# Language TemplateHaskell #-}
{-# Language PatternSynonyms #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.ACB where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.Base58
import HBS2.Data.Types

import Data.Config.Suckless

import Control.Applicative
import Lens.Micro.Platform
import Codec.Serialise()
import Data.List qualified as L
import Data.Text qualified as Text
import Data.Maybe
import Data.Either

data family ACB s

data DefineACB s  = DefineACB Text (ACB s)

type ACBSimple s = ACB s

data instance ACB s =
  ACB1
  { _acbRoot     :: !(Maybe (PubKey  'Sign s)) -- it's monoid. no choice but Maybe
  , _acbOwners   :: ![PubKey 'Sign s]
  , _acbReaders  :: ![PubKey 'Encrypt s]
  , _acbWriters  :: ![PubKey 'Sign s]
  , _acbPrev     :: !(Maybe HashRef)
  }
  deriving stock (Generic)


makeLenses 'ACB1

type ForACB e = ( Serialise (PubKey 'Sign e)
                , Serialise (PubKey 'Encrypt e)
                , Eq (PubKey 'Sign e)
                , Eq (PubKey 'Encrypt e)
                , FromStringMaybe (PubKey 'Sign e)
                , FromStringMaybe (PubKey 'Encrypt e)
                )

deriving instance ForACB e => Eq (ACBSimple e)

instance ForACB e => Serialise (ACBSimple e)

instance ForACB e => Monoid (ACBSimple e) where
  mempty = ACB1 Nothing mempty mempty mempty Nothing

instance ForACB e => Semigroup (ACBSimple e) where
  (<>) a b = ACB1 (view acbRoot a <|> view acbRoot b)
                  (L.nub (view acbOwners a <> view acbOwners b))
                  (L.nub (view acbReaders a <> view acbReaders b))
                  (L.nub (view acbWriters a <> view acbWriters b))
                  (view acbPrev a <|> view acbPrev b)


instance ( Pretty (AsBase58 (PubKey 'Sign s))
         , Pretty (AsBase58 (PubKey 'Encrypt s) )
         ) => Pretty (AsSyntax (DefineACB s)) where
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
      owners  = vcat $ fmap owner (view acbOwners acb)
      acbR    = "acb-root" <+> nacb
      readers = vcat $ fmap reader (view acbReaders acb)
      writers = vcat $ fmap writer (view acbWriters acb)

      owner  = (wacb "acb-owner"  <+>)  . dquotes . pretty . AsBase58
      reader = (wacb "acb-reader"  <+>) . dquotes . pretty . AsBase58
      writer = (wacb "acb-writer"  <+>) . dquotes . pretty . AsBase58


pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

instance ForACB s => FromStringMaybe (ACB s) where
  fromStringMay s = Just $ ACB1 root owners readers writers prev

    where
      parsed = parseTop s & fromRight mempty
      defAcb = headMay [ acb | (ListVal (Key "define-acb" [SymbolVal acb]) ) <- parsed ]

      root = lastMay $ catMaybes $
                     [ fromStringMay (Text.unpack e)
                     | (ListVal (Key "acb-root" [SymbolVal a, LitStrVal e]) ) <- parsed
                     , Just a == defAcb
                     ]

      owners = L.nub $ catMaybes
                     [ fromStringMay (Text.unpack e)
                     | (ListVal (Key "acb-owner" [SymbolVal a, LitStrVal e]) ) <- parsed
                     , Just a == defAcb
                     ]

      readers = L.nub $ catMaybes
                     [ fromStringMay (Text.unpack e)
                     | (ListVal (Key "acb-reader" [SymbolVal a, LitStrVal e]) ) <- parsed
                     , Just a == defAcb
                     ]

      writers = L.nub $ catMaybes
                     [ fromStringMay (Text.unpack e)
                     | (ListVal (Key "acb-writer" [SymbolVal a, LitStrVal e]) ) <- parsed
                     , Just a == defAcb
                     ]

      prev =lastMay $ catMaybes $
                     [ fromStringMay (Text.unpack e)
                     | (ListVal (Key "acb-prev" [SymbolVal a, LitStrVal e]) ) <- parsed
                     , Just a == defAcb
                     ]


