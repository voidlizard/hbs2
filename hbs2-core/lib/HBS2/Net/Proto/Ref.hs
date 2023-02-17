{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.Ref where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef)
import HBS2.Net.Auth.Credentials


import Data.Config.Suckless

import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString (ByteString)
import Lens.Micro.Platform
import Codec.Serialise
import Data.Either
-- import Data.Set (Set)

-- TODO: tagged-hash-ref
--   Сделать newtype HashRef t = HashRef
--   тегировать типом того, на что он указывает

data Ref e = Ref
  { _refNonce     :: ByteString
  , _refACB       :: HashRef
  , _refData      :: HashRef
  }
  deriving stock (Generic)

makeLenses 'Ref


type IsRef e = (Eq (Signature e), Serialise (Signature e))

deriving stock instance IsRef e => Eq (Ref e)

instance IsRef e => Serialise (Ref e)

data DefineRef e  = DefineRef Text (Ref e)

instance FromStringMaybe (Ref e) where
  fromStringMay s = Nothing

    where
      parsed = parseTop s & fromRight mempty
      defRe  = headMay [ re | (ListVal (Key "define-ref" [SymbolVal re]) ) <- parsed ]

      nonce  = lastMay [ n
                       | (ListVal (Key "ref-nonce" [SymbolVal x, LitStrVal n]) ) <- parsed
                       , Just x == defRe
                       ]

      acbDefs = ""

      -- root = lastMay $ catMaybes $
      --                [ fromStringMay (Text.unpack e)
      --                | (ListVal (Key "acb-root" [SymbolVal a, LitStrVal e]) ) <- parsed
      --                , Just a == defAcb
      --                ]

      -- owners = L.nub $ catMaybes
      --                [ fromStringMay (Text.unpack e)
      --                | (ListVal (Key "acb-owner" [SymbolVal a, LitStrVal e]) ) <- parsed
      --                , Just a == defAcb
      --                ]

      -- readers = L.nub $ catMaybes
      --                [ fromStringMay (Text.unpack e)
      --                | (ListVal (Key "acb-reader" [SymbolVal a, LitStrVal e]) ) <- parsed
      --                , Just a == defAcb
      --                ]

      -- writers = L.nub $ catMaybes
      --                [ fromStringMay (Text.unpack e)
      --                | (ListVal (Key "acb-writer" [SymbolVal a, LitStrVal e]) ) <- parsed
      --                , Just a == defAcb
      --                ]

      -- prev =lastMay $ catMaybes $
      --                [ fromStringMay (Text.unpack e)
      --                | (ListVal (Key "acb-prev" [SymbolVal a, LitStrVal e]) ) <- parsed
      --                , Just a == defAcb
      --                ]

