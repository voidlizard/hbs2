{-# Language UndecidableInstances #-}
module HBS2Git.Encryption.KeyInfo where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Net.Auth.Credentials

import HBS2.Net.Proto.Types hiding (Cookie)

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.KeyValue

import Codec.Serialise
import Data.HashSet
import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Maybe


data KeyInfo =
  KeyInfo
  { keyInfoNonce   :: Integer
  , keyInfoRef     :: PubKey 'Sign HBS2Basic
  , keyInfoOwner   :: PubKey 'Encrypt HBS2Basic
  , keyInfoMembers :: HashSet (PubKey 'Encrypt HBS2Basic)
  }
  deriving (Eq,Ord,Show,Generic)

type ForKeys s = (Serialise (PubKey 'Sign s), Serialise (PubKey 'Encrypt s))

instance ForKeys HBS2Basic => Serialise KeyInfo

instance ForKeys HBS2Basic => Hashed HbSync KeyInfo where
  hashObject ki = hashObject (serialise ki)


keyInfoFrom :: POSIXTime -> Syntax C -> Maybe KeyInfo
keyInfoFrom t (ListVal (SymbolVal "encrypted" : (LitStrVal r) : args)) =
  KeyInfo <$> nonce
          <*> ref
          <*> owner
          <*> members

  where
    nonce = Just $ maybe 0 (round t `div`) ttl
    ref = fromStringMay  (Text.unpack r)
    ttl = Just $ lastDef 86400 [ x | ListVal (Key "ttl" [LitIntVal x]) <- args ]
    owner = fromStringMay =<< lastMay [ Text.unpack o | ListVal (Key "owner" [LitStrVal o]) <- args ]
    members = Just $ HashSet.fromList
                   $ catMaybes
                   [ fromStringMay (Text.unpack o) | ListVal (Key "member" [LitStrVal o]) <- args ]

    -- keypath = lastMay [ Text.unpack p | ListVal @C (Key "keyring" [LitStrVal p]) <- args ]

keyInfoFrom _ _ = Nothing
