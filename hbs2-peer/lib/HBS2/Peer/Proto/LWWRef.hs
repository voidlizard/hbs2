{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.Proto.LWWRef where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Hash
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Schema()

import Data.Hashable hiding (Hashed)
import Data.Maybe
import Data.Word

type Epoch = Word64

data LWWRefProtoReq e =
    LWWProtoGet (LWWRefKey (Encryption e))
  | LWWProtoSet (SignedBox (LWWRef e) e)
  deriving stock Generic


data LWWRefProto e =
  LWWRefProto1 (LWWRefProtoReq e)
  deriving stock (Generic)

data LWWRef e =
  LWWRef
  { lwwEpoch :: Epoch
  , lwwProof :: Maybe HashRef
  , lwwValue :: HashRef
  }
  deriving stock (Generic)


type ForLWWRefProto e = (ForSignedBox e, Serialise (LWWRefKey (Encryption e)))

instance ForLWWRefProto e => Serialise (LWWRefProtoReq e)
instance ForLWWRefProto e => Serialise (LWWRefProto e)
instance ForLWWRefProto e => Serialise (LWWRef e)

newtype LWWRefKey s =
  LWWRefKey
  { lwwRefKey :: PubKey 'Sign s
  }
  deriving stock (Generic)


deriving stock instance IsRefPubKey s => Eq (LWWRefKey s)

instance IsRefPubKey e => Serialise (LWWRefKey e)

instance IsRefPubKey s => Hashable (LWWRefKey s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance IsRefPubKey s => Hashed HbSync (LWWRefKey s) where
  hashObject (LWWRefKey pk) = hashObject ("lwwrefkey|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (LWWRefKey s) where
  fromStringMay s = LWWRefKey <$> fromStringMay s

instance IsRefPubKey s =>  IsString (LWWRefKey s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (AsBase58 (LWWRefKey s)) where
  pretty (AsBase58 (LWWRefKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (LWWRefKey s) where
  pretty (LWWRefKey k) = pretty (AsBase58 k)

