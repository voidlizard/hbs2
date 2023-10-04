{-# Language DuplicateRecordFields #-}
{-# Language UndecidableInstances #-}
module HBS2.Data.Types.Refs
  ( module HBS2.Data.Types.Refs
  , serialise
  ) where

import HBS2.Base58
import HBS2.Hash
import HBS2.Net.Proto.Types
import HBS2.Prelude

import Codec.Serialise(serialise)
import Data.Data

newtype HashRef = HashRef { fromHashRef :: Hash HbSync }
                  deriving newtype (Eq,Ord,IsString,Pretty,Hashable)
                  deriving stock (Data,Generic,Show)



instance Pretty (AsBase58 HashRef) where
  pretty (AsBase58 x) = pretty x
  -- TODO: should be instance Pretty (AsBase58 (Hash HbSync))

instance FromStringMaybe HashRef where
  fromStringMay = fmap HashRef . fromStringMay

newtype TheHashRef t = TheHashRef { fromTheHashRef :: Hash HbSync }
                      deriving newtype (Eq,Ord,IsString,Pretty,Hashable)
                      deriving stock (Data,Generic,Show)

instance Pretty (AsBase58 (TheHashRef t)) where
  pretty (AsBase58 x) = pretty x

instance FromStringMaybe (TheHashRef t) where
  fromStringMay = fmap TheHashRef . fromStringMay

data HashRefObject = HashRefObject HashRef (Maybe HashRefMetadata)
  deriving stock (Data,Show,Generic)

newtype HashRefMetadata =
  HashRefMetadata HashRef
  deriving newtype (Eq,Ord,Pretty)
  deriving stock (Data,Show,Generic)

data HashRefType =
    HashRefMerkle HashRefObject
  | HashRefBlob   HashRefObject
  deriving stock (Data,Show,Generic)

data AnnotatedHashRef =
  AnnotatedHashRef (Maybe HashRef) HashRef
  deriving stock (Data,Show,Generic)

data SequentialRef =
  SequentialRef Integer AnnotatedHashRef
  deriving stock (Data,Show,Generic)

instance Serialise AnnotatedHashRef
instance Serialise SequentialRef
instance Serialise HashRef


type IsRefPubKey s =  ( Eq (PubKey 'Sign s)
                      , Serialise (PubKey 'Sign s)
                      , FromStringMaybe (PubKey 'Sign s)
                      , Hashable (PubKey 'Sign s)
                      )

type ForSomeRefKey a = ( Hashed HbSync a )

newtype SomeRefKey a = SomeRefKey a

-- TODO: fix-slow-hash-calculation
instance Serialise a => Hashed HbSync (SomeRefKey a) where
  hashObject (SomeRefKey s) = hashObject (serialise s)

newtype RefAlias = RefAlias { unRefAlias :: HashRef }
                   deriving stock (Eq,Ord,Show)
                   deriving newtype (Pretty,Serialise)

instance Hashed HbSync RefAlias  where
  hashObject (RefAlias h) = fromHashRef h


refAlias :: Hashed HbSync ref => ref -> RefAlias
refAlias x = RefAlias (HashRef $ hashObject @HbSync x)


