{-# Language DuplicateRecordFields #-}
{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
module HBS2.Data.Types.Refs
  ( module HBS2.Data.Types.Refs
  , serialise
  , pattern HashLike
  ) where

import HBS2.Base58
import HBS2.Hash
import HBS2.Net.Proto.Types
import HBS2.Prelude

import Data.Config.Suckless.Syntax

import Codec.Serialise(serialise)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Data
import Data.Text qualified as Text

class RefMetaData a where
  refMetaData :: a -> [(String, String)]
  refMetaData = const mempty

newtype HashRef = HashRef { fromHashRef :: Hash HbSync }
                  deriving newtype (Eq,Ord,IsString,Pretty,Hashable,Hashed HbSync)
                  deriving stock (Data,Generic,Show)

newtype TaggedHashRef t = TaggedHashRef { fromTaggedHashRef :: HashRef }
                          deriving newtype (Eq,Ord,IsString,Pretty,Hashable,Hashed HbSync)
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

instance Pretty (AsBase58 (TaggedHashRef t)) where
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
instance Serialise (TaggedHashRef e)


type IsRefPubKey s =  ( Eq (PubKey 'Sign s)
                      , Serialise (PubKey 'Sign s)
                      , FromStringMaybe (PubKey 'Sign s)
                      , Hashable (PubKey 'Sign s)
                      , Pretty (AsBase58 (PubKey 'Sign s))
                      )

type ForSomeRefKey a = ( Hashed HbSync a )

newtype SomeRefKey a = SomeRefKey a

instance RefMetaData (SomeRefKey a)

instance Hashed HbSync (SomeRefKey a)  => Pretty (SomeRefKey a) where
  pretty a = pretty $ hashObject @HbSync a
-- instance Hashed HbSync (SomeRefKey a) => Pretty (AsBase58 (SomeRefKey a)) where
--   pretty a = pretty $ AsBase58 (hashObject @HbSync a)

-- TODO: fix-slow-hash-calculation
instance Serialise a => Hashed HbSync (SomeRefKey a) where
  hashObject (SomeRefKey s) = hashObject (serialise s)

newtype RefAlias = RefAlias { unRefAlias :: HashRef }
                   deriving stock (Eq,Ord,Show)
                   deriving newtype (Pretty,Serialise)

instance Hashed HbSync RefAlias  where
  hashObject (RefAlias h) = fromHashRef h

refAlias :: (Hashed HbSync ref, RefMetaData ref) => ref -> RefAlias2
refAlias x = RefAlias2 (Map.fromList $ refMetaData x) (HashRef $ hashObject @HbSync x)

data RefAlias2  =
  RefAlias2 { unRefAliasMeta :: Map String String
            , unRefAlias2    :: HashRef
            }
  deriving stock (Eq,Ord,Show,Generic)

instance Hashed HbSync RefAlias2  where
  hashObject (RefAlias2 _ h) = fromHashRef h

instance Serialise RefAlias2

instance Pretty RefAlias2 where
  pretty (RefAlias2 _ h) = pretty h

instance RefMetaData RefAlias2 where
  refMetaData x = Map.toList (unRefAliasMeta x)

type LoadedRef a = Either HashRef a


-- TODO: move-outta-here
pattern HashLike:: forall {c} . HashRef -> Syntax c
pattern HashLike x <- (
  \case
    LitStrVal s      -> fromStringMay @HashRef (Text.unpack s)
    SymbolVal (Id s) -> fromStringMay @HashRef (Text.unpack s)
    _                -> Nothing
      -> Just x )



