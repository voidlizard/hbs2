{-# Language UndecidableInstances #-}
module HBS2.Data.Types.Refs
  ( module HBS2.Data.Types.Refs
  , serialise
  ) where

import HBS2.Base58
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Prelude

import Codec.Serialise(serialise)
import Data.Data
import Data.Functor.Identity
import Data.String(IsString)
import GHC.Generics
import Prettyprinter

newtype HashRef = HashRef { fromHashRef :: Hash HbSync }
                  deriving newtype (Eq,Ord,IsString,Pretty)
                  deriving stock (Data,Generic,Show)


instance Pretty (AsBase58 HashRef) where
  pretty (AsBase58 x) = pretty x
  -- TODO: should be instance Pretty (AsBase58 (Hash HbSync))

instance FromStringMaybe HashRef where
  fromStringMay = fmap HashRef . fromStringMay

data HashRefObject = HashRefObject HashRef (Maybe HashRefMetadata)
  deriving stock (Data,Show,Generic)

newtype HashRefMetadata =
  HashRefMetadata HashRef
  deriving newtype (Eq,Ord,Pretty)
  deriving stock (Data,Show,Generic)

newtype HashRefPrevState = HashRefPrevState HashRef
  deriving newtype (Eq,Ord,Pretty,IsString)
  deriving stock (Data,Show,Generic)

data HashRefType =
    HashRefMerkle HashRefObject
  | HashRefBlob   HashRefObject
  deriving stock (Data,Show,Generic)

data AnnotatedHashRef =
  AnnotatedHashRef (Maybe HashRefPrevState) HashRefType
  deriving stock (Data,Show,Generic)


instance Serialise AnnotatedHashRef
instance Serialise HashRef
instance Serialise HashRefMetadata
instance Serialise HashRefObject
instance Serialise HashRefPrevState
instance Serialise HashRefType

---

data RefGenesis e = RefGenesis
  { refOwner :: !(PubKey 'Sign e)
  , refName :: !Text
  , refMeta :: !AnnMetaData
  }
  deriving stock (Generic)

instance (Serialise (PubKey 'Sign e)) => Serialise (RefGenesis e)

data RefForm
  = LinearRef

---

data family Refs e ( f :: RefForm )

newtype instance Refs e 'LinearRef
  -- List of hashes of stored RefGenesis
  = LinearRefs { unLinearRefs :: [Hash HbSync] }
  deriving stock (Generic)

instance Serialise (Refs e 'LinearRef)

---

data family MutableRef e ( f :: RefForm )

data instance MutableRef e 'LinearRef
  = LinearMutableRef
  { lrefId :: !(Hash HbSync)
  , lrefHeight :: !Int
  -- , lrefMTree :: !(MTreeAnn [Hash HbSync])
  , lrefVal :: !(Hash HbSync)
  }
  deriving stock (Generic, Show)

instance Serialise (MutableRef e 'LinearRef)

---

data SignPhase = SignaturePresent | SignatureVerified

data family Signed ( p :: SignPhase ) a

data instance Signed SignaturePresent (MutableRef e 'LinearRef)
  = LinearMutableRefSigned
  { lmrefSignature :: Signature e
  , lmrefSignedRef :: MutableRef e 'LinearRef
  }
  deriving stock (Generic)

instance Serialise (Signature e) =>
    Serialise (Signed 'SignaturePresent (MutableRef e 'LinearRef))

data instance Signed 'SignatureVerified (MutableRef e 'LinearRef)
  = LinearMutableRefSignatureVerified
  { lmrefVSignature :: Signature e
  , lmrefVSignedRef :: MutableRef e 'LinearRef
  , lmrefVSigner :: PubKey 'Sign e
  }
  deriving stock (Generic)

---

nodeLinearRefsRef :: PubKey 'Sign e -> RefGenesis e
nodeLinearRefsRef pk = RefGenesis
  { refOwner = pk
  , refName = "List of node linear refs"
  , refMeta = NoMetaData
  }
