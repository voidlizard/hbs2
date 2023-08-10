{-# Language DuplicateRecordFields #-}
{-# Language UndecidableInstances #-}
module HBS2.Data.Types.Refs
  ( module HBS2.Data.Types.Refs
  , serialise
  ) where

import HBS2.Base58
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Proto.Types (Encryption)
import HBS2.Net.Auth.Credentials
import HBS2.Prelude

import Codec.Serialise(serialise)
import Data.Data
import GHC.Generics
import Data.Hashable hiding (Hashed)
import Data.Maybe (fromMaybe)

newtype HashRef = HashRef { fromHashRef :: Hash HbSync }
                  deriving newtype (Eq,Ord,IsString,Pretty,Hashable)
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
  AnnotatedHashRef (Maybe HashRef) HashRef
  deriving stock (Data,Show,Generic)

data SequentialRef =
  SequentialRef Integer AnnotatedHashRef
  deriving stock (Data,Show,Generic)

instance Serialise AnnotatedHashRef
instance Serialise SequentialRef
instance Serialise HashRef
instance Serialise HashRefMetadata
instance Serialise HashRefObject

---

data RefGenesis s = RefGenesis
  { refOwner :: !(PubKey 'Sign s)
  , refName :: !Text
  , refMeta :: !AnnMetaData
  }
  deriving stock (Generic)

instance Serialise (PubKey 'Sign s) => Serialise (RefGenesis s)

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

data instance MutableRef s 'LinearRef
  = LinearMutableRef
  { lrefId :: !(Hash HbSync)
  , lrefHeight :: !Int
  -- , lrefMTree :: !(MTreeAnn [Hash HbSync])
  , lrefVal :: !(Hash HbSync)
  }
  deriving stock (Generic, Show)

instance Serialise (MutableRef s 'LinearRef)

---

data SignPhase = SignaturePresent | SignatureVerified

data family Signed ( p :: SignPhase ) a

data instance Signed SignaturePresent (MutableRef s 'LinearRef)
  = LinearMutableRefSigned
  { signature :: Signature s
  , signedRef :: MutableRef s 'LinearRef
  }
  deriving stock (Generic)

instance Serialise (Signature s) =>
    Serialise (Signed 'SignaturePresent (MutableRef s 'LinearRef))

data instance Signed 'SignatureVerified (MutableRef s 'LinearRef)
  = LinearMutableRefSignatureVerified
  { signature :: Signature s
  , signedRef :: MutableRef s 'LinearRef
  , signer :: PubKey 'Sign s
  }
  deriving stock (Generic)

---

nodeLinearRefsRef :: PubKey 'Sign s -> RefGenesis s
nodeLinearRefsRef pk = RefGenesis
  { refOwner = pk
  , refName = "List of node linear refs"
  , refMeta = NoMetaData
  }


type IsRefPubKey s =  ( Eq (PubKey 'Sign s)
                      , Serialise (PubKey 'Sign s)
                      , FromStringMaybe (PubKey 'Sign s)
                      , Hashable (PubKey 'Sign s)
                      )


