{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
module HBS2.Storage.NCQ3.Internal.Types where

import HBS2.Storage.NCQ3.Internal.Prelude

import Data.Generics.Product
import Numeric (readHex)
import Data.Set qualified as Set
import Data.HashSet qualified as HS
import Text.Printf
-- import Lens.Micro.Platform


data CachedData =  CachedData !ByteString
data CachedIndex = CachedIndex !ByteString !NWayHash


type CachePrio = TimeSpec

type Shard = TVar (HashMap HashRef NCQEntry)

type StateVersion = Word64

newtype FileKey = FileKey Word32
                  deriving newtype (Eq,Ord,Show,Num,Enum,Real,Integral,Pretty,Hashable)

deriving stock instance Eq (DataFile FileKey)
deriving stock instance Ord (DataFile FileKey)
deriving stock instance Eq (IndexFile FileKey)
deriving stock instance Ord (IndexFile FileKey)

data  NCQEntry =
  NCQEntry
  { ncqEntryData   :: !ByteString
  , ncqDumped      :: !(TVar (Maybe FileKey))
  }

type NCQOffset = Word64
type NCQSize   = Word32

data Location =
       InFossil {-# UNPACK #-} !FileKey !NCQOffset !NCQSize
     | InMemory {-# UNPACK #-} !ByteString


data Fact =
    FI (DataFile FileKey) (IndexFile FileKey) -- file X has index Y
  | P  PData                                  -- pending, not indexed
  deriving stock (Eq,Ord)

data PData = PData (DataFile FileKey) Word64

instance Ord PData where
  compare (PData a _) (PData b _) = compare a b

instance Eq PData where
  (==) (PData a _) (PData b _) = a == b

data NCQState =
  NCQState
  { ncqStateFiles     :: HashSet FileKey
  , ncqStateIndex     :: [(Down POSIXTime, FileKey)] -- backward timestamp order
  , ncqStateFileSeq   :: FileKey
  , ncqStateVersion   :: StateVersion
  , ncqStateFacts     :: Set Fact
  }
  deriving stock (Eq,Generic)

data NCQStorage3 =
  NCQStorage3
  { ncqRoot           :: FilePath
  , ncqGen            :: Int
  , ncqSalt           :: HashRef
  , ncqPostponeMerge  :: Timeout 'Seconds
  , ncqPostponeSweep  :: Timeout 'Seconds
  , ncqFsync          :: Int
  , ncqWriteQLen      :: Int
  , ncqWriteBlock     :: Int
  , ncqMinLog         :: Int
  , ncqMaxLog         :: Int
  , ncqMaxCachedIndex :: Int
  , ncqMaxCachedData  :: Int
  , ncqIdleThrsh      :: Double
  , ncqMMapCachedIdx  :: TVar (HashPSQ FileKey CachePrio CachedIndex)
  , ncqMMapCachedData :: TVar (HashPSQ FileKey CachePrio CachedData)
  , ncqMemTable       :: Vector Shard
  , ncqState          :: TVar NCQState
  , ncqWrites         :: TVar Int
  , ncqWriteEMA       :: TVar Double  -- for writes-per-seconds
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqWriteOps       :: Vector (TQueue (IO ()))
  , ncqReadReq        :: TQueue (HashRef, TMVar (Maybe Location))
  , ncqAlive          :: TVar Bool
  , ncqStopReq        :: TVar Bool
  , ncqSyncReq        :: TVar Bool
  , ncqOnRunWriteIdle :: TVar (IO ())
  , ncqSyncNo         :: TVar Int
  }



instance Monoid FileKey where
  mempty = FileKey 0

instance Semigroup  FileKey where
  (<>) (FileKey a) (FileKey b) = FileKey (max a b)

instance IsString FileKey where
  fromString = FileKey . maybe maxBound fst . headMay . readHex . drop 1 . dropWhile (/= '-') . takeBaseName

instance ToFileName (DataFile FileKey) where
  toFileName (DataFile fk) = ncqMakeFossilName fk

instance ToFileName (IndexFile FileKey) where
  toFileName (IndexFile fk) = printf "i-%08x.cq" (coerce @_ @Word32 fk)

instance ToFileName (StateFile FileKey) where
  toFileName (StateFile fk) = printf "s-%08x" (coerce @_ @Word32 fk)


instance Monoid NCQState where
  mempty = ncqState0

instance Semigroup NCQState where
  (<>) a b = NCQState files index seqq version facts
    where
      files   = ncqStateFiles a <> ncqStateFiles b
      index   = ncqStateIndex a <> ncqStateIndex b
      seqq    = max (ncqStateFileSeq a) (ncqStateFileSeq b)
      version = max (ncqStateVersion a) (ncqStateVersion b)
      facts   = ncqStateFacts a <> ncqStateFacts b


instance Pretty Location where
  pretty = \case
    InFossil k  o s -> parens $ "in-fossil" <+> pretty k <+> pretty o <+> pretty s
    InMemory _      -> "in-memory"

ncqMakeFossilName :: FileKey -> FilePath
ncqMakeFossilName = printf "f-%08x.data" . coerce @_ @Word32

ncqState0 :: NCQState
ncqState0 = NCQState{..}
  where
    ncqStateFiles = mempty
    ncqStateIndex = mempty
    ncqStateVersion = 0
    ncqStateFacts = mempty
    ncqStateFileSeq = 0


instance Pretty NCQState where
  pretty NCQState{..} = vcat
    [ prettyIndex
    , prettyFiles
    , prettyFacts
    , prettySeq
    ]
    where
      prettySeq = "n" <+> pretty ncqStateFileSeq

      prettyIndex = vcat
        [ "i" <+> pretty fk <+> pretty (round @_ @Word64 p)
        | (Down p, fk) <- ncqStateIndex
        ]

      prettyFiles = vcat
        [ "f" <+> pretty fk
        | fk <- HS.toList ncqStateFiles
        ]

      prettyFacts = vcat
        [ pf f
        | f <- Set.toList ncqStateFacts
        ]

      pf (FI (DataFile a) (IndexFile b)) = "fi" <+> pretty a <+> pretty b
      pf (P (PData (DataFile a) s)) = "fp" <+> pretty a <+> pretty s

