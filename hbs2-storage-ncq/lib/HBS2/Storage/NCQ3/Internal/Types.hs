module HBS2.Storage.NCQ3.Internal.Types where

import HBS2.Storage.NCQ3.Internal.Prelude

import Numeric (readHex)
import Text.Printf

data CachedData =  CachedData !ByteString
data CachedIndex = CachedIndex !ByteString !NWayHash


type CachePrio = TimeSpec

type Shard = TVar (HashMap HashRef NCQEntry)

type StateVersion = Word64

newtype FileKey = FileKey Word32
                  deriving newtype (Eq,Ord,Show,Num,Enum,Pretty,Hashable)

deriving stock instance Eq (DataFile FileKey)
deriving stock instance Ord (DataFile FileKey)
deriving stock instance Eq (IndexFile FileKey)
deriving stock instance Ord (IndexFile FileKey)

instance IsString FileKey where
  fromString = FileKey . maybe maxBound fst . headMay . readHex . drop 1 . dropWhile (/= '-') . takeBaseName

instance ToFileName (DataFile FileKey) where
  toFileName (DataFile fk) = ncqMakeFossilName fk

instance ToFileName (IndexFile FileKey) where
  toFileName (IndexFile fk) = printf "i-%08x.cq" (coerce @_ @Word32 fk)

instance ToFileName (StateFile FileKey) where
  toFileName (StateFile fk) = printf "s-%08x" (coerce @_ @Word32 fk)

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

instance Pretty Location where
  pretty = \case
    InFossil k  o s -> parens $ "in-fossil" <+> pretty k <+> pretty o <+> pretty s
    InMemory _      -> "in-memory"

data Fact =
  FI (DataFile FileKey) (IndexFile FileKey) -- file X has index Y
  deriving stock (Eq,Ord)

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
  , ncqStateFiles     :: TVar (HashSet FileKey)
  , ncqStateIndex     :: TVar [(Down POSIXTime, FileKey)] -- backward timestamp orde
  , ncqStateFileSeq   :: TVar FileKey
  , ncqStateVersion   :: TVar StateVersion
  , ncqStateUsage     :: TVar (IntMap (Int, HashSet FileKey))
  , ncqStateFacts     :: TVar (Set Fact)
  , ncqMemTable       :: Vector Shard
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


ncqMakeFossilName :: FileKey -> FilePath
ncqMakeFossilName = printf "f-%08x.data" . coerce @_ @Word32

