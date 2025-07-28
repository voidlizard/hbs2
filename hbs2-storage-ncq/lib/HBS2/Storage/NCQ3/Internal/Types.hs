module HBS2.Storage.NCQ3.Internal.Types where

import HBS2.Storage.NCQ3.Internal.Prelude

import Text.Printf

data CachedMMap =
    CachedData  ByteString
  | CachedIndex ByteString NWayHash


type CachePrio = Word64

type Shard = TVar (HashMap HashRef NCQEntry)

type StateVersion = Word64

newtype FileKey = FileKey Word32
                  deriving newtype (Eq,Ord,Show,Num,Enum,Pretty,Hashable)

instance IsString FileKey where
  fromString = FileKey . read

instance ToFileName (DataFile FileKey) where
  toFileName (DataFile fk) = ncqMakeFossilName fk

instance ToFileName (IndexFile FileKey) where
  toFileName (IndexFile fk) = printf "i-%08x.cq" (coerce @_ @Word32 fk)

data  NCQEntry =
  NCQEntry
  { ncqEntryData   :: !ByteString
  , ncqDumped      :: !(TVar (Maybe FileKey))
  }

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
  , ncqMaxCached      :: Int
  , ncqIdleThrsh      :: Double
  , ncqMMapCache      :: TVar (HashPSQ FileKey CachePrio CachedMMap)
  , ncqStateFiles     :: TVar (HashSet FileKey)
  , ncqStateIndex     :: TVar (HashSet FileKey)
  , ncqStateFileSeq   :: TVar FileKey
  , ncqStateVersion   :: TVar StateVersion
  , ncqStateUsage     :: TVar (IntMap (Int, HashSet FileKey))
  , ncqMemTable       :: Vector Shard
  , ncqWrites         :: TVar Int
  , ncqWriteEMA       :: TVar Double  -- for writes-per-seconds
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqWriteOps       :: Vector (TQueue (IO ()))
  , ncqAlive          :: TVar Bool
  , ncqStopReq        :: TVar Bool
  , ncqSyncReq        :: TVar Bool
  , ncqOnRunWriteIdle :: TVar (IO ())
  , ncqSyncNo         :: TVar Int
  }


ncqMakeFossilName :: FileKey -> FilePath
ncqMakeFossilName = printf "f-%08x.data" . coerce @_ @Word32

