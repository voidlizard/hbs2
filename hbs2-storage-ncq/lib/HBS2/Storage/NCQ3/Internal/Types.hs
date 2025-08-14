{-# LANGUAGE UndecidableInstances #-}
module HBS2.Storage.NCQ3.Internal.Types where

import HBS2.Storage.NCQ3.Internal.Prelude

import Numeric (readHex)
import Data.Data
import Data.Set qualified as Set
import Data.HashSet qualified as HS
import Text.Printf
import Control.Concurrent.STM.TSem (TSem,waitTSem,signalTSem)
import System.FileLock (FileLock)
import Data.Vector qualified as V

data CachedData =  CachedData !ByteString
data CachedIndex = CachedIndex !ByteString !NWayHash


type CachePrio = TimeSpec

type Shard = TVar (HashMap HashRef NCQEntry)

type StateVersion = Word64

newtype FileKey = FileKey Word64
                  deriving newtype (Eq,Ord,Show,Num,Enum,Real,Integral,Pretty,Hashable)
                  deriving stock (Data,Generic)

deriving stock instance Eq (DataFile FileKey)
deriving stock instance Ord (DataFile FileKey)
deriving stock instance Eq (IndexFile FileKey)
deriving stock instance Ord (IndexFile FileKey)
deriving stock instance Data (IndexFile FileKey)
deriving stock instance Data (DataFile FileKey)
deriving stock instance Data (StateFile FileKey)

data  NCQEntry =
  NCQEntry
  { ncqEntryData   :: !ByteString
  , ncqDumped      :: !(TVar (Maybe FileKey))
  }

type NCQOffset = Word64
type NCQFileSize = NCQOffset
type NCQSize   = Word32

data Location =
       InFossil {-# UNPACK #-} !FileKey !NCQOffset !NCQSize
     | InMemory {-# UNPACK #-} !ByteString


data Fact = P PData  -- pending, not indexed
  deriving stock (Eq,Ord,Data)

data PData = PData (DataFile FileKey) Word64
             deriving stock (Data)

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
  deriving stock (Eq,Generic,Data)

data NCQStorage =
  NCQStorage
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
  , ncqStateKey       :: TVar FileKey
  , ncqStateUse       :: TVar (HashMap FileKey (NCQState, TVar Int))
  , ncqWrites         :: TVar Int
  , ncqWriteEMA       :: TVar Double  -- for writes-per-seconds
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqWriteOps       :: Vector (TQueue (IO ()))
  , ncqSyncOps        :: TQueue (IO ())
  , ncqReadReq        :: TQueue (HashRef, TMVar (Maybe Location))
  , ncqAlive          :: TVar Bool
  , ncqStopReq        :: TVar Bool
  , ncqSyncReq        :: TVar Bool
  , ncqOnRunWriteIdle :: TVar (IO ())
  , ncqSyncNo         :: TVar Int
  , ncqServiceSem     :: TSem
  , ncqFileLock       :: TVar (Maybe FileLock)
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
  toFileName (IndexFile fk) = printf "i-%016x.cq" (coerce @_ @Word64 fk)

instance ToFileName (StateFile FileKey) where
  toFileName (StateFile fk) = printf "s-%016x" (coerce @_ @Word64 fk)


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
ncqMakeFossilName = printf "f-%016x.data" . coerce @_ @Word64

withSem :: forall a m . MonadUnliftIO m => TSem -> m a -> m a
withSem sem action =
  bracket (atomically (waitTSem sem))
          (const $ atomically (signalTSem sem))
          (const action)

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

      pf (P (PData (DataFile a) s)) = "fp" <+> pretty a <+> pretty s



ncqTombEntrySize :: NCQSize
ncqTombEntrySize = ncqSLen + ncqKeyLen + ncqPrefixLen

ncqIsTombEntrySize :: Integral a => a -> Bool
ncqIsTombEntrySize s = fromIntegral s <= ncqTombEntrySize
{-# INLINE ncqIsTombEntrySize #-}

ncqDeferredWriteOpSTM :: NCQStorage -> IO () -> STM ()
ncqDeferredWriteOpSTM NCQStorage{..} work = do
    nw <- readTVar ncqWrites <&> (`mod` V.length ncqWriteOps)
    writeTQueue (ncqWriteOps ! nw) work

