{-# Language RecordWildCards #-}
module HBS2.Storage.NCQ2.Internal.Types where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Storage
import HBS2.Misc.PrettyStuff
import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.SD
import HBS2.Data.Log.Structured.NCQ

import HBS2.Storage.NCQ.Types

import Data.Config.Suckless.System

import Numeric (showHex)
import Network.ByteOrder qualified as N
import Data.HashMap.Strict (HashMap)
import Control.Concurrent.STM.TSem
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..), (|>),(<|))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Word
import Data.Vector qualified as V
import Data.Vector (Vector, (!))
import Lens.Micro.Platform
import Data.HashSet (HashSet)
import System.FilePath.Posix

import Control.Monad.ST
import System.Random.MWC as MWC

import UnliftIO


type FOff = Word64

data  NCQEntry =
  NCQEntry
  { ncqEntryData   :: !ByteString
  , ncqDumped      :: !(TVar (Maybe FileKey))
  }

type Shard = TVar (HashMap HashRef NCQEntry)

type NCQOffset = Word64
type NCQSize   = Word32

type StateVersion = Word64

data NCQIdxEntry =
  NCQIdxEntry {-# UNPACK#-} !NCQOffset !NCQSize

data StateOP = D FileKey | F TimeSpec FileKey | P FileKey
               deriving (Eq,Ord,Show)

data NCQFlag =
  NCQMergeNow | NCQCompactNow
  deriving (Eq,Ord,Generic)

data Location =
       InFossil {-# UNPACK #-} !FileKey !ByteString !NCQOffset !NCQSize
     | InMemory {-# UNPACK #-} !ByteString

instance Pretty Location where
  pretty = \case
    InFossil k _ o s -> parens $ "in-fossil" <+> pretty k <+> pretty o <+> pretty s
    InMemory _     -> "in-memory"

data TrackedFile =
  TrackedFile
  { tfTime   :: FilePrio
  , tfKey    :: FileKey
  , tfCached :: TVar (Maybe CachedEntry)
  }

data FactE = KeyIntersection FileKey FileKey Int
             deriving (Eq,Ord,Show,Generic)

type FactSeq = POSIXTime

newtype FactKey =
  FactKey ByteString
  deriving newtype (Eq,Ord,Hashable)

data Fact =
  Facot
  { factWritten :: Maybe FactSeq
  , factE       :: FactE
  }
  deriving (Eq,Ord,Show,Generic)

instance Hashable FactE
instance Hashable Fact

type TrackedFiles = Vector TrackedFile

data NCQStorage2 =
  NCQStorage2
  { ncqRoot           :: FilePath
  , ncqGen            :: Int
  , ncqSalt           :: HashRef
  , ncqPostponeMerge  :: Timeout 'Seconds
  , ncqPostponeSweep  :: Timeout 'Seconds
  , ncqLuckyNum       :: Int
  , ncqFsync          :: Int
  , ncqWriteQLen      :: Int
  , ncqWriteBlock     :: Int
  , ncqMinLog         :: Int
  , ncqMaxLog         :: Int
  , ncqMaxCached      :: Int
  , ncqIdleThrsh      :: Double
  , ncqMemTable       :: Vector Shard
  , ncqWriteQ         :: TVar (Seq HashRef)
  , ncqWriteOps       :: Vector (TQueue (IO ()))
  , ncqReadReq        :: TQueue (HashRef, TMVar (Maybe Location))
  , ncqStorageTasks   :: TVar Int
  , ncqStorageStopReq :: TVar Bool
  , ncqStorageSyncReq :: TVar Bool
  , ncqMergeReq       :: TVar Bool
  , ncqMergeSem       :: TSem
  , ncqSyncNo         :: TVar Int
  , ncqCurrentFiles   :: TVar (HashSet FileKey)
  , ncqTrackedFiles   :: TVar TrackedFiles
  , ncqStateVersion   :: TVar StateVersion
  , ncqStateUsage     :: TVar (IntMap (Int, HashSet FileKey))
  , ncqStateName      :: TVar (Maybe (StateFile FileKey))
  , ncqStateSem       :: TSem
  , ncqCachedEntries  :: TVar Int
  , ncqWrites         :: TVar Int
  , ncqWriteEMA       :: TVar Double  -- for writes-per-seconds
  , ncqJobQ           :: TQueue (IO ())
  , ncqMiscSem        :: TSem
  , ncqSweepSem       :: TSem
  , ncqMergeTasks     :: TVar Int
  , ncqOnRunWriteIdle :: TVar (IO ())

  , ncqFactFiles      :: TVar (HashSet FileKey)
  , ncqFacts          :: TVar (HashMap FactKey Fact)
  , ncqRndGen         :: Gen RealWorld
  }


ncqGetFileName :: NCQStorage2 -> FilePath -> FilePath
ncqGetFileName ncq fp = ncqGetWorkDir ncq </> takeFileName fp

ncqGetWorkDir :: NCQStorage2 -> FilePath
ncqGetWorkDir NCQStorage2{..} = ncqRoot </> show ncqGen

ncqGetLockFileName :: NCQStorage2 -> FilePath
ncqGetLockFileName ncq = ncqGetFileName ncq ".lock"

ncqNewUniqFileName :: MonadIO m => NCQStorage2 -> FilePath -> FilePath -> m FilePath
ncqNewUniqFileName  me@NCQStorage2{..} pref suff = liftIO $ withSem ncqMiscSem do
 flip fix 0 $ \next i -> do
   t <- round @_ @Integer . (* 1e9) <$> getPOSIXTime
   let v = show $ pretty (showHex t "") <> "-" <> pretty (showHex i "")
   let n = ncqGetFileName  me (pref <> v <> suff)
   doesFileExist n >>= \case
    False -> pure n
    True  -> next (succ i)

ncqEmptyKey :: ByteString
ncqEmptyKey = BS.replicate ncqKeyLen 0

ncqGetFactsDir :: NCQStorage2 -> FilePath
ncqGetFactsDir me = ncqGetWorkDir me </> ".facts"

ncqGetNewFossilName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewFossilName me = ncqNewUniqFileName me "fossil-" ".data"

ncqGetNewStateName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewStateName me = ncqNewUniqFileName me "state-" ""

ncqGetNewCompactName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewCompactName me = ncqNewUniqFileName me "compact-" ".data"

ncqGetNewFactFileName :: MonadIO m => NCQStorage2 -> m FilePath
ncqGetNewFactFileName me = do
  ncqNewUniqFileName me (d </> "fact-") ".f"
  where d = ncqGetFactsDir me

ncqStorageStop2 :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqStorageStop2 NCQStorage2{..} = do
  atomically $ writeTVar ncqStorageStopReq True

ncqStorageSync2 :: MonadUnliftIO m => NCQStorage2 -> m ()
ncqStorageSync2 NCQStorage2{..} = do
  atomically $ writeTVar ncqStorageSyncReq True

ncqShardIdx :: NCQStorage2 -> HashRef -> Int
ncqShardIdx NCQStorage2{..} h =
  fromIntegral (BS.head (coerce h)) `mod` V.length ncqMemTable
{-# INLINE ncqShardIdx #-}

ncqGetShard :: NCQStorage2 -> HashRef -> Shard
ncqGetShard ncq@NCQStorage2{..} h = ncqMemTable ! ncqShardIdx ncq h
{-# INLINE ncqGetShard #-}


useVersion :: forall m a . MonadUnliftIO m => NCQStorage2 -> (() -> m a) -> m a
useVersion ncq m = bracket succV predV m
  where
    succV = atomically (ncqStateUseSTM ncq)
    predV = const $ atomically (ncqStateUnuseSTM ncq)


ncqStateUseSTM :: NCQStorage2 -> STM ()
ncqStateUseSTM NCQStorage2{..} = do
  k <- readTVar ncqStateVersion <&> fromIntegral
  modifyTVar ncqStateUsage (IntMap.update (Just . over _1 succ) k)
{-# INLINE ncqStateUseSTM #-}

ncqStateUnuseSTM :: NCQStorage2 -> STM ()
ncqStateUnuseSTM NCQStorage2{..} = do
  k <- readTVar ncqStateVersion <&> fromIntegral
  -- TODO: remove when n <= 0
  modifyTVar ncqStateUsage (IntMap.update (Just . over _1 pred) k)
{-# INLINE ncqStateUnuseSTM #-}

withSem :: forall a m . MonadUnliftIO m => TSem -> m a -> m a
withSem sem m = bracket enter leave (const m)
  where enter = atomically (waitTSem sem)
        leave = const $ atomically (signalTSem sem)



ncqLookupIndex :: MonadUnliftIO m
               => HashRef
               -> (ByteString, NWayHash)
               -> m (Maybe NCQIdxEntry )
ncqLookupIndex hx (mmaped, nway) = do
  fmap decodeEntry <$> nwayHashLookup nway mmaped (coerce hx)
{-# INLINE ncqLookupIndex #-}

decodeEntry :: ByteString -> NCQIdxEntry
decodeEntry entryBs = do
    let (p,r) = BS.splitAt 8 entryBs
    let off = fromIntegral (N.word64 p)
    let size = fromIntegral (N.word32 (BS.take 4 r))
    NCQIdxEntry off size
{-# INLINE decodeEntry #-}


isNotPending :: Maybe CachedEntry -> Bool
isNotPending = \case
  Just (PendingEntry {}) -> False
  _ -> True

isPending :: Maybe CachedEntry -> Bool
isPending = not . isNotPending


ncqListTrackedFilesSTM :: NCQStorage2 -> STM (Vector (FileKey, Maybe CachedEntry, TVar (Maybe CachedEntry)))
ncqListTrackedFilesSTM NCQStorage2{..} = do
  fs <- readTVar ncqTrackedFiles
  for fs $ \TrackedFile{..} -> (tfKey,,) <$> readTVar tfCached <*> pure tfCached

ncqListTrackedFiles :: MonadUnliftIO m => NCQStorage2 -> m (Vector (FileKey, Maybe CachedEntry, TVar (Maybe CachedEntry)))
ncqListTrackedFiles = atomically . ncqListTrackedFilesSTM

