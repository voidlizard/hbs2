{-# Language UndecidableInstances #-}
module HBS2.Storage.NCQ.Types where

import HBS2.Prelude
import HBS2.Data.Types.Refs
import HBS2.Hash

import HBS2.Data.Log.Structured.NCQ

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Network.ByteOrder qualified as N
import Data.Ord (Down(..))
import Data.Coerce
import System.FilePath
import Data.Word
import Data.Data
import Control.Exception

import UnliftIO (TVar)

-- Log structure:
-- (SD)*
-- S      ::= word32be, section prefix
-- D      ::= HASH PREFIX DATA
-- HASH   ::= BYTESTRING(32)
-- PREFIX ::= BYTESTRING(4)
-- DATA   ::= BYTESTRING(n) | n == S - LEN(WORD32) - LEN(HASH) - LEN(PREFIX)

data NCQStorageException =
    NCQStorageAlreadyExist String
  | NCQStorageSeedMissed
  | NCQStorageTimeout
  | NCQStorageCurrentAlreadyOpen
  | NCQStorageCantOpenCurrent
  | NCQStorageBrokenCurrent
  | NCQMergeInvariantFailed String
  | NCQCompactInvariantFailed String
  | NCQStorageCantLock FilePath
  | NCQStorageCantMapFile FilePath
  deriving stock (Show,Typeable)

instance Exception NCQStorageException

newtype FileKey = FileKey ByteString
                  deriving newtype (Eq,Ord,Hashable,Show,Serialise)

instance IsString FileKey where
  fromString = FileKey . BS8.pack . dropExtension . takeFileName

instance Pretty FileKey where
  pretty (FileKey s) = pretty (BS8.unpack s)

newtype DataFile a = DataFile a
                      deriving newtype (IsString,Pretty)

newtype IndexFile a = IndexFile a
                      deriving newtype (IsString,Pretty)

newtype StateFile a = StateFile a
                      deriving newtype (IsString,Eq,Ord,Pretty)

class ToFileName a where
  toFileName :: a -> FilePath

instance ToFileName FilePath where
  toFileName = id

instance ToFileName FileKey where
  toFileName = BS8.unpack . coerce

instance ToFileName (DataFile FileKey) where
  toFileName (DataFile fk) = dropExtension  (toFileName fk) `addExtension` ".data"

instance ToFileName (IndexFile FileKey) where
  toFileName (IndexFile fk) = dropExtension  (toFileName fk) `addExtension` ".cq"

instance ToFileName (DataFile FilePath) where
  toFileName (DataFile fp) = dropExtension  fp `addExtension` ".data"

instance ToFileName (IndexFile FilePath) where
  toFileName (IndexFile fp) = dropExtension  fp `addExtension` ".cq"

instance ToFileName (StateFile FileKey) where
  toFileName (StateFile fk) = toFileName fk

newtype FilePrio = FilePrio (Down TimeSpec)
                    deriving newtype (Eq,Ord)
                    deriving stock (Generic,Show)

mkFilePrio :: TimeSpec -> FilePrio
mkFilePrio = FilePrio . Down

timeSpecFromFilePrio :: FilePrio -> TimeSpec
timeSpecFromFilePrio (FilePrio what) = getDown what
{-# INLINE timeSpecFromFilePrio #-}

data CachedEntry =
  CachedEntry { cachedMmapedIdx  :: ByteString
              , cachedMmapedData :: ByteString
              , cachedNway       :: NWayHash
              , cachedTs         :: TVar TimeSpec
              }
  | PendingEntry {}

instance Show CachedEntry where
  show = \case
    CachedEntry{}  -> "CachedEntry{...}"
    PendingEntry{} -> "PendingEntry{...}"

newtype NCQFullRecordLen a =
  NCQFullRecordLen a
  deriving newtype (Num,Enum,Integral,Real,Ord,Eq)

-- including prefix
ncqFullDataLen :: forall a . Integral a => NCQFullRecordLen a -> a
ncqFullDataLen full = fromIntegral full - ncqKeyLen
{-# INLINE ncqFullDataLen #-}

ncqKeyLen :: forall a . Integral a => a
ncqKeyLen = 32
{-# INLINE ncqKeyLen #-}

-- 'S' in SD, i.e size, i.e section header
ncqSLen:: forall a . Integral a => a
ncqSLen = 4
{-# INLINE ncqSLen #-}

ncqDataOffset :: forall a b . (Integral a, Integral b) => a -> b
ncqDataOffset base = fromIntegral base + ncqSLen + ncqKeyLen
{-# INLINE ncqDataOffset #-}


ncqFullTombLen :: forall a . Integral a => a
ncqFullTombLen = ncqSLen + ncqKeyLen + ncqPrefixLen + 0
{-# INLINE ncqFullTombLen #-}


data NCQSectionType = B | R | T | M
                      deriving stock (Eq,Ord,Show)

instance Pretty NCQSectionType where
  pretty = \case
    B -> "B"
    T -> "T"
    R -> "R"
    M -> "M"

ncqPrefixLen :: Integral a => a
ncqPrefixLen = 4
{-# INLINE ncqPrefixLen #-}

ncqRefPrefix :: ByteString
ncqRefPrefix = "R;;\x00"

ncqBlockPrefix :: ByteString
ncqBlockPrefix = "B;;\x00"

ncqTombPrefix :: ByteString
ncqTombPrefix = "T;;\x00"

ncqMetaPrefix :: ByteString
ncqMetaPrefix = "M;;\x00"

ncqIsMeta :: ByteString -> Maybe NCQSectionType
ncqIsMeta bs = headMay [ t | (t,x) <- meta, BS.isPrefixOf x bs ]
  where meta = [ (R, ncqRefPrefix)
               , (B, ncqBlockPrefix)
               , (T, ncqTombPrefix)
               , (M, ncqMetaPrefix)
               ]

ncqMakeSectionBS :: Maybe NCQSectionType
                 -> HashRef
                 -> ByteString
                 -> ByteString
ncqMakeSectionBS t h bs = do
  let slen = ncqKeyLen + prefLen + fromIntegral (BS.length bs)
  let ss = N.bytestring32 slen
  let section = ss <> coerce h <> prefix <> bs
  section

  where
    (prefLen, prefix) =
      case t of
        Nothing -> (0, mempty)
        Just B  -> (ncqPrefixLen, ncqBlockPrefix)
        Just T  -> (ncqPrefixLen, ncqTombPrefix)
        Just R  -> (ncqPrefixLen, ncqRefPrefix)
        Just M  -> (ncqPrefixLen, ncqMetaPrefix)

{-# INLINE ncqMakeSectionBS #-}


data NCQFsckException =
  NCQFsckException | NCQFsckIssueExt NCQFsckIssueType
  deriving stock (Show,Typeable)

instance Exception NCQFsckException

data NCQFsckIssueType =
    FsckInvalidPrefix
  | FsckInvalidContent
  | FsckInvalidFileSize Integer
  deriving stock (Eq,Ord,Show,Data,Generic)

data NCQFsckIssue =
  NCQFsckIssue FilePath Word64 NCQFsckIssueType
  deriving stock (Eq,Ord,Show,Data,Generic)


posixToTimeSpec :: POSIXTime -> TimeSpec
posixToTimeSpec pt =
  let (s, frac) = properFraction pt :: (Integer, POSIXTime)
      ns = round (frac * 1e9)
  in TimeSpec (fromIntegral s) ns


megabytes :: forall a . Integral a => a
megabytes = 1024 ^ 2

gigabytes :: forall a . Integral a => a
gigabytes = 1024 ^ 3


