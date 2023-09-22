{-# Language TemplateHaskell #-}
module HBS2Git.GitRepoLog where

import HBS2.Prelude.Plated
import HBS2.Git.Types
import HBS2.Data.Types.Refs

import HBS2.System.Logger.Simple

import Data.Word
import Data.Function
import Lens.Micro.Platform
import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy (ByteString)
-- import System.IO
import UnliftIO.IO
import Control.Monad.IO.Unlift
import Codec.Compression.GZip
import System.Directory
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Control.Concurrent.STM
import Data.Maybe

class HasGitLogOptions a where
  compressEntries  :: a -> Bool
  compressWholeLog :: a -> Bool


-- | default GitLogOptions
instance HasGitLogOptions () where
  compressEntries = const True
  compressWholeLog = const False

data CompressWholeLog = CompressWholeLog

instance HasGitLogOptions CompressWholeLog where
  compressEntries = const False
  compressWholeLog = const True

data GitLogEntryType = GitLogEntryCommit
                     | GitLogEntryBlob
                     | GitLogEntryTree
                     | GitLogEntryHead
                     | GitLogHead
                     | GitLogDeps
                     | GitLogHeadDel
                     | GitLogContext
                     deriving stock (Eq,Ord,Enum,Generic,Show)


newtype GitLogTimeStamp = GitLogTimeStamp Int
                          deriving stock (Eq,Ord,Show,Data,Generic)

instance Serialise GitLogTimeStamp

newtype GitLogHeadEntry =
  GitLogHeadEntry
  { _gitLogHeadAfter :: Maybe HashRef
  }
  deriving stock (Eq,Generic)

instance Serialise GitLogHeadEntry

makeLenses ''GitLogHeadEntry


newtype GitLogDepsEntry =
  GitLogDepsEntry
  { _gitLogDeps :: [HashRef]
  }
  deriving stock (Eq,Generic)

makeLenses ''GitLogDepsEntry

instance Serialise GitLogDepsEntry

-- deletion is handled by special way.
-- we need a context WHEN the reference is deleted
-- because it may be deleted, created again, deleted again, etc.
-- Having current repository context via collecting all reference states
-- we may calculate an actual current state of the reference.
-- Or, we may use a special code to mark object as deleted
data GitLogHeadDelEntry =
  GitLogHeadDelEntry
  { _gitHeadContext :: [(GitRef, GitHash)] -- this gives us context to order this delete operation
  , _gitHeadDeleted :: GitRef              -- this is a reference to delete
  }
  deriving stock (Eq,Generic)

makeLenses ''GitLogHeadDelEntry

instance Serialise GitLogHeadDelEntry

data GitLogContextEntry =
    GitLogNoContext
  | GitLogContextCommits (HashSet GitHash)
  | GitLogContextRank Int
  deriving stock (Eq,Data,Generic)

commitsOfGitLogContextEntry :: GitLogContextEntry -> [GitHash]
commitsOfGitLogContextEntry = \case
  GitLogContextCommits co -> HashSet.toList co
  _ -> mempty

instance Serialise GitLogContextEntry

data GitLogEntry =
  GitLogEntry
  { _gitLogEntryType :: GitLogEntryType
  , _gitLogEntryHash :: Maybe GitHash
  , _gitLogEntrySize :: Word32
  }
  deriving stock (Eq,Ord,Generic,Show)

makeLenses 'GitLogEntry

entryHeadSize :: Integral a => a
entryHeadSize = 64

instance Serialise GitLogEntryType
instance Serialise GitLogEntry

gitLogEntryTypeOf :: GitObjectType -> GitLogEntryType
gitLogEntryTypeOf = \case
  Commit -> GitLogEntryCommit
  Tree   -> GitLogEntryTree
  Blob   -> GitLogEntryBlob

-- | scans hbs2-git repo log
gitRepoLogScan :: forall m . MonadUnliftIO m
               => Bool         -- ^ do read log section content
               -> FilePath     -- ^ log file path
               -> (GitLogEntry -> Maybe ByteString -> m ()) -- ^ log section callback
               -> m ()

gitRepoLogScan r fn cb = do

  trace $ "gitRepoLogScan" <+> pretty fn
  withBinaryFile fn ReadMode $ \h -> do
    sz <- liftIO $ getFileSize fn
    go h sz

  where
    go _ 0 = pure ()
    go h size = do
      ss <- liftIO $ LBS.hGet h entryHeadSize
      let es = deserialise @GitLogEntry ss
      let esize = es ^. gitLogEntrySize
      let consumed = entryHeadSize + fromIntegral esize
      if r then do
        o <- liftIO $ LBS.hGet h (fromIntegral esize) <&> decompress
        cb es (Just o)
      else do
        liftIO $ hSeek h RelativeSeek (fromIntegral esize)
        cb es Nothing
      go h ( max 0 (size - consumed) )

gitRepoLogWriteHead :: forall o m . (HasGitLogOptions o, MonadIO m)
                    => o
                    -> Handle
                    -> GitLogHeadEntry
                    -> m ()

gitRepoLogWriteHead opt fh e = do
  let s = serialise e
  let entry = GitLogEntry GitLogHead Nothing (fromIntegral $ LBS.length s)
  gitRepoLogWriteEntry opt fh entry s



gitRepoLogMakeEntry :: forall o . (HasGitLogOptions o)
                    => o
                    -> GitLogEntry
                    -> ByteString
                    -> ByteString

gitRepoLogMakeEntry opts entry' o = bs <> ss
  where
    ss = compressWith co o
    entry = entry' & set gitLogEntrySize (fromIntegral $ LBS.length ss)
    bs = LBS.take entryHeadSize $ serialise entry <> LBS.replicate entryHeadSize 0
    co | compressEntries opts = defaultCompressParams { compressLevel = bestSpeed }
       | otherwise            = defaultCompressParams { compressLevel = noCompression }

gitRepoLogWriteEntry :: forall o m . (MonadIO m, HasGitLogOptions o)
                     => o
                     -> Handle
                     -> GitLogEntry
                     -> ByteString
                     -> m ()

gitRepoLogWriteEntry opts fh entry' o = do
  let entryWithSize = gitRepoLogMakeEntry opts entry' o
  liftIO $ LBS.hPutStr fh entryWithSize

gitRepoMakeIndex :: FilePath -> IO (HashSet GitHash)
gitRepoMakeIndex fp = do
  here <- doesFileExist fp
  if not here then do
    pure mempty
  else do
    out <- newTQueueIO

    gitRepoLogScan  False fp $ \e _ -> do
      atomically $ writeTQueue out ( e ^. gitLogEntryHash )

    atomically $ flushTQueue out <&> HashSet.fromList . catMaybes


