module HBS2.Storage.Compact
  ( CompactStorage
  ) where


import Data.Word
import Data.ByteString
import Data.Coerce
import Codec.Serialise
import GHC.Generics
import System.IO
import UnliftIO

-- compact storage
-- for the off-tree data representation
-- may be it will be faster, than Simple storage
-- who knows

newtype EntryOffset = EntryOffset Word64
                      deriving newtype (Ord,Eq,Num,Enum,Real)
                      deriving stock Generic

newtype EntrySize = EntrySize Word64
                    deriving newtype (Ord,Eq,Num,Enum,Real)
                    deriving stock Generic

data IndexEntry =
  IndexEntry
  { idxEntryPrev   :: Maybe Word64
  , idxEntryOffset :: EntryOffset
  , idxEntrySize   :: EntrySize
  , idxEntryKey    :: ByteString
  }
  deriving stock Generic



data CompactStorage =
  CompactStorage
  { csHandle :: Handle
  }

type ForCompactStorage m = MonadIO m


compactStorageOpen :: ForCompactStorage m => FilePath -> m CompactStorage
compactStorageOpen fp = undefined


