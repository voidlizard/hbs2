module HBS2.Storage.Compact
  (
  ) where


import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder as B
import Data.Binary.Get
import Data.Coerce
import Codec.Serialise
import GHC.Generics
-- import System.IO
import Lens.Micro.Platform
import Control.Monad.Except
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


data Header =
  Header
  { hdrMagic       :: Word16
  , hdrVersion     :: Word16
  , hdrIndexOffset :: EntryOffset
  }
  deriving stock Generic

data CompactStorage =
  CompactStorage
  { csHandle :: MVar Handle
  }

type ForCompactStorage m = MonadIO m

data CompactStorageOpenOpt = Default
                             deriving stock (Eq,Ord)

data CompactStorageOpenError =
       InvalidHeader
  deriving stock (Typeable,Show)

instance Exception CompactStorageOpenError


compactStorageOpen :: ForCompactStorage m
                   => CompactStorageOpenOpt
                   -> FilePath
                   -> m CompactStorage

compactStorageOpen _ fp = do
  ha <- openFile fp ReadWriteMode
  mha <- newMVar ha
  header <- readHeader mha >>= maybe (throwIO InvalidHeader) pure
  pure $ CompactStorage mha

compactStorageCommit :: ForCompactStorage m => CompactStorage -> m ()
compactStorageCommit sto = do
  pure ()

appendHeader :: ForCompactStorage m => Handle -> EntryOffset -> m ()
appendHeader ha offset = do
  let bs =   word16BE headerMagic
          <> word16BE headerVersion
          <> word64BE (coerce offset)
          <> byteString (BS.replicate 52 0)
  liftIO $ LBS.hPut ha (B.toLazyByteString bs)

readHeader :: ForCompactStorage m => MVar Handle -> m (Maybe Header)
readHeader mha = do
  bs <- liftIO $ withMVar mha $ \ha -> do
    hSeek ha SeekFromEnd (-64)
    LBS.hGet ha 64

  let what = flip runGetOrFail bs do
                Header <$> getWord16be
                       <*> getWord16be
                       <*> getOffset

  pure $ either (const Nothing) (Just . view _3) what

  where
    getOffset = EntryOffset <$> getWord64be

headerMagic :: Word16
headerMagic = 32264

headerVersion :: Word16
headerVersion = 1


