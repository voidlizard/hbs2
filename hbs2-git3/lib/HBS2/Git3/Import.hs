{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Import where

import HBS2.Git3.Prelude
import HBS2.Git3.State.Index
import HBS2.Git3.Git
import HBS2.Git3.Git.Pack
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Git3.State.Segment

import HBS2.Data.Log.Structured

import HBS2.System.Dir

import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import System.IO.Temp as Temp
import UnliftIO.IO.File qualified as UIO
import Network.ByteOrder qualified as N

data ImportException =
  ImportInvalidSegment HashRef
  deriving stock (Show,Typeable)

instance Exception ImportException

writeAsGitPack :: forall m . (HBS2GitPerks m, HasStorage m)
              => FilePath
              -> HashRef
              -> m (Maybe FilePath)

writeAsGitPack dir href = do

  sto <- getStorage

  file <- liftIO $ Temp.emptyTempFile dir (show (pretty href) <> ".pack")

  no_ <- newTVarIO 0

  liftIO $ UIO.withBinaryFileAtomic file ReadWriteMode $ \fh -> flip runContT pure do

    let header = BS.concat [ "PACK", N.bytestring32 2,  N.bytestring32 0 ]

    liftIO $ BS.hPutStr fh header

    seen_ <- newTVarIO (mempty :: HashSet GitHash)

    source <- liftIO (runExceptT (getTreeContents sto href))
                >>= orThrow MissedBlockError

    lbs' <- decompressSegmentLBS source

    lbs <- ContT $ maybe1 lbs' none

    runConsumeLBS lbs $ readLogFileLBS () $ \h s obs -> do
      seen <- readTVarIO seen_ <&> HS.member h
      unless seen do

        let (t, body) = LBS.splitAt 1 obs

        let tp = fromStringMay @(Short GitObjectType) (LBS8.unpack t)
                     & maybe Blob coerce

        let params = Zlib.defaultCompressParams { Zlib.compressMethod = Zlib.deflateMethod }

        let packed = Zlib.compressWith params body

        let preamble = encodeObjectSize (gitPackTypeOf tp) (fromIntegral $ LBS.length body)

        liftIO do
          atomically $ modifyTVar seen_ (HS.insert h)
          BS.hPutStr fh preamble
          LBS.hPutStr fh packed

        atomically $ modifyTVar no_ succ

    no <- readTVarIO no_
    hSeek fh AbsoluteSeek 8
    liftIO $ BS.hPutStr fh (N.bytestring32 no)
    hFlush fh

    sz <- hFileSize fh
    hSeek fh AbsoluteSeek 0

    sha <- liftIO $ LBS.hGetNonBlocking fh (fromIntegral sz) <&> sha1lazy

    hSeek fh SeekFromEnd 0

    liftIO $ BS.hPutStr fh sha

  no <- readTVarIO no_

  if no > 0 then do
    pure $ Just file
  else do
    rm file
    pure Nothing

