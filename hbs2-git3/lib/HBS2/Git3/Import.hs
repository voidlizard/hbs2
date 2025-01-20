{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Import where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Git
import HBS2.Git3.Git.Pack

import HBS2.Storage.Operations.Missed
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Data.Log.Structured

import HBS2.System.Dir

import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List qualified as L
import Network.ByteOrder qualified as N
import System.IO.Temp as Temp
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.IO.File qualified as UIO

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



importGitRefLog :: forall m . ( HBS2GitPerks m
                              , HasStorage m
                              , HasClientAPI PeerAPI UNIX m
                              , HasClientAPI RefLogAPI UNIX m
                              , HasGitRemoteKey m
                              , MonadReader Git3Env m
                              )
             => m (Maybe HashRef)

importGitRefLog = do

  fix \next -> do
    updateReflogIndex `catch` \case
      MissedBlockError -> do
        pause @'Seconds 2.0
        warn "missed block on import"
        next

      e -> throwIO e

  packs <- gitDir
             >>= orThrowUser "git directory not found"
             <&> (</> "objects/pack")

  mkdir packs

  sto <- getStorage

  prev <- importedCheckpoint

  excl <- maybe1 prev (pure mempty) $ \p -> do
    txListAll (Just p) <&> HS.fromList . fmap fst

  rv <- refLogRef

  hxs <- txList ( pure . not . flip HS.member excl ) rv

  cp' <- flip fix (fmap snd hxs, Nothing) $ \next -> \case
    ([], r) -> pure (gitTxTree <$> r)
    (TxSegment{}:xs, l) -> next (xs, l)
    (cp@(TxCheckpoint n tree) : xs, l) -> do
      full <- findMissedBlocks sto tree <&> L.null
      if full && Just n > (getGitTxRank <$> l) then do
        next (xs, Just cp)
      else do
        next (xs, l)

  runMaybeT do
    cp <- toMPlus cp'
    notice $ "found checkpoint" <+> pretty cp
    txs <- lift $ txList ( pure . not . flip HS.member excl ) (Just cp)

    lift do
      forConcurrently_ txs $ \case
        (_, TxCheckpoint{}) -> none
        (h, TxSegment tree) -> do
          s <- writeAsGitPack  packs tree

          for_ s $ \file -> do
            gitRunCommand [qc|git index-pack {file}|]
              >>= orThrowPassIO

          notice $ "imported" <+> pretty h

      updateImportedCheckpoint cp

      pure cp

