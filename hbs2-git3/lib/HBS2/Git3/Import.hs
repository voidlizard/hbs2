{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language MultiWayIf #-}
module HBS2.Git3.Import where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Git
import HBS2.Git3.Git.Pack
import HBS2.Git3.Config.Local

import HBS2.Data.Detect (readLogThrow,deepScan,ScanLevel(..))
import HBS2.Storage.Operations.Missed
import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Data.Log.Structured

import HBS2.System.Dir
import Data.Config.Suckless.Almost.RPC
import Data.Config.Suckless.Script

import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Either
import Data.List qualified as L
import Network.ByteOrder qualified as N
import System.IO.Temp as Temp
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.IO.File qualified as UIO
import System.IO (hPrint)

import Streaming.Prelude qualified as S

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
                >>= orThrow (MissedBlockError2 (show $ pretty href))

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


data ImportStage =
    ImportStart
  | ImportWIP  Int (Maybe HashRef)
  | ImportWait (Maybe Int) ImportStage
  | ImportDone (Maybe HashRef)

{- HLINT ignore "Functor law" -}

importGitRefLog :: forall m . ( HBS2GitPerks m
                              -- , HasStorage m
                              -- , HasClientAPI PeerAPI UNIX m
                              -- , HasClientAPI RefLogAPI UNIX m
                              -- , HasGitRemoteKey m
                              -- , MonadReader Git3Env m
                              )
             => Git3 m (Maybe HashRef)

importGitRefLog = withStateDo $ ask >>= \case
  Git3Disconnected{} -> throwIO Git3PeerNotConnected
  env@Git3Connected{..} ->  do

    packs <- gitDir
               >>= orThrow NoGitDir
               <&> (</> "objects/pack")

    mkdir packs

    sto <- getStorage

    already_ <- newTVarIO (mempty :: HashSet HashRef)

    flip fix ImportStart $ \again -> \case
      ImportDone x -> do
        notice "import done"
        for_ x updateImportedCheckpoint
        updateReflogIndex
        pure x

      ImportWait d next -> do

        pause @'Seconds 1.15

        down <- callRpcWaitRetry @RpcGetProbes (TimeoutSec 1) 3 peerAPI ()
                   >>= orThrow RpcTimeout
                   <&> maybe 0 fromIntegral . headMay . mapMaybe \case
                         ProbeSnapshotElement "Download.wip" n -> Just n
                         _ -> Nothing

        notice $ "wait some time..." <+> parens (pretty down)

        case d of
          Just n | down /= n || down == 0 -> again next

          _ -> pause @'Seconds 2.85 >> again (ImportWait (Just down) next)

      ImportStart -> do

        rvl  <- readTVarIO gitRefLogVal

        importGroupKeys

        prev <- importedCheckpoint

        if | isNothing prev -> again $ ImportWIP 0 prev

           | prev /= rvl -> do
              again $ ImportWIP 0 prev

           | otherwise -> again $ ImportDone prev

      ImportWIP attempt prev -> do

        r <- try @_ @OperationError $ do

          excl <- maybe1 prev (pure mempty) $ \p -> do
            txListAll (Just p) <&> HS.fromList . fmap fst

          rv <- refLogRef

          hxs <- txList ( pure . not . flip HS.member excl ) rv

          cp' <- flip fix (fmap snd hxs, Nothing) $ \next -> \case
            ([], r) -> pure (gitTxTree <$> r)
            (TxSegment{}:xs, l) -> next (xs, l)
            (cp@(TxCheckpoint n tree) : xs, l) -> do

              -- full <- findMissedBlocks sto tree <&> L.null
              missed_ <- newTVarIO 0
              deepScan ScanDeep (\_ -> atomically $ modifyTVar missed_ succ)
                                (coerce tree)
                                (getBlock sto)
                                (const none)

              full <- readTVarIO missed_ <&> (==0)

              if full && Just n > (getGitTxRank <$> l) then do
                next (xs, Just cp)
              else do
                next (xs, l)

          case cp' of
            Nothing -> pure Nothing
            Just cp -> do

              notice $ "found checkpoint" <+> pretty cp
              txs <- txList ( pure . not . flip HS.member excl ) (Just cp)

              forConcurrently_ txs $ \case
                (_, TxCheckpoint{}) -> none
                (h, TxSegment tree) -> do
                  new <- readTVarIO already_ <&> not . HS.member tree

                  when new do
                    s <- writeAsGitPack  packs tree

                    for_ s $ \file -> do
                      gitRunCommand [qc|git index-pack {file}|]
                        >>= orThrowPassIO

                    atomically $ modifyTVar already_ (HS.insert tree)
                    notice $ "imported" <+> pretty h

              pure (Just cp)

        case r of
          Right cp -> again $ ImportDone cp
          Left  (MissedBlockError2 _) -> notice "missed blocks" >> again (ImportWait Nothing (ImportWIP (succ attempt) prev))
          Left  MissedBlockError      -> notice "missed blocks" >> again (ImportWait Nothing (ImportWIP (succ attempt) prev))
          Left  e                     -> throwIO e



groupKeysFile :: MonadIO m => Git3 m FilePath
groupKeysFile = getStatePathM <&> (</> "groupkeys")

readGroupKeyFile :: MonadIO m => Git3 m (Maybe HashRef)
readGroupKeyFile = do
  file <- groupKeysFile
  debug $ "readGroupKeyFile" <+> pretty file
  liftIO (try @_ @IOError (readFile file))
    <&> fromRight mempty
    <&> parseTop
    <&> fromRight mempty
    <&> \x -> headMay [ w | ListVal [HashLike w] <- x ]

importGroupKeys :: forall m . ( HBS2GitPerks m
                              )
             => Git3 m ()

importGroupKeys = do

  debug $ "importGroupKeys"
  sto <- getStorage

  already <- readGroupKeyFile

  LWWRef{..} <- getRepoRefMaybe >>= orThrow GitRepoRefNotSet
  rhead <- readLogThrow (getBlock sto) lwwValue
  let keyTree' = headMay (tailSafe rhead)

  when (keyTree' /= already) do

    ops <- S.toList_ $ for_ keyTree' $ \tree -> do
      keyhashes <- readLogThrow (getBlock sto) tree
      for_ keyhashes $ \h -> do
        S.yield $ mkForm @C "gk:track" [mkSym (show $ pretty h)]

    -- FIXME: check-added-keys
    unless (null ops) do
      _ <- callProc "hbs2-keyman" ["run","stdin"] ops
      updateGroupKeys keyTree'

  where

    updateGroupKeys keyTree' = do
      file <- groupKeysFile
      void $ runMaybeT do
        val <- keyTree' & toMPlus
        liftIO $ UIO.withBinaryFileAtomic file WriteMode $ \fh -> do
          hPrint fh $ pretty (mkSym @C (show $ pretty val))

