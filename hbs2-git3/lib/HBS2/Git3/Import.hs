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

import Control.Applicative
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
  | ImportWIP  (Timeout 'Seconds) Int (Maybe HashRef)
  | ImportWait (Timeout 'Seconds)  (Maybe Int) ImportStage
  | ImportDone (Maybe HashRef)

{- HLINT ignore "Functor law" -}

importGitRefLog :: forall m . ( HBS2GitPerks m
                              )
             => Git3 m (Maybe HashRef)

importGitRefLog = do
    packs <- gitDir
               >>= orThrow NoGitDir
               <&> (</> "objects/pack")

    mkdir packs

    doImport packs `catch` (\( e :: OperationError) -> err (viaShow e) >> pause @'Seconds 1 >> doImport packs)

  where
    doImport packs = withStateDo $ ask >>= \case
      Git3Disconnected{} -> throwIO Git3PeerNotConnected
      Git3Connected{..} ->  flip runContT pure do

        sto <- getStorage

        already_ <- newTVarIO (mempty :: HashSet HashRef)

        oldRvl <- gitRefLogVal & readTVarIO
        reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet
        newRvl_ <- newTVarIO Nothing

        void $ ContT $ withAsync $ forever do
          void $ lift (callRpcWaitMay @RpcRefLogFetch (TimeoutSec 2) reflogAPI reflog)

          lift (callRpcWaitMay @RpcRefLogGet (TimeoutSec 2) reflogAPI reflog)
            >>= \case
                   Just (Just x) | Just x /= oldRvl -> atomically (writeTVar newRvl_ (Just x))
                   _  -> none

          pause @'Seconds 10

        lift $ flip fix ImportStart $ \again -> \case
          ImportDone x -> do
            notice "import done"

            newRlv <- readTVarIO newRvl_
            let doAgain = newRlv /= oldRvl

            updateReflogIndex
            for_ x updateImportedCheckpoint

            refs <- importedRefs

            if not (null refs && isJust x) || doAgain then do
              pure x
            else do
              atomically do
                writeTVar newRvl_ Nothing
                writeTVar gitRefLogVal (newRlv <|> oldRvl)

              notice $ "import: go again"
              again ImportStart

          ImportWait sec d next -> do

            pause sec

            down <- callRpcWaitRetry @RpcGetProbes (TimeoutSec 1) 3 peerAPI ()
                       >>= orThrow RpcTimeout
                       <&> maybe 0 fromIntegral . headMay . mapMaybe \case
                             ProbeSnapshotElement "Download.wip" n -> Just n
                             _ -> Nothing

            notice $ "wait-for-download" <+> parens (pretty down)

            case d of
              Just n | down /= n || down == 0 -> again next

              _ -> pause @'Seconds 2.85 >> again (ImportWait (sec*1.10) (Just down) next)

          ImportStart -> do

            rvl  <- readTVarIO gitRefLogVal

            importGroupKeys

            prev <- importedCheckpoint

            if | isNothing prev -> again $ ImportWIP 1.0 0 prev

               | prev /= rvl -> do
                  again $ ImportWIP 1.0 0 prev

               | otherwise -> again $ ImportDone prev

          ImportWIP w attempt prev -> do

            notice $ "download wip" <+> pretty attempt

            r <- try @_ @OperationError $ do

              excl <- maybe1 prev (pure mempty) $ \p -> do
                txListAll (Just p) <&> HS.fromList . fmap fst

              rv <- refLogRef

              hxs <- txList ( pure . not . flip HS.member excl ) rv

              cp' <- flip fix (fmap snd hxs, Nothing) $ \next -> \case
                ([], r) -> pure r
                (TxSegment{}:xs, l) -> next (xs, l)
                (cp@(TxCheckpoint n tree) : xs, l) -> do

                  missed <- findMissedBlocks sto tree

                  let full = L.null missed

                  if full && Just n > (getGitTxRank <$> l) then do
                    next (xs, Just cp)
                  else do
                    next (xs, l)

              case cp' of
                Just TxCheckpoint{..} -> do

                  notice $ "checkpoint" <+> pretty gitTxTree <+> pretty gitTxRank
                  txs <- txList ( pure . not . flip HS.member excl ) (Just gitTxTree)

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

                  pure (Just gitTxTree)

                _ -> do
                  notice "no checkpoints found"
                  pure Nothing

            case r of
              Right cp -> again $ ImportDone cp

              Left  (MissedBlockError2 _) -> do
                notice "missed blocks"
                again (ImportWait w Nothing (ImportWIP (w*1.15) (succ attempt) prev))

              Left  MissedBlockError      -> do
                notice "missed blocks"
                again (ImportWait w Nothing (ImportWIP (w*1.15) (succ attempt) prev))

              Left  e                     -> err (viaShow e) >> throwIO e


groupKeysFile :: (MonadIO m) => Git3 m FilePath
groupKeysFile = getStatePathM <&> (</> "groupkeys")

readGroupKeyFile :: (MonadIO m) => Git3 m (Maybe HashRef)
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

  notice $ "importGroupKeys"
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

