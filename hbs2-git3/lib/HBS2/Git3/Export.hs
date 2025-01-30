{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}

module HBS2.Git3.Export (exportEntries,export) where

import HBS2.Git3.Prelude
import HBS2.Git3.State
import HBS2.Git3.Git
import HBS2.Data.Detect

import HBS2.Data.Log.Structured

import HBS2.CLI.Run.Internal.Merkle (createTreeWithMetadata)
-- import HBS2.CLI.Run.RefLog (mkRefLogUpdateFrom)

import HBS2.System.Dir

import HBS2.Git3.Config.Local

import Data.Config.Suckless.Script

import Codec.Compression.Zstd.Streaming qualified as ZstdS
import Codec.Compression.Zstd.Streaming (Result(..))
import Data.ByteString.Builder as Builder
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.Fixed
import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import System.IO (hPrint)
import System.IO qualified as IO
import System.IO.Temp as Temp
import UnliftIO.Concurrent

data ExportException =
   ExportWriteTimeout
 | ExportRefLogNotSet
  deriving stock (Show,Typeable)

instance Exception ExportException

data ECC =
    ECCInit
  | ECCWrite Int FilePath Handle Result
  | ECCFinalize Int Bool FilePath Handle Result


genRefLogUpdate :: forall m . MonadUnliftIO m => ByteString -> Git3 m (RefLogUpdate L4Proto)
genRefLogUpdate txraw = do
  (puk,privk) <- getRepoRefLogCredentials
  makeRefLogUpdate @L4Proto @'HBS2Basic puk privk txraw

exportEntries :: forall m . (HBS2GitPerks m) => Id -> MakeDictM C (Git3 m) ()
exportEntries prefix = do
  entry $ bindMatch (prefix <> "export") $ nil_ $ \syn -> lift $ connectedDo do
    let (opts, argz) = splitOpts [("--ref",1),("--set",2),("--del",1)] syn

    let hd = headDef "HEAD" [ x | StringLike x <- argz]
    h <- gitRevParseThrow hd

    refs' <- S.toList_ $ for opts $ \case
              ListVal [StringLike "--ref", StringLike x] -> do
                S.yield (gitNormaliseRef (fromString x), h)

              ListVal [StringLike "--set", StringLike x, StringLike what] -> do
                y <- gitRevParseThrow what
                S.yield $ (gitNormaliseRef (fromString x), y)

              ListVal [StringLike "--del", StringLike x] -> do
                S.yield $ (gitNormaliseRef (fromString x), GitHash (BS.replicate 20 0))

              _ -> none

    let refs = HM.toList $ HM.fromList refs'
    export (Just h) refs

export :: forall m . HBS2GitPerks m => Maybe GitHash -> [(GitRef, GitHash)] -> Git3 m ()
export mbh refs = withStateDo do
      tn <- getNumCapabilities

      updateReflogIndex

      idx <- openIndex

      _already <- newTVarIO ( mempty :: HashSet GitHash )
      _exported <- newTVarIO 0

      enumEntries idx $ \bs -> do
        atomically $ modifyTVar _already (HS.insert (coerce $ BS.take 20 bs))

      level    <- getCompressionLevel
      segment  <- getPackedSegmetSize
      env      <- ask

      let
        notWrittenYet :: forall m . MonadIO m => GitHash -> m Bool
        notWrittenYet x = do
            already <- readTVarIO _already <&> HS.member x
            pure (not already) -- &&  not alsoInIdx)

      hpsq <- maybe1 mbh (pure HPSQ.empty) $ \h -> do
                readCommitChainHPSQ notWrittenYet Nothing h (\c -> debug $ "commit" <+> pretty c)

      txCheckQ <- newTVarIO ( mempty :: HashSet HashRef )

      let r = HPSQ.toList hpsq
                & sortBy (comparing (view _2))
                & fmap (view _1)

      let total = HPSQ.size hpsq
      bytes_ <- newTVarIO 0

      debug $ "TOTAL" <+> pretty total

      liftIO $ flip runContT pure do

        sourceQ <- newTBQueueIO (fromIntegral tn * 1024)
        hbs2Q   <- newTBQueueIO @_ @(Maybe (FilePath, Int)) 100

        hbs2 <- liftIO $ async $ void $ withGit3Env env do
          sto <- getStorage
          reflogAPI <- getClientAPI @RefLogAPI @UNIX

          reflog <- getGitRemoteKey
                        >>= orThrowUser "reflog not set"

          gk' <- getGK

          lift $ fix \next -> atomically (readTBQueue hbs2Q) >>= \case
            Nothing -> none
            Just (fn,_) -> void $ flip runContT pure do
              ContT $ bracket none (const $ rm fn)
              lift do
                ts <- liftIO getPOSIXTime <&> round
                lbs <- LBS.readFile fn
                let meta = mempty

                exported <- readTVarIO _exported
                debug $ red "EXPORTED" <+> pretty exported

                let gkh = fst <$> gk'
                let gk = snd <$> gk'

                when (exported > 0) do
                  href <- createTreeWithMetadata sto gk meta lbs >>= orThrowPassIO
                  writeLogEntry ("tree" <+> pretty ts <+> pretty href )
                  debug $ "SENDING" <+> pretty href <+> pretty fn

                  let payload = LBS.toStrict $ serialise (AnnotatedHashRef Nothing href)
                  tx <- withGit3Env env $ genRefLogUpdate payload

                  let txh = hashObject @HbSync (serialise tx) & HashRef

                  atomically (modifyTVar txCheckQ (HS.insert txh))

                  callRpcWaitMay @RpcRefLogPost (TimeoutSec 2) reflogAPI tx
                    >>= orThrowUser "rpc timeout"

                rm fn
                next

        link hbs2

        l <- lift (async (segmentWriter env bytes_ sourceQ hbs2Q) >>= \x -> link x >> pure x)

        let chunkSize = if total > tn*2 then total `div` tn else total
        let commitz = chunksOf chunkSize r

        progress_ <- newTVarIO 0

        gitCatBatchQ <- contWorkerPool tn do
          che <- ContT withGitCat
          pure $ gitReadObjectMaybe che

        -- void $ ContT $ bracket (pure pool) cancel

        let lastCommit = lastMay r

        workers <- lift $ forM (zip [0..] commitz) $ \(i,chunk) -> async $ flip runContT pure do

          -- let gitCatBatchQ commit = gitReadObjectMaybe theReader commit

          for_ chunk  \commit -> do

              atomically $ modifyTVar progress_ succ

              (_,self) <- lift (gitCatBatchQ commit)
                             >>= orThrow (GitReadError (show $ pretty commit))

              tree <- gitReadCommitTree self

              hashes <- gitReadTreeObjectsOnly commit
                            <&> ([commit,tree]<>)
                            >>= filterM notWrittenYet

              for_ hashes $ \gh -> do
                atomically do
                  modifyTVar _already (HS.insert gh)
                  modifyTVar _exported succ

                -- debug $ "object" <+> pretty gh
                (_t,lbs) <- lift (gitCatBatchQ gh)
                               >>= orThrow (GitReadError (show $ pretty gh))

                let e = [ Builder.byteString (coerce gh)
                        , Builder.char8 (headDef 'B' $ show $ pretty $ Short _t)
                        , Builder.lazyByteString lbs
                        ] & Builder.toLazyByteString . mconcat

                atomically do
                  writeTBQueue sourceQ (Just e)

              when (Just commit == lastCommit) do
                writeRefSectionSome sourceQ refs

        t0 <- getTimeCoarse
        ContT $ withAsync $ do

          notice $ "segment" <+> pretty segment <> comma
                             <> "compression level"
                             <+> pretty level

          flip fix (t0,0)  $ \next (tPrev,bytesPrev) -> do

            pause @'Seconds 1

            p <- readTVarIO progress_
            b <- readTVarIO bytes_

            let pp = fromIntegral p / (fromIntegral total :: Double) * 100
                         & realToFrac @_ @(Fixed E2)

            t1 <- getTimeCoarse

            let dt = realToFrac @_ @Double (t1 - tPrev) * 1e-9
                       & realToFrac @_ @(Fixed E2)

            let tspent = realToFrac (t1 - t0) * 1e-9 & realToFrac @_ @(Fixed E2)

            let mbytes = realToFrac b / 1024/1024  & realToFrac @_ @(Fixed E2)

            let dbdt = mbytes / tspent

            -- liftIO $ IO.hPutStr stderr  $ show $
            --     "                 \r"
            --     <+> pretty tspent <> "s"
            --     <+> pretty mbytes <> "mb"
            --     <+> pretty dbdt <> "mbs"
            --     <+> pretty pp <> "%"

            -- liftIO $ IO.hPutStr stderr  $ "\r"

            next (t1,b)

        mapM_ link workers
        mapM_ wait workers

        exported <- readTVarIO _exported

        when (exported == 0 && not (L.null refs)) do
          notice $ "no new segments, but refs" <+> pretty lastCommit
          writeRefSectionSome sourceQ refs
          atomically $ modifyTVar _exported succ

        atomically do
          writeTBQueue sourceQ Nothing

        mapM_  wait [hbs2,l]

        txh <- liftIO $ withGit3Env env (postCheckPoint 30.0 =<< readTVarIO txCheckQ)

        notice $ "checkpoint" <+> pretty txh

 where

   writeLogEntry e = do
     path' <- getConfigPath
     for_ path' $ \path -> do
       let logPath = path </> "log"
       touch path
       liftIO (IO.appendFile logPath (show $ e <> line))

   writeRefSectionSome :: forall m1 . MonadIO m1 => TBQueue (Maybe LBS.ByteString) -> [(GitRef, GitHash)] -> m1 ()
   writeRefSectionSome sourceQ refsAndCommits = do
     ts <- liftIO $ getPOSIXTime <&> round

     let brefs = [ LBS8.pack (show $ pretty ts <+> pretty commit <+> pretty ref)
                 | (ref, commit) <- refsAndCommits
                 ] & LBS8.unlines

     let sha1 = gitHashBlobPure brefs

     let e = [ Builder.byteString (coerce sha1)
             , Builder.char8 'R'
             , Builder.lazyByteString brefs
             ] & Builder.toLazyByteString . mconcat

     atomically do
          writeTBQueue sourceQ (Just e)

   segmentWriter env bytes_ sourceQ hbs2Q = flip runReaderT env do
     maxW <- getPackedSegmetSize
     level <- getCompressionLevel
     lift $ flip fix ECCInit $ \loop -> \case
       ECCInit -> do
         zstd <- ZstdS.compress level
         fn <- emptySystemTempFile "hbs2-git-export"
         logFile <- IO.openBinaryFile fn WriteMode
         debug $ red "NEW FILE" <+> pretty fn
         loop $ ECCWrite 0 fn logFile zstd

       ECCWrite bnum fn fh sn | bnum >= maxW -> do
         loop (ECCFinalize bnum True fn fh sn)

       ECCWrite bnum fn fh sn -> do
         atomically (readTBQueue sourceQ) >>= \case
            Nothing  -> loop (ECCFinalize bnum False fn fh sn)
            Just s -> do
               lbs <- S.toList_ (writeSection s $ S.yield) <&> mconcat

               sz_ <- newTVarIO 0

               sn1 <- writeCompressedChunkZstd (write sz_ fh) sn (Just lbs)

               sz <- readTVarIO sz_ <&> fromIntegral
               atomically $ modifyTVar bytes_ (+ fromIntegral sz)

               loop (ECCWrite (bnum + sz) fn fh sn1)

       ECCFinalize bnum again fn fh sn -> do
         void $ writeCompressedChunkZstd (write bytes_ fh) sn Nothing
         hClose fh
         atomically $ writeTBQueue hbs2Q (Just (fn, bnum))
         debug $ "SEGMENT" <+> pretty bnum <+> pretty fn
         when again $ loop ECCInit
         atomically $ writeTBQueue hbs2Q Nothing

    where
       write sz_ fh ss = do
          LBS.hPutStr fh ss
          atomically $ modifyTVar sz_ (+ LBS.length ss)

   -- checks if all transactions written to reflog
   -- post tx with current reflog value
   postCheckPoint :: forall m1 . ( MonadUnliftIO  m1
                                 -- , HasStorage m1
                                 -- , HasClientAPI RefLogAPI UNIX m1
                                 -- , HasGitRemoteKey m1
                                 )
                  => Timeout 'Seconds
                  -> HashSet HashRef
                  -> Git3 m1 (Maybe HashRef)

   postCheckPoint _ txq | HS.null txq = pure Nothing
   postCheckPoint t txq = perform  >>= either (const $ throwIO ExportWriteTimeout) pure
     where
       perform = race (pause t) do
         notice "wait reflog write to complete"
         sto <- getStorage
         api <- getClientAPI @RefLogAPI @UNIX
         reflog <- getGitRemoteKey >>= orThrow ExportRefLogNotSet

         cp <- flip fix txq $ \next q -> do

           let wnext w = pause @'Seconds 0.85 >> next w

           rv <- runMaybeT do
                    lift (callRpcWaitRetry @RpcRefLogGet (TimeoutSec 1) 2 api reflog)
                      >>= toMPlus
                      >>= toMPlus

           maybe1 rv (wnext q) $ \x -> do
             rset <- HS.fromList <$> readLogThrow (getBlock sto) x

             let diff =  txq `HS.difference` rset

             if not (HS.null diff) then do
               debug "again"
               wnext diff
             else
               pure x

         t0 <- liftIO getPOSIXTime <&> round
         let payload = LBS.toStrict $ serialise (SequentialRef t0 (AnnotatedHashRef Nothing cp))
         tx <- genRefLogUpdate payload

         callRpcWaitMay @RpcRefLogPost (TimeoutSec 2) api tx
           >>= orThrow ExportWriteTimeout

         pure $ Just $ HashRef (hashObject @HbSync (serialise tx))


