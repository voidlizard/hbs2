module HBS2.Git3.Run where

import HBS2.Git3.Prelude
import HBS2.Data.Log.Structured

import HBS2.CLI.Run.Internal.Merkle (getTreeContents)

import HBS2.System.Dir

import HBS2.Git3.Git
import HBS2.Git3.Export
import HBS2.Git3.Import
import HBS2.Git3.State
import HBS2.Git3.Repo qualified as Repo
import HBS2.Git3.Repo
import HBS2.Git3.Logger

import Data.Config.Suckless.Script
import Data.Config.Suckless.Almost.RPC

import Codec.Compression.Zstd.Lazy qualified as ZstdL

import Codec.Compression.Zlib qualified as Zlib

import Data.HashPSQ qualified as HPSQ

import Data.Maybe
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Network.ByteOrder qualified as N
import Text.InterpolatedString.Perl6 (qc)
import Data.HashSet qualified as HS
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Fixed
import Data.Either
import Lens.Micro.Platform

import Streaming.Prelude qualified as S

import System.Exit qualified as Q
import Control.Concurrent.STM qualified as STM
import System.Directory (setCurrentDirectory)
import System.TimeIt
import System.IO (hPrint)

import UnliftIO.Concurrent

{- HLINT ignore "Functor law" -}

theDict :: forall m . ( HBS2GitPerks m
                      ) => Dict C (Git3 m)
theDict = do
  makeDict @C do
    -- TODO: write-man-entries
    myEntries
    entry $ bindValue "best" (mkInt 22)
    hidden $ internalEntries

  where

    myEntries =  do
        entry $ bindMatch "--help" $ nil_ $ \case
          HelpEntryBound what -> do
            helpEntry what
            quit

          [ StringLike x ] -> helpList True (Just x) >> quit

          _ -> helpList True Nothing >> quit

        hidden do
          entry $ bindMatch "--help-all" $ nil_ $ \case
            [ StringLike x ] -> helpList False (Just x) >> quit
            _ -> helpList False Nothing >> quit

        brief "set zstd compression level" do
          examples [qc|
compression best ;  sets compression level to best (22)
compression 4    ;  sets low compression level (faster)
compression      ;  prints compression level
          |] do
            entry $ bindMatch "compression" $ nil_ $ \case
              [ LitIntVal n ] -> lift do
                setCompressionLevel (fromIntegral n)

              [] -> lift do
                co <- getCompressionLevel
                liftIO $ print $ pretty co

              _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "segment" $ nil_ $ \case
          [ LitIntVal n ] -> lift do
            setPackedSegmedSize (fromIntegral n)

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "quiet" $ nil_ $ const $ lift do
          silence

        entry $ bindMatch "index-block-size" $ nil_ \case
          [ LitIntVal size ]-> lift do
            setIndexBlockSize (fromIntegral size)

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "git:tree:ls" $ nil_ $ const do
          r <- gitReadTree "HEAD"
          for_ r $ \GitTreeEntry{..} -> do
            liftIO $ print $  pretty gitEntryHash
                          <+> pretty gitEntryType
                          <+> pretty gitEntrySize
                          <+> pretty gitEntryName

        entry $ bindMatch "debug" $ nil_ $ const do
         setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

        -- hidden do

        entry $ bindMatch "test:git:normalize-ref" $ nil_  \case
          [ StringLike s ] -> display $ mkStr @C (show $ pretty $ gitNormaliseRef (fromString s))
          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:hbs2:peer:poke" $ nil_ $ \syn -> do
          peer <- getClientAPI @PeerAPI @UNIX
          r    <- callRpcWaitRetry @RpcPoke (TimeoutSec 0.5) 2 peer () >>= orThrowUser "hbs2-peer not found"
          notice $ pretty r

        hidden do
          entry $ bindMatch "git:hash:blob" $ nil_ $ const $ liftIO do
            co <- LBS.hGetContents stdin
            print $ pretty $ gitHashBlobPure co

        entry $ bindMatch "zlib:deflate" $ nil_ $ const $ liftIO do
          LBS.hGetContents stdin <&> Zlib.decompress >>= LBS.hPutStr stdout

        entry $ bindMatch "test:git:read-commit-chain" $ nil_ $ \syn -> lift do
          (mpath, hss) <- case syn of
            [ HashLike s ] -> pure (Nothing, s)
            [ StringLike path , HashLike s ] -> pure (Just path, s)
            [ StringLike path ] -> pure (Just path, "HEAD")
            [] -> pure (Nothing, "HEAD")
            _ -> throwIO (BadFormException @C nil)

          void $ flip runContT pure do

            liftIO $ mapM_ setCurrentDirectory mpath

            idx <- lift openIndex

            let req h = lift $ indexEntryLookup idx h <&> isNothing

            -- let hss = headDef "HEAD" [ x | StringLike x <- snd (splitOpts [] syn) ]
            h <- gitRevParseThrow hss
            r <- lift $ readCommitChainHPSQ req Nothing h dontHandle

            for_ (HPSQ.toList r) $ \(k,_,_) -> do
              liftIO $ print $ pretty  k

        entry $ bindMatch "test:git:log:list" $ nil_ $ \syn -> do
          let (_, argz) = splitOpts [] syn

          let fs = [fn | StringLike fn <- argz]

          for_ fs $ \f -> do
            lbs <- liftIO$ LBS.readFile f
            runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \h s _ -> do
              liftIO $ print $ "object" <+> pretty h <+> pretty s


        entry $ bindMatch "reflog:index:search" $ nil_ $ \syn -> lift $ connectedDo do

          let (_, argz) = splitOpts [] syn

          hash <- headMay [ x | GitHashLike x <- argz ] & orThrowUser "need sha1"

          idx <- openIndex

          answ <- indexEntryLookup idx hash

          for_ answ $ \bs -> do
            let a = coerce (BS.take 32 bs) :: HashRef
            liftIO $ print $ pretty a

        entry $ bindMatch "test:segment:dump" $ nil_ $ \syn -> lift do
          sto <- getStorage
          let (_, argz) = splitOpts [] syn
          tree <- headMay [ x | HashLike x <- argz ] & orThrowUser "tree hash required"

          lbs <- runExceptT (getTreeContents sto tree) >>= orThrowPassIO

          runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \h s obs -> do
            let (t, body) = LBS.splitAt 1 obs

            let tp = fromStringMay @(Short GitObjectType) (LBS8.unpack t)
                         & maybe Blob coerce

            liftIO $ print $ pretty h <+> fill 8 (viaShow tp) <+> pretty s

        entry $ bindMatch "test:segment:dump:pack" $ nil_ $ \syn -> lift do
          let (opts, argz) = splitOpts [ ("--dir",1)] syn
          let dir = headDef  "." [ p | ListVal [StringLike "--dir", StringLike p] <- opts ]
          let trees = [ x | HashLike x <- argz ]
          for_ trees $ \tree -> do
            writeAsGitPack dir tree

        entry $ bindMatch "reflog:index:list:count" $ nil_ $ const $ lift $ connectedDo do
          idx <- openIndex
          num_ <- newIORef 0
          enumEntries idx $ \_ -> void $ atomicModifyIORef num_ (\x -> (succ x, x))
          readIORef num_ >>= liftIO . print . pretty

        entry $ bindMatch "reflog:index:list" $ nil_ $ const $ lift $ connectedDo do
          files <- listObjectIndexFiles
          for_ files  $ \(ifn,_) -> do
            lbs <- liftIO $ LBS.readFile ifn

            void $ runConsumeLBS lbs $ readSections $ \s ss -> do

              let (sha1, blake) = LBS.splitAt 20 ss
                                      & over _1 (coerce @_ @GitHash . LBS.toStrict)
                                      & over _2 (coerce @_ @HashRef . LBS.toStrict)

              liftIO $ hPrint stdout $ pretty sha1 <+> pretty blake

        entry $ bindMatch "reflog:index:check" $ nil_ $ \case
          [ StringLike fn ] -> lift do
            bs <- liftIO $ mmapFileByteString fn Nothing

            unless (validateSorted bs) do
              error "malformed"

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "reflog:index:compact" $ nil_ $ \_ -> lift $ connectedDo do
          size <- getIndexBlockSize
          compactIndex size

        entry $ bindMatch "reflog:index:path" $ nil_ $ const $ lift $ connectedDo do
          indexPath >>= liftIO . print . pretty

          -- let entriesListOf lbs = S.toList_ $ runConsumeLBS lbs $ readSections $ \s ss -> do
        entry $ bindMatch "reflog:index:files" $ nil_ $ \syn -> lift $ connectedDo do
          files <- listObjectIndexFiles
          cur <- pwd
          for_ files $ \(f',s) -> do
            let f = makeRelative cur f'
            liftIO $ print $ fill 10 (pretty s) <+> pretty f

        entry $ bindMatch "reflog:index:list:tx" $ nil_ $ const $ lift $ connectedDo do
          r <- newIORef  ( mempty :: HashSet HashRef )
          index <- openIndex
          enumEntries index $ \bs -> do
            let h =  coerce $ BS.take 32 $ BS.drop 20 bs
            -- here <- readIORef r <&> HS.member h
            -- unless here do
            atomicModifyIORef' r ( \x -> (HS.insert h x, ()))
          z <- readIORef r <&> HS.toList
          for_ z $ \h ->do
            liftIO $ print $  pretty h

        entry $ bindMatch "reflog:index:build" $ nil_ $ const $ lift $ connectedDo do
          updateReflogIndex

        entry $ bindMatch "test:reflog:index:lookup" $ nil_ \case
          [ GitHashLike h ] -> lift do
            idx <- openIndex
            what <- indexEntryLookup idx h >>= orThrowUser "object not found"
            liftIO $ print $ pretty ( coerce @_ @HashRef what )

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "git:commit:list:objects:new" $ nil_ $ \case
          [ StringLike what ] -> lift $ connectedDo do

            commit  <- gitRevParseThrow what

            updateReflogIndex

            idx <- openIndex

            -- let req h = lift $ indexEntryLookup idx h <&> isNothing

            flip runContT pure do
              cap <- liftIO getNumCapabilities
              gitCatBatchQ <- contWorkerPool cap do
                che <- ContT withGitCat
                pure $ gitReadObjectMaybe che

              new_ <- newTQueueIO
              c1 <- newCacheFixedHPSQ 1000

              (_,self) <- lift $ gitCatBatchQ commit
                             >>= orThrow (GitReadError (show $ pretty commit))

              tree <- gitReadCommitTree self

              -- читаем только те объекты, которые не в индексе
              hashes <- gitReadTreeObjectsOnly commit
                            <&> ([commit,tree]<>)
                            >>= lift . indexFilterNewObjects idx . HS.fromList
                            --
              atomically $ mapM_ (writeTQueue new_) hashes
              atomically (STM.flushTQueue new_) >>= liftIO . print . pretty . length

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "git:list:objects:new" $ nil_ $ \syn -> lift $ connectedDo do
          let (opts,argz) = splitOpts [] syn

          let what = headDef "HEAD" [ x | StringLike x <- argz ]
          h0  <- gitRevParseThrow what

          no_ <- newTVarIO 0

          void $ flip runContT pure do

            lift updateReflogIndex

            idx <- lift openIndex
            let req h = lift $ indexEntryLookup idx h <&> isNothing

            (t1,r) <- timeItT (lift $ readCommitChainHPSQ req Nothing h0 dontHandle)

            let s = HPSQ.size r
            debug $ pretty s <+> "new commits read at" <+> pretty (realToFrac @_ @(Fixed E3) t1)

            cap <- liftIO getNumCapabilities
            gitCatBatchQ <- contWorkerPool cap do
              che <- ContT withGitCat
              pure $ gitReadObjectMaybe che

            uniq_ <- newTVarIO mempty
            -- c1 <- newCacheFixedHPSQ 1000
            (t3, _) <- timeItT $ lift $ forConcurrently_ (HPSQ.toList r) $ \(commit,_,_) -> do

              (_,self) <- gitCatBatchQ commit
                             >>= orThrow (GitReadError (show $ pretty commit))

              tree <- gitReadCommitTree self

              -- читаем только те объекты, которые не в индексе
              gitReadTreeObjectsOnly commit
                  <&> ([commit,tree]<>)
                  >>= \hs -> atomically (for_ hs (modifyTVar uniq_ . HS.insert))

            debug $ "read new objects" <+> pretty (realToFrac @_ @(Fixed E2) t3)

            (t4,new) <- lift $ timeItT $ readTVarIO uniq_ >>= indexFilterNewObjects idx

            liftIO $ for_ new $ \n -> do
               print $ pretty n
            -- notice $ pretty (length new) <+> "new objects" <+> "at" <+> pretty (realToFrac @_ @(Fixed E2) t4)

        entry $ bindMatch "reflog:tx:list" $ nil_ $ \syn -> lift $ connectedDo do

          waitRepo Nothing =<< getGitRepoKeyThrow

          let (opts, _) = splitOpts [ ("--checkpoints",0)
                                    , ("--segments",0)
                                    ] syn

          let cpOnly   = or [ True | ListVal [StringLike "--checkpoints"] <- opts ]
          let sOnly    = or [ True | ListVal [StringLike "--segments"] <- opts ]

          hxs <- txListAll Nothing

          liftIO $ forM_ hxs $ \(h,tx) -> do
            let decoded = case tx of
                  TxSegment x   | not cpOnly ->
                    Just ("S" <+> fill 44 (pretty h) <+> fill 44 (pretty x))

                  TxCheckpoint n x | not sOnly ->
                    Just ("C" <+> fill 44 (pretty h) <+> pretty x <+> fill 8 (pretty n))

                  _ -> Nothing

            forM_ decoded print

        entry $ bindMatch "reflog:refs" $ nil_ $ \syn -> lift $ connectedDo do

          waitRepo Nothing =<< getGitRepoKeyThrow

          rrefs <- importedRefs
          for_  rrefs $ \(r,h) -> do
            liftIO $ print $ fill 20  (pretty h) <+> pretty r

        entry $ bindMatch "reflog:refs:raw" $ nil_ $ \syn -> lift $ connectedDo do

          waitRepo Nothing =<< getGitRepoKeyThrow

          refsFiles >>= readRefsRaw >>= liftIO . mapM_ (print . pretty)

        entry $ bindMatch "repo:wait" $ nil_ $ \syn -> lift $ connectedDo do
          let (_,argz) = splitOpts [] syn

          let t = headMay [ realToFrac x | LitIntVal x <- argz ]

          waitRepo t =<< getGitRepoKeyThrow

          getRepoManifest >>= liftIO . print . pretty . mkForm "manifest" . coerce

        entry $ bindMatch "repo:remotes" $ nil_ $ \syn -> lift do

          remotes <- listRemotes

          liftIO $ for_ remotes $ \(r,k) -> do
            print $ fill 44 (pretty (AsBase58 k)) <+> pretty r

        entry $ bindMatch "reflog:imported" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepoKeyThrow syn >>= setGitRepoKey
          p <- importedCheckpoint
          liftIO $ print $ pretty p

        entry $ bindMatch "reflog:import" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepoKeyThrow syn >>= setGitRepoKey
          importGitRefLog

        brief "shows repo manifest" $
          entry $ bindMatch "repo:manifest" $ nil_ $ \syn -> lift $ connectedDo do
            resolveRepoKeyThrow syn >>= setGitRepoKey
            manifest <- Repo.getRepoManifest
            liftIO $ print $ pretty $ mkForm "manifest" (coerce manifest)

        brief "shows repo reflog" $
          entry $ bindMatch "repo:reflog" $ nil_ $ const $ lift $ connectedDo do
            repo <- Repo.getRepoManifest

            reflog <- getRefLog repo  & orThrow GitRepoManifestMalformed

            liftIO $ print $ pretty (AsBase58 reflog)

        entry $ bindMatch "repo:credentials" $ nil_ $ \syn -> lift $ connectedDo $ do
          resolveRepoKeyThrow syn >>= setGitRepoKey
          waitRepo (Just 10) =<< getGitRepoKeyThrow

          (p,_) <- getRepoRefLogCredentials
          liftIO $ print $ pretty $ mkForm @C "matched" [mkSym (show $ pretty ( AsBase58 p) )]

        entry $ bindMatch "repo:init" $ nil_ $ \syn -> lift $ connectedDo do
            Repo.initRepo syn

        entry $ bindMatch "repo:relay-only" $ nil_ $ \case
          [ SignPubKeyLike repo ] -> lift $ connectedDo do
            setGitRepoKey repo

            waitRepo (Just 2) =<< getGitRepoKeyThrow

          _ -> throwIO (BadFormException @C nil)

        exportEntries "reflog:"



