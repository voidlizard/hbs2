module HBS2.Git3.Run where

import HBS2.Git3.Prelude
import HBS2.Data.Log.Structured

import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.Data.Detect hiding (Blob)
import HBS2.System.Dir

import HBS2.Git3.Git
import HBS2.Git3.Export
import HBS2.Git3.Import
import HBS2.Git3.State
import HBS2.Git3.Repo qualified as Repo
import HBS2.Git3.Repo
import HBS2.Git3.Logger
import HBS2.Git3.Man
import HBS2.Net.Auth.GroupKeySymm

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

    myEntries = hidePrefixes ["test", "debug", "segment", "reflog:index", "tss"] do
        entry $ bindMatch "--help" $ nil_ $ \case
          HelpEntryBound what -> do
            helpEntry what
            quit

          [ StringLike x ] -> helpList True (Just x) >> quit

          _ -> helpList True Nothing >> quit

        entry $ bindAlias "help" "--help"

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

        brief "sets packed segment size in bytes"
         $ entry $ bindMatch "segment" $ nil_ $ \case
            [ LitIntVal n ] -> lift do
              setPackedSegmedSize (fromIntegral n)

            _ -> throwIO (BadFormException @C nil)

        brief "silent mode"
         $ entry $ bindMatch "quiet" $ nil_ $ const $ lift do
            silence

        hidden $
         entry $ bindMatch "index-block-size" $ nil_ \case
          [ LitIntVal size ]-> lift do
            setIndexBlockSize (fromIntegral size)

          _ -> throwIO (BadFormException @C nil)

        brief "list current git objects"
         $ entry $ bindMatch "git:tree:ls" $ nil_ $ const do
          r <- gitReadTree "HEAD"
          for_ r $ \GitTreeEntry{..} -> do
            liftIO $ print $  fill 40 (pretty gitEntryHash)
                          <+> pretty gitEntryType
                          <+> pretty gitEntrySize
                          <+> pretty gitEntryName

        brief "turn debug output on"
         $ entry $ bindMatch "debug" $ nil_ $ const do
            setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

        -- hidden do

        entry $ bindMatch "test:git:normalize-ref" $ nil_  \case
          [ StringLike s ] -> display $ mkStr @C (show $ pretty $ gitNormaliseRef (fromString s))
          _ -> throwIO (BadFormException @C nil)

        brief "checks if hbs2-peer available"
          $ entry $ bindMatch "hbs2:peer:poke" $ nil_ $ \syn -> do
            peer <- getClientAPI @PeerAPI @UNIX
            r    <- callRpcWaitRetry @RpcPoke (TimeoutSec 0.5) 2 peer () >>= orThrowUser "hbs2-peer not found"
            notice $ pretty r

        hidden do
          entry $ bindMatch "git:hash:blob" $ nil_ $ const $ liftIO do
            co <- LBS.hGetContents stdin
            print $ pretty $ gitHashBlobPure co

        hidden do
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


        hidden $
          entry $ bindMatch "reflog:index:search" $ nil_ $ \syn -> lift $ connectedDo do

            let (_, argz) = splitOpts [] syn

            hash <- case argz of
                      [ x@StringLike{}, GitHashLike h ] -> do
                        resolveRepoKeyThrow [x] >>= setGitRepoKey
                        waitRepo Nothing =<< getGitRepoKeyThrow
                        pure h

                      _ -> throwIO $ BadFormException @C nil

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

        brief "prints indexed object count for repo" $
          entry $ bindMatch "repo:index:count" $ nil_ $ \syn -> lift $ connectedDo do

            resolveRepo syn

            idx <- openIndex
            num_ <- newIORef 0
            enumEntries idx $ \_ -> void $ atomicModifyIORef num_ (\x -> (succ x, x))
            readIORef num_ >>= liftIO . print . pretty

        brief "lists indexed objects for repo" $
          entry $ bindMatch "repo:index:list" $ nil_ $ \syn -> lift $ connectedDo do
            resolveRepo syn
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

        entry $ bindMatch "repo:index:compact" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn
          size <- getIndexBlockSize
          compactIndex size

        entry $ bindMatch "repo:index:path" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn
          indexPath >>= liftIO . print . pretty

          -- let entriesListOf lbs = S.toList_ $ runConsumeLBS lbs $ readSections $ \s ss -> do
        entry $ bindMatch "repo:index:files" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn
          files <- listObjectIndexFiles
          cur <- pwd
          for_ files $ \(f',s) -> do
            let f = makeRelative cur f'
            liftIO $ print $ fill 10 (pretty s) <+> pretty f

        entry $ bindMatch "repo:index:list:tx" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn
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

        entry $ bindMatch "repo:index:build" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn
          updateReflogIndex

        entry $ bindMatch "test:reflog:index:lookup" $ nil_ \case
          [ GitHashLike h ] -> lift do
            idx <- openIndex
            what <- indexEntryLookup idx h >>= orThrowUser "object not found"
            liftIO $ print $ pretty ( coerce @_ @HashRef what )

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "git:commit:list:objects:new" $ nil_ $ \case
          [ repo, StringLike what ] -> lift $ connectedDo do

            resolveRepo [repo]

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

        manGitListObjectsNew $
          entry $ bindMatch "git:list:objects:new" $ nil_ $ \syn -> lift $ connectedDo do

            resolveRepo syn

            let (opts,argz) = splitOpts [("-r", 1)] (tail syn)

            let what = headDef "HEAD" [ x | MatchOption "-r" (StringLike x) <- opts ]

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


        entry $ bindMatch "repo:tx:list:imported" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn

          txImported >>= liftIO . print . vcat . fmap pretty . HS.toList

          let (opts, argz) = splitOpts [ ("--checkpoints",0)
                                    , ("--segments",0)
                                    ] syn

          let cpOnly   = or [ True | ListVal [StringLike "--checkpoints"] <- opts ]
          let sOnly    = or [ True | ListVal [StringLike "--segments"] <- opts ]

          resolveRepoKeyThrow argz >>= setGitRepoKey
          waitRepo Nothing =<< getGitRepoKeyThrow

          hxs <- txListAll Nothing

          liftIO $ forM_ hxs $ \(h,tx) -> do
            let decoded = case tx of
                  TxSegment x   | not cpOnly ->
                    Just ("S" <+> fill 44 (pretty h) <+> fill 44 (pretty x))

                  TxCheckpoint n x | not sOnly ->
                    Just ("C" <+> fill 44 (pretty h) <+> pretty x <+> fill 8 (pretty n))

                  _ -> Nothing

            forM_ decoded print


        entry $ bindMatch "repo:tx:list" $ nil_ $ \syn -> lift $ connectedDo do

          resolveRepo syn

          let (opts, argz) = splitOpts [ ("--checkpoints",0)
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

        entry $ bindMatch "repo:refs" $ nil_ $ \syn -> lift $ connectedDo do

          resolveRepo syn

          rrefs <- importedRefs
          for_  rrefs $ \(r,h) -> do
            liftIO $ print $ fill 20  (pretty h) <+> pretty r

        entry $ bindMatch "repo:refs:raw" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn
          refsFiles >>= readRefsRaw >>= liftIO . mapM_ (print . pretty)

        entry $ bindMatch "repo:wait" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn
          getRepoManifest >>= liftIO . print . pretty . mkForm "manifest" . coerce

        manRemotes $ entry $ bindAlias "remotes" "repo:remotes"

        manRemotes $
          entry $ bindMatch "repo:remotes" $ nil_ $ const $ lift do
          remotes <- listRemotes
          liftIO $ for_ remotes $ \(r,k) -> do
            print $ fill 44 (pretty (AsBase58 k)) <+> pretty r

        entry $ bindMatch "repo:imported" $ nil_ $ \syn -> lift $ connectedDo do
          resolveRepo syn
          p <- importedCheckpoint
          liftIO $ print $ pretty p

        hidden do
          entry $ bindMatch "repo:import" $ nil_ $ \syn -> lift $ connectedDo do
            resolveRepo syn
            importGitRefLog

        brief "shows repo manifest" $
          entry $ bindMatch "repo:manifest" $ nil_ $ \syn -> lift $ connectedDo do
            resolveRepo syn
            manifest <- Repo.getRepoManifest
            liftIO $ print $ pretty $ mkForm "manifest" (coerce manifest)

        brief "shows repo reflog" $
          entry $ bindMatch "repo:reflog" $ nil_ $ \syn -> lift $ connectedDo do
            resolveRepo syn
            repo <- Repo.getRepoManifest
            reflog <- getRefLog repo  & orThrow GitRepoManifestMalformed
            liftIO $ print $ pretty (AsBase58 reflog)

        entry $ bindMatch "repo:credentials" $ nil_ $ \syn -> lift $ connectedDo $ do
          resolveRepo syn
          (p,_) <- getRepoRefLogCredentials
          liftIO $ print $ pretty $ mkForm @C "matched" [mkSym (show $ pretty ( AsBase58 p) )]

        entry $ bindMatch "repo:gk" $ nil_ $ \syn -> lift $ connectedDo $ do
          resolveRepoKeyThrow syn >>= setGitRepoKey
          gk' <- getGK
          for_ gk' $ \(gkh, _) -> do
            liftIO $ print $ pretty $ mkForm @C "gk" [ mkSym (show $ pretty gkh) ]

        brief "updates group key for repo" $
          args [arg "string" "repo", arg "hash" "group-key-hash" ]  $
          entry $ bindMatch "repo:gk:update" $ nil_ $ \case
            [ x@(StringLike{}), HashLike h ] -> lift $ connectedDo do
              repo <- resolveRepoKeyThrow [x]
              updateGroupKey repo h

            _ -> throwIO $ BadFormException @C nil

        entry $ bindMatch "repo:gk:add:extra:keys" $ nil_ $ \case
          ( x@(StringLike{}) : keyHashes ) -> lift $ connectedDo do
            repo <- resolveRepoKeyThrow [x]
            setGitRepoKey repo
            waitRepo (Just 10) =<< getGitRepoKeyThrow

            sto <- getStorage
            RepoManifest mf <- getRepoManifest

            hh <- for [ x | HashLike x <- keyHashes ] $ \k -> do
                     _ <- loadGroupKeyMaybe @HBS2Basic sto k >>= orThrow (GitRepoNoGroupKey k)
                     pure k

            updateRepoHead repo mf hh

          _ -> throwIO $ BadFormException @C nil


        -- FIXME: maybe-add-default-remote
        entry $ bindMatch "repo:head" $ nil_ $ \syn -> lift $ connectedDo $ do
          resolveRepo syn
          lww <- getRepoRefMaybe
          liftIO $ print $ pretty lww

        entry $ bindMatch "repo:gk:journal:import" $ nil_ $ \syn -> lift $ connectedDo $ do
          resolveRepo syn
          importGroupKeys

        entry $ bindMatch "repo:gk:journal:imported" $ nil_ $ \syn -> lift $ connectedDo $ do
          resolveRepo syn
          readGroupKeyFile <&> maybe nil (mkSym @C . show . pretty)
            >>= liftIO . print . pretty

        entry $ bindMatch "repo:gk:journal" $ nil_ $ \syn -> lift $ connectedDo $ do
          resolveRepo syn

          ref <- getGitRepoKeyThrow

          lwwAPI <- getClientAPI @LWWRefAPI @UNIX

          sto <- getStorage

          runMaybeT do

            LWWRef{..} <- liftIO (callRpcWaitMay @RpcLWWRefGet (TimeoutSec 1) lwwAPI (coerce ref))
                             >>= orThrow RpcTimeout
                            >>= toMPlus

            hrefs <- lift $ readLogThrow (getBlock sto) lwwValue

            journal <- headMay (tail hrefs) & toMPlus

            keys <- lift $ readLogThrow (getBlock sto) journal

            liftIO $ for_ keys $ \k -> do
              liftIO $ print $ pretty k

        manInit $
          entry $ bindMatch "repo:init" $ nil_ $ \syn -> lift $ connectedDo do
              Repo.initRepo syn

        manInit $ entry $
          bindAlias "init" "repo:init"

        manRepoRelayOnly $
          entry $ bindMatch "repo:relay-only" $ nil_ $ lift . relayOnlyRepo

        exportEntries "reflog:"


