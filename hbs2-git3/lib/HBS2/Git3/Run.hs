module HBS2.Git3.Run where

import HBS2.Git3.Prelude
import HBS2.Git3.State.Index
import HBS2.Git3.Git.Pack

import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Storage.Operations.Missed

-- move to Data.Config.Suckless.Script.Filea sepatate library
import HBS2.Data.Log.Structured

import HBS2.CLI.Run.Internal.Merkle (getTreeContents)
import HBS2.CLI.Run.RefLog (getCredentialsForReflog,mkRefLogUpdateFrom)

import HBS2.System.Dir

import HBS2.Git3.Types
import HBS2.Git3.Config.Local
import HBS2.Git3.Git
import HBS2.Git3.Export
import HBS2.Git3.Import
import HBS2.Git3.State.RefLog

import Data.Config.Suckless.Script
import Data.Config.Suckless.Script.File

import Codec.Compression.Zstd.Streaming qualified as ZstdS
import Codec.Compression.Zstd.Streaming (Result(..))
import Codec.Compression.Zstd.Lazy qualified as ZstdL

import Codec.Compression.Zlib qualified as Zlib

import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)

import Data.Maybe
import Data.List qualified as L
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Builder as Builder
import Network.ByteOrder qualified as N
import Text.InterpolatedString.Perl6 (qc)
import Data.Set qualified as Set
import Data.HashSet qualified as HS
import Data.HashSet (HashSet(..))
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict (HashMap(..))
import Data.Word
import Data.Fixed
import Data.Either
import Data.Ord (comparing)
import Data.Generics.Labels
import Data.Generics.Product
import Lens.Micro.Platform

import Streaming.Prelude qualified as S

import System.Exit qualified as Q
import System.Environment qualified as E
import System.Process.Typed
import Control.Monad.State qualified as State
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Control.Concurrent.STM qualified as STM
import System.Directory (setCurrentDirectory)
import System.Random hiding (next)
import System.IO.MMap (mmapFileByteString)
import System.IO qualified as IO
import System.IO (hPrint,hPutStrLn,hPutStr)
import System.IO.Temp as Temp
import System.TimeIt

import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Search qualified as MV

import UnliftIO.Concurrent
import UnliftIO.IO.File qualified as UIO

import Control.Monad.ST
import Data.BloomFilter qualified as Bloom
import Data.BloomFilter.Mutable qualified as MBloom

import Crypto.Hash qualified as C


theDict :: forall m . ( HBS2GitPerks m
                      -- , HasClientAPI PeerAPI UNIX m
                      -- , HasStorage m
                      -- , HasGitRemoteKey m
                      -- , HasStateDB m
                      ) => Dict C (Git3 m)
theDict = do
  makeDict @C do
    -- TODO: write-man-entries
    myHelpEntry
    entry $ bindValue "best" (mkInt 22)
    internalEntries

  where

    myHelpEntry = do
        entry $ bindMatch "--help" $ nil_ $ \case
          HelpEntryBound what -> do
            helpEntry what
            quit

          _ -> helpList False Nothing >> quit

        entry $ bindMatch "compression" $ nil_ $ \case
          [ LitIntVal n ] -> lift do
            setCompressionLevel (fromIntegral n)

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "segment" $ nil_ $ \case
          [ LitIntVal n ] -> lift do
            setPackedSegmedSize (fromIntegral n)

          _ -> throwIO (BadFormException @C nil)

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

        entry $ bindMatch "reflog" $ nil_ $ \case
          [ SignPubKeyLike what ] -> do
            debug $ "set reflog" <+> pretty (AsBase58 what)
            lift $ setGitRemoteKey what

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "debug" $ nil_ $ const do
         setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

        entry $ bindMatch "test:git:normalize-ref" $ nil_  \case
          [ StringLike s ] -> display $ mkStr @C (show $ pretty $ gitNormaliseRef (fromString s))
          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:hbs2:peer:poke" $ nil_ $ \syn -> do
          peer <- getClientAPI @PeerAPI @UNIX
          r    <- callRpcWaitRetry @RpcPoke (TimeoutSec 0.5) 2 peer () >>= orThrowUser "hbs2-peer not found"
          notice $ pretty r

        entry $ bindMatch "test:git:hash:blob" $ nil_ $ const $ liftIO do
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

        entry $ bindMatch "test:git:log:cat" $ nil_ $ \syn -> lift do

          let (opts, argz) = splitOpts [("--git",0),("--packed",0),("--import",1)] syn

          let git    = or [ True | ListVal [StringLike "--git"] <- opts ]
          let packed = or [ True | ListVal [StringLike "--packed"] <- opts ]

          (gh, fn) <- case argz of
                       [ GitHashLike a, StringLike b ] -> do
                        pure (a, b)

                       _ -> throwIO (BadFormException @C nil)


          src <- liftIO$ LBS.readFile fn

          what <- S.toList_ $ runConsumeLBS (ZstdL.decompress src) $ readLogFileLBS () $ \h s src -> do
            let (t,rest) = LBS.splitAt 1 src

            Short tp <- fromStringMay @(Short GitObjectType) (LBS8.unpack t)
                          & orThrowUser "Invalid object type"

            when ( h == gh ) $ lift $ S.yield (tp,rest)

          liftIO $ maybe1 (listToMaybe what) (Q.exitFailure) $ \(t,s) -> do

            let raw = if not git then s else do
                      let signature = [qc|{pretty t} {pretty $ LBS.length s}|] <> "\x00" :: LBS8.ByteString
                      signature <> s

            let result = if not packed then raw else do
                            let params = Zlib.defaultCompressParams { Zlib.compressMethod = Zlib.deflateMethod }
                            Zlib.compressWith params  raw

            LBS.hPutStr stdout result

        entry $ bindMatch "test:git:log:list" $ nil_ $ \syn -> do
          let (_, argz) = splitOpts [] syn

          let fs = [fn | StringLike fn <- argz]

          for_ fs $ \f -> do
            lbs <- liftIO$ LBS.readFile f
            runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \h s _ -> do
              liftIO $ print $ "object" <+> pretty h <+> pretty s


        entry $ bindMatch "test:git:log:list:refs" $ nil_ $ \syn -> do
          let (_, argz) = splitOpts [] syn

          let fs = [fn | StringLike fn <- argz]

          for_ fs $ \f -> do
            lbs <- liftIO$ LBS.readFile f
            runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \h s lbs -> do
              let (sign,rest) = LBS.splitAt 1 lbs

              let tp = fromStringMay @(Short SegmentObjectType) (LBS8.unpack sign)

              case tp of
                Just (Short RefObject) -> do
                  liftIO $ LBS.hPutStr stdout rest

                _ -> pure ()

        entry $ bindMatch "test:git:log:index:flat:dump" $ nil_ $ \syn -> lift do
          let (_, argz) = splitOpts [] syn
          fname <- headMay [ x | StringLike x <- argz] & orThrowUser  "no file"

          bs <- liftIO $ mmapFileByteString fname Nothing

          runConsumeBS bs $ flip fix 0 \go n -> do
            done <- noBytesLeft
            if done then pure ()
              else do
                ssize  <- readBytesMaybe 4
                               >>= orThrow SomeReadLogError
                               <&> fromIntegral . N.word32 . LBS.toStrict

                hash   <- readBytesMaybe 20
                             >>= orThrow SomeReadLogError
                             <&> GitHash . LBS.toStrict

                liftIO $ print $ pretty hash <+> pretty ssize
                go (succ n)

        entry $ bindMatch "test:reflog:index:search:binary:test:2" $ nil_ $ const $ lift do
          r <- newTQueueIO
          idx <- openIndex
          enumEntries idx $ \e -> do
            let ha = GitHash $ coerce $ BS.take 20 e
            atomically $ writeTQueue r ha

          hashes <- atomically $ STM.flushTQueue r
          liftIO $ print (length hashes)

          mmaped <- listObjectIndexFiles <&> fmap fst
                       >>= \xs -> for xs $ \x -> liftIO $  mmapFileByteString x Nothing

          already_ <- newTVarIO (mempty :: HashSet GitHash)

          for_ hashes $ \h -> do
            for_ mmaped $ \bs -> do
              here <- readTVarIO already_ <&> HS.member h
              unless here do
                found <- binarySearchBS 56 ( BS.take 20 . BS.drop 4 ) (coerce h) bs
                when (isJust found) do
                  atomically $ modifyTVar already_ (HS.insert h)
                  notice $ pretty h <+> "True"

        entry $ bindMatch "test:reflog:index:search:binary:test" $ nil_ $ const $ lift do

            files <- listObjectIndexFiles

            forConcurrently_ files $ \(fn,_) -> do

              lbs <- liftIO $ LBS.readFile fn

              hashes <- S.toList_ $  runConsumeLBS lbs $ flip fix 0 \go n -> do
                done <- consumed
                if done then pure ()
                  else do
                    ssize  <- readBytesMaybe 4
                                     >>= orThrow SomeReadLogError
                                   <&> fromIntegral . N.word32 . LBS.toStrict

                    hash   <- readBytesMaybe 20
                                  >>= orThrow SomeReadLogError
                                  <&> GitHash . LBS.toStrict

                    void $ readBytesMaybe 32

                    lift $ S.yield hash
                    go (succ n)

              file <- liftIO $ mmapFileByteString fn Nothing

              for_ hashes $ \h -> do
               -- found <- binSearchBS 24 (BS.take 20 . BS.drop 4) ( show . pretty . GitHash ) (coerce h) file
                found <- liftIO $ binarySearchBS 56 (BS.take 20 . BS.drop 4) (coerce h) file
                liftIO $ notice $ pretty h <+> pretty (isJust found)

        entry $ bindMatch "reflog:index:search" $ nil_ $ \syn -> lift do

          let (_, argz) = splitOpts [] syn

          hash <- headMay [ x | GitHashLike x <- argz ] & orThrowUser "need sha1"

          idx <- openIndex

          answ <- indexEntryLookup idx hash

          for_ answ $ \bs -> do
            let a = coerce (BS.take 32 bs) :: HashRef
            liftIO $ print $ pretty a

        entry $ bindMatch "test:git:log:index:flat:search:vector:test" $ nil_ $ \case
          [ StringLike fn ] -> do

            lbs <- liftIO $ LBS.readFile fn

            hashes <- S.toList_ $ runConsumeLBS lbs $ flip fix 0 \go n -> do
              done <- consumed
              if done then pure ()
                else do
                  shit <- LBS.toStrict <$> (readBytesMaybe 24 >>= orThrow SomeReadLogError)
                  lift $ S.yield shit
                  go (succ n)

            let wat = Vector.fromList hashes
            vec <- liftIO $ Vector.thaw wat

            let cmp bs1 bs2 = compare (BS.take 20 $ BS.drop 4 bs1) (BS.take 20 $ BS.drop 4 bs2)

            for_ hashes $ \h -> do
              found <- liftIO $ MV.binarySearchBy cmp vec h
              liftIO $ print $ pretty (GitHash h) <+> pretty found

          _ -> throwIO (BadFormException @C nil)

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

        entry $ bindMatch "test:segment:import:loose" $ nil_ $ \syn -> lift $ connectedDo do
          let (opts, argz) = splitOpts [] syn
          let logs = [ x| StringLike x <- argz ]

          d <- findGitDir >>= orThrowUser "not a git directory"

          sto <- getStorage

          flip runContT pure do

            gitCatCheck <- contWorkerPool 8 do
              che <- ContT withGitCatCheck
              pure $ gitCheckObjectFromHandle che

            let trees = [ x | HashLike x <- argz ]

            lift $ for_ trees $ \tree -> do

              notice $ pretty "running" <+> pretty tree

              lbs <- runExceptT (getTreeContents sto tree) >>= orThrowPassIO

              runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \h s lbs -> do
                let (t, body) = LBS.splitAt 1 lbs

                let tp = fromStringMay @(Short GitObjectType) (LBS8.unpack t)
                             & maybe Blob coerce

                here <- lift $ isJust <$> gitCatCheck h

                let gitHash = show $ pretty h
                let (prefix,name) = L.splitAt 2 gitHash
                let path = joinPath [d, "objects", prefix, name]

                let signature = [qc|{pretty tp} {pretty $ LBS.length body}|] <> "\x00" :: LBS8.ByteString
                let o = signature <> body

                unless here $ liftIO do

                  notice $ "FUCKING IMPORT OBJECT" <+> pretty here <+> pretty h <+> pretty tp

                  touch path

                  debug $ pretty tp <+> pretty s <+> pretty h <+> pretty path

                  let params = Zlib.defaultCompressParams { Zlib.compressMethod = Zlib.deflateMethod }
                  UIO.withBinaryFileAtomic path WriteMode $ \fh -> do
                    let contents = Zlib.compressWith params  o
                    LBS.hPutStr fh contents


        entry $ bindMatch "reflog:index:count:missed" $ nil_ $ const $ lift $ flip runContT pure do

           hashes <- gitRunCommand [qc|git rev-list --all --objects|]
                        >>= orThrowPassIO
                        <&> LBS8.lines
                        <&> mapMaybe (fromStringMay @GitHash . LBS8.unpack)

           for_ hashes $ \h -> do
            liftIO $ print $ pretty h

          -- git <- findGitDir >>= orThrowUser ".git directory not found"

          -- ofiles <- S.toList_ $ glob ["**/*"] ["info/**", "pack/**"] (git </> "objects") $ \fn -> do
          --   S.yield fn >> pure True

          -- idxFiles <- S.toList_ $ glob ["**/*.idx"] [] (git </> "objects/pack") $ \fn -> do
          --   S.yield fn >> pure True

          -- liftIO $ for_ ofiles $ \f -> do
          --   print f

          -- liftIO $ for_ idxFiles $ \f -> flip runContT pure do
          --   p <- ContT withGitShowIndex
          --   -- void $ ContT $ bracket (pure p) (hClose . getStdin)
          --   liftIO do
          --     LBS.hPutStr (getStdin p) =<< LBS.readFile f
          --     hFlush (getStdin p)
          --     wtf <- IO.hGetContents (getStdout p) <&> lines
          --     for_ wtf $ IO.putStrLn

            -- _ <- gitRunCommand [qc|git show-index|]
            -- print f

          -- gitCatCheck <- contWorkerPool 4 do
          --   che <- ContT withGitCatCheck
          --   pure $ gitCheckObjectFromHandle che

          -- idx <- lift openIndex

          -- missed_ <- newTVarIO ( mempty :: HashSet GitHash )
          -- lift $ enumEntries idx $ \bs -> do
          --   let gh = GitHash (coerce (BS.take 20 bs))
          --   here <- gitCatCheck gh
          --   unless (isJust here) do
          --     atomically $ modifyTVar missed_ (HS.insert gh)

          -- missed <- readTVarIO missed_ <&> HS.size

          -- liftIO $ print $ "missed" <+> pretty missed

        entry $ bindMatch "reflog:index:list:fast" $ nil_ $ const $ lift do
          files <- listObjectIndexFiles
          forConcurrently_ files  $ \(f,_) -> do
            bs <- liftIO $ mmapFileByteString f Nothing
            for_ (toSectionList bs) $ \segment -> do
              let (sha1,blake) = BS.splitAt 20 segment
                                      & over _1 (coerce @_ @GitHash)
                                      & over _2 (coerce @_ @HashRef)

              notice $ pretty sha1 <+> pretty blake


        entry $ bindMatch "reflog:index:list:count" $ nil_ $ const $ lift do
          idx <- openIndex
          num_ <- newIORef 0
          enumEntries idx $ \_ -> void $ atomicModifyIORef num_ (\x -> (succ x, x))
          readIORef num_ >>= liftIO . print . pretty

        entry $ bindMatch "reflog:index:list" $ nil_ $ const $ lift do
          files <- listObjectIndexFiles
          for_ files  $ \(ifn,_) -> do
            lbs <- liftIO $ LBS.readFile ifn

            void $ runConsumeLBS lbs $ readSections $ \s ss -> do

              let (sha1, blake) = LBS.splitAt 20 ss
                                      & over _1 (coerce @_ @GitHash . LBS.toStrict)
                                      & over _2 (coerce @_ @HashRef . LBS.toStrict)

              liftIO $ hPrint stdout $ pretty sha1 <+> pretty blake

        entry $ bindMatch "test:reflog:file:check" $ nil_ $ \case
          [ StringLike fn ] -> lift do
            bs <- liftIO $ mmapFileByteString fn Nothing

            unless (validateSorted bs) do
              error "malformed"

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "reflog:index:compact" $ nil_ $ \_ -> lift do
          size <- getIndexBlockSize
          compactIndex size

        entry $ bindMatch "reflog:index:path" $ nil_ $ const $ lift do
          indexPath >>= liftIO . print . pretty

          -- let entriesListOf lbs = S.toList_ $ runConsumeLBS lbs $ readSections $ \s ss -> do
        entry $ bindMatch "reflog:index:files" $ nil_ $ \syn -> lift do
          files <- listObjectIndexFiles
          cur <- pwd
          for_ files $ \(f',s) -> do
            let f = makeRelative cur f'
            liftIO $ print $ fill 10 (pretty s) <+> pretty f

        entry $ bindMatch "reflog:index:list:tx" $ nil_ $ const $ lift do
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
          [ StringLike what ] -> lift do

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

        entry $ bindMatch "git:list:objects:new" $ nil_ $ \syn -> lift do
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
          rrefs <- importedRefs
          liftIO $ print $ pretty rrefs


        entry $ bindMatch "reflog:imported" $ nil_ $ \syn -> lift $ connectedDo do
          p <- importedCheckpoint
          liftIO $ print $ pretty p

        entry $ bindMatch "reflog:import" $ nil_ $ \syn -> lift $ connectedDo do
          importGitRefLog

        exportEntries "reflog:"

