{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language MultiWayIf #-}
{-# Language FunctionalDependencies #-}
{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedLabels #-}
module Main where

import HBS2.Git3.Prelude
import HBS2.Git3.State.Index

import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.CLI.Run.Internal.Merkle (getTreeContents)

-- move to a sepatate library
import HBS2.Data.Log.Structured


import HBS2.CLI.Run.Internal.Merkle (createTreeWithMetadata)
import HBS2.CLI.Run.RefLog (getCredentialsForReflog,mkRefLogUpdateFrom)

import HBS2.System.Dir

import HBS2.Git3.Types
import HBS2.Git3.State.Direct
import HBS2.Git3.Config.Local
import HBS2.Git3.Git

import Data.Config.Suckless.Script
import DBPipe.SQLite

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
import Data.Ord (comparing)
import Data.Generics.Labels
import Data.Generics.Product
import Lens.Micro.Platform

import Data.Heap (Entry(..))
import Data.Heap qualified as Heap

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
import System.IO (hPrint)
import System.IO.Temp as Temp
import System.TimeIt

import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Search qualified as MV

import UnliftIO.Concurrent
import UnliftIO.IO.File qualified as UIO

import Control.Monad.ST
import Data.BloomFilter qualified as Bloom
import Data.BloomFilter.Mutable qualified as MBloom

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}


class Cached cache k v | cache -> k, cache -> v where
  isCached :: forall m . MonadIO m => cache -> k -> m Bool
  cached   :: forall m . MonadIO m => cache -> k -> m v -> m v
  uncache  :: forall m . MonadIO m => cache -> k -> m ()


recover :: Git3 IO a -> Git3 IO a
recover m = fix \again -> do
  catch m $ \case
    Git3PeerNotConnected -> do

      soname <- detectRPC
                  `orDie` "can't locate hbs2-peer rpc"

      flip runContT pure do

        client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                    >>= orThrowUser ("can't connect to" <+> pretty soname)

        void $ ContT $ withAsync $ runMessagingUnix client

        peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
        refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
        lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peerAPI
                        , Endpoint @UNIX  refLogAPI
                        , Endpoint @UNIX  lwwAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        ref <- getGitRemoteKey >>= orThrowUser "remote ref not set"

        let sto = AnyStorage (StorageClient storageAPI)

        connected <- Git3Connected soname sto peerAPI refLogAPI
                        <$> newTVarIO (Just ref)
                        <*> newTVarIO defSegmentSize
                        <*> newTVarIO defCompressionLevel

        liftIO $ withGit3Env connected again

    e -> throwIO e
---

data TreeReadState = TreeReadState
  { treeReadKnownObjects :: HashSet GitHash
  , treeReadKnownTrees :: HashSet GitHash
  , treeReadKnownCommits :: HashSet GitHash
  , treeReadQueue :: [(GitObjectType, GitHash)]
  }
  deriving (Generic)

emptyTreeReadState :: TreeReadState
emptyTreeReadState = TreeReadState
  { treeReadKnownObjects = mempty
  , treeReadKnownTrees = mempty
  , treeReadKnownCommits = mempty
  , treeReadQueue = mempty
  }

pushKnownObject :: (State.MonadState TreeReadState m) => GitHash -> m ()
pushKnownObject co = State.modify' (over #treeReadKnownObjects (HS.insert co))

queryIsKnownObject :: (State.MonadState TreeReadState m) => GitHash -> m Bool
queryIsKnownObject co = State.gets (HS.member co . view #treeReadKnownObjects)

pushKnownTree :: (State.MonadState TreeReadState m) => GitHash -> m ()
pushKnownTree co = State.modify' (over #treeReadKnownTrees (HS.insert co))

queryIsKnownTree :: (State.MonadState TreeReadState m) => GitHash -> m Bool
queryIsKnownTree co = State.gets (HS.member co . view #treeReadKnownTrees)

pushKnownCommit :: (State.MonadState TreeReadState m) => GitHash -> m ()
pushKnownCommit co = State.modify' (over #treeReadKnownCommits (HS.insert co))

queryIsKnownCommit :: (State.MonadState TreeReadState m) => GitHash -> m Bool
queryIsKnownCommit co = State.gets (HS.member co . view #treeReadKnownCommits)

pushObjHash :: (State.MonadState TreeReadState m) => (GitObjectType, GitHash) -> m ()
pushObjHash p = State.modify' (over #treeReadQueue (p:))

popObjHash :: (State.MonadState TreeReadState m) => m (Maybe (GitObjectType, GitHash))
popObjHash = State.state \s -> case treeReadQueue s of
  [] -> (Nothing, s)
  a:as -> (Just a, s { treeReadQueue = as })

queueCondBlob :: (State.MonadState TreeReadState m) => GitHash -> m ()
queueCondBlob co = do
  queryIsKnownObject co >>= flip unless do
    pushObjHash (Blob, co)
    pushKnownObject co

queueCondTree :: (State.MonadState TreeReadState m) => GitHash -> m ()
queueCondTree co = do
  queryIsKnownTree co >>= flip unless do
    pushObjHash (Tree, co)
    pushKnownTree co

queueCondCommit :: (State.MonadState TreeReadState m) => GitHash -> m ()
queueCondCommit co = do
  queryIsKnownCommit co >>= flip unless do
    pushObjHash (Commit, co)
    pushKnownCommit co


newtype CacheTVH k v = CacheTVH (TVar (HashMap k v))

instance Hashable k => Cached (CacheTVH k v) k v where
  isCached (CacheTVH t) k = readTVarIO  t <&> HM.member k
  uncache (CacheTVH t) k = atomically (modifyTVar t (HM.delete k))
  cached (CacheTVH t) k a = do
    what <- readTVarIO t <&> HM.lookup k
    case what of
      Just x -> pure x
      Nothing -> do
        r <- a
        atomically $ modifyTVar t (HM.insert k r)
        pure r

data CacheFixedHPSQ  k v =
  CacheFixedHPSQ
  { _cacheSize :: Int
  , _theCache  :: TVar (HashPSQ k TimeSpec v)
  }

newCacheFixedHPSQ :: MonadIO m => Int -> m (CacheFixedHPSQ k v)
newCacheFixedHPSQ l = CacheFixedHPSQ l <$> newTVarIO HPSQ.empty

instance (Ord k, Hashable k) => Cached (CacheFixedHPSQ k v) k v where

  isCached CacheFixedHPSQ{..} k = readTVarIO _theCache <&> HPSQ.member k

  uncache CacheFixedHPSQ{..} k = atomically $ modifyTVar _theCache (HPSQ.delete k)

  cached CacheFixedHPSQ{..} k a = do
    w <- readTVarIO _theCache <&> HPSQ.lookup k
    case w of
      Just (_,e) -> pure e
      Nothing -> do
        v <- a

        t <- getTimeCoarse

        atomically do
          s <- readTVar _theCache <&> HPSQ.size

          when (s >= _cacheSize) do
            modifyTVar _theCache HPSQ.deleteMin

          modifyTVar _theCache (HPSQ.insert k t v)

          pure v

data HCC =
  HCC { hccHeight :: Int
      , hccRest   :: [GitHash]
      , hccResult :: HashPSQ GitHash Int (HashSet GitHash)
      }

readCommitChainHPSQ :: ( HBS2GitPerks m
                       , MonadUnliftIO m
                       , MonadReader Git3Env m
                       , HasStorage m
                       )
       => (GitHash -> m Bool)
       -> Maybe GitRef
       -> GitHash
       -> (GitHash -> m ())
       -> m (HashPSQ GitHash Int (HashSet GitHash))

readCommitChainHPSQ filt _ h0 action = flip runContT pure $ callCC \_ -> do
  theReader  <- ContT $ withGitCat
  void $ ContT $ bracket (pure theReader) stopProcess
  flip fix (HCC 0 [h0] HPSQ.empty) $ \next  -> \case

    HCC _ [] result -> pure result

    HCC n ( h : hs ) result | HPSQ.member h result -> do
      next  ( HCC n hs result )

    HCC n ( h : hs ) result -> do

        done <- not <$> lift (filt h)

        if done then next  ( HCC n hs result ) else do

          co <- gitReadObjectMaybe theReader h
                     >>= orThrow(GitReadError $ show $ pretty "object not found" <+> pretty h)

          parents <- gitReadCommitParents (Just h) (snd co)

          lift $ action h
          next $ HCC (n-1) ( parents <> hs ) (snd $ HPSQ.alter (addParents () n parents) h result )


  where
    addParents :: a
               -> Int
               -> [GitHash]
               -> Maybe (Int, HashSet GitHash)
               -> (a, Maybe (Int, HashSet GitHash))

    addParents a n p = \case
      Nothing -> (a, Just (n, HS.fromList p))
      Just (l,s) -> (a, Just (min l n, s <> HS.fromList p))


readIndexFromFile :: forall m . MonadIO m
                  => FilePath
                  -> m (HashSet GitHash)
readIndexFromFile fname = do

    bs <- liftIO $ LBS.readFile fname

    r <- S.toList_ $ runConsumeLBS bs $ flip fix 0 \go n -> do
      done <- noBytesLeft
      if done then pure ()
        else do
          _  <- readBytesMaybe 4
                  >>= orThrow SomeReadLogError
                  <&> fromIntegral . N.word32 . LBS.toStrict

          hash   <- readBytesMaybe 20
                       >>= orThrow SomeReadLogError
                       <&> GitHash . LBS.toStrict

          lift (S.yield hash)
          go (succ n)

    pure $ HS.fromList r

-- FIXME: move-to-suckless-script
splitOpts :: [(Id,Int)]
          -> [Syntax C]
          -> ([Syntax C], [Syntax C])

splitOpts def opts' = flip fix (mempty, opts) $ \go -> \case
  (acc, []) -> acc
  ( (o,a), r@(StringLike x) : rs ) -> do
    case HM.lookup (fromString x) omap of
      Nothing -> go ((o, a <> [r]), rs)
      Just n  -> do
        let (w, rest) = L.splitAt n rs
        let result = mkList @C ( r : w )
        go ( (o <> [result], a), rest )
  ( (o,a), r : rs ) -> do
      go ((o, a <> [r]), rs)

  where
    omap = HM.fromList [ (p, x) | (p,x) <- def ]
    opts = opts'

data ECC =
    ECCInit
  | ECCWrite Int FilePath Handle Result
  | ECCFinalize Bool FilePath Handle Result



mergeSortedFiles :: forall m . MonadUnliftIO m
                  => (ByteString -> ByteString)
                  -> FilePath
                  -> FilePath
                  -> FilePath
                  -> m ()

mergeSortedFiles getKey file1 file2 outFile = do
  l1 <- parseFile file1
  l2 <- parseFile file2

  UIO.withBinaryFileAtomic outFile WriteMode $ \hOut ->
    mergeEntries l1 l2 getKey (\s -> writeSection s (liftIO . LBS.hPutStr hOut))

  mapM_ rm [file1, file2]

  where
    parseFile :: FilePath -> m [ByteString]
    parseFile path = do
      lbs <- liftIO $ LBS.readFile path
      S.toList_ $ runConsumeLBS lbs $ readSections $ \_ sdata -> lift $ S.yield sdata

    mergeEntries :: [ByteString]
                 -> [ByteString]
                 -> (ByteString -> ByteString)
                 -> (ByteString -> m ()) -> m ()

    mergeEntries [] ys _ write = mapM_ write ys
    mergeEntries xs [] _ write = mapM_ write xs
    mergeEntries (x:xs) (y:ys) extractKey write
      | extractKey x <= extractKey y = write x >> mergeEntries xs (y:ys) extractKey write
      | otherwise                    = write y >> mergeEntries (x:xs) ys extractKey write


mergeSortedFilesN :: forall m . MonadUnliftIO m
                  => (N.ByteString -> N.ByteString) -- ^ Функция извлечения ключа
                  -> [FilePath]                -- ^ Входные файлы
                  -> FilePath                  -- ^ Выходной файл
                  -> m ()

mergeSortedFilesN _ [] out  = rm out

mergeSortedFilesN _ [_] out = rm out

mergeSortedFilesN getKey inputFiles outFile = do

  mmaped <- for inputFiles $ \fn -> do
     liftIO (mmapFileByteString fn Nothing)

  liftIO $ UIO.withBinaryFileAtomic outFile WriteMode $ \hOut -> do
    flip fix (mmaped, Heap.empty) $ \next (mmf, win) -> do
      let (entries, files) = fmap readEntry mmf & unzip
      let values = [ Entry (getKey e) e | e <- catMaybes entries ]
      let e' = (win <> Heap.fromList values) & Heap.uncons
      maybe1 e' none $ \(Entry _ e, newWin) -> do
        liftIO $ writeSection (LBS.fromStrict e) (LBS.hPutStr hOut)
        next (catMaybes files, newWin)

  mapM_ rm inputFiles

  where
    readEntry :: BS.ByteString -> (Maybe BS.ByteString, Maybe BS.ByteString)

    readEntry src | BS.length src < 4 = (mzero, mzero)

    readEntry src = do
      let (size, rest) = BS.splitAt 4 src & over _1 ( fromIntegral . N.word32 )
      let (e, rest2) = BS.splitAt size rest

      if BS.length e < size then
        (mzero, mzero)
      else
        (Just e, Just rest2)


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

        entry $ bindMatch "git:tree:ls" $ nil_ $ const do
          r <- gitReadTree "HEAD"
          for_ r $ \GitTreeEntry{..} -> do
            liftIO $ print $  pretty gitEntryHash
                          <+> pretty gitEntryType
                          <+> pretty gitEntrySize
                          <+> pretty gitEntryName

        entry $ bindMatch "test:git:tree:read:bench" $ nil_ $ \syn -> do
          (mpath, sref) <- case syn of
            [ HashLike s ] -> pure (Nothing, s)
            [ StringLike path , HashLike s ] -> pure (Just path, s)
            [ StringLike path ] -> pure (Just path, "HEAD")
            [] -> pure (Nothing, "HEAD")
            _ -> throwIO (BadFormException @C nil)
          liftIO $ mapM_ setCurrentDirectory mpath
          ref0 <- gitRevParse sref
                  `orDie` (show $ "Can not find revision" <+> pretty sref)
          liftIO $ print sref
          liftIO $ print $ pretty ref0
          withGitCat \reader -> do
            cs :: [GitHash] <- Writer.execWriterT $ flip State.evalStateT emptyTreeReadState do
              pushObjHash (Commit, ref0)
              fix \go ->
                popObjHash >>= maybe (pure ()) \(ty', co) -> (>> go) do
                  unless (ty' == Commit) do
                      throwIO $ userError $ show $ "Only commits should be in queue. Got" <+> pretty ty'
                  -- lift $ Writer.tell [co]
                  (ty, bs) <- gitReadObjectOrThrow reader co
                  liftIO . print $ pretty co <+> viaShow ty <+> pretty (LBS.length bs)
                  unless (ty' == ty) do
                      throwIO $ userError $ show $ "object types do not match" <+> pretty ty' <+> pretty ty
                  case ty of
                    Commit -> do
                      commitParents <- gitReadCommitParents Nothing bs
                      mapM_ queueCondCommit commitParents
                      -- queueCondTree commitTree
                    Tree -> do
                      gitReadTree co >>= mapM_ \GitTreeEntry {..} ->
                        case gitEntryType of
                            Commit -> do
                              throwIO $ userError "Impossible commit entry in a git tree"
                            Tree -> do
                              queryIsKnownTree gitEntryHash >>= flip unless do
                                (ty'', bs'') <- gitReadObjectOrThrow reader gitEntryHash
                                liftIO . print $ pretty gitEntryHash <+> viaShow ty'' <+> pretty (LBS.length bs'')
                                pushKnownTree gitEntryHash
                            Blob -> do
                              queueCondBlob gitEntryHash
                    Blob -> do
                      pure ()
            -- liftIO $ print $ "Commits:" <+> pretty (length cs)
            pure ()

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

        entry $ bindMatch "test:git:read-commits" $ nil_ $ \syn -> do
          let hdr = headDef "HEAD" [ w | StringLike w <- syn ] :: String

          commits <- gitRunCommand [qc|git rev-list -100000 {hdr}|]
                      >>= orThrowPassIO
                      <&> mapMaybe (fromStringMay @GitHash . LBS8.unpack) . LBS8.lines

          liftIO $ print $ pretty $ length commits

        entry $ bindMatch "test:git:hash:blob" $ nil_ $ const $ liftIO do
          co <- LBS.hGetContents stdin
          print $ pretty $ gitHashBlobPure co

        entry $ bindMatch "test:git:exists:fast" $ nil_ \case
          [ StringLike x ] -> lift $ flip runContT pure do

            h <- fromStringMay @GitHash x & orThrowUser "invalid hash"

            cache <- newCacheFixedHPSQ 10
            reader <- ContT $ withGitCat
            ContT $ bracket none $ const $ stopProcess reader

            what <- liftIO (cached cache h (gitReadObjectMaybe reader h))
                      <&> isJust

            liftIO $ print $ pretty what

          _ -> throwIO (BadFormException @C nil)

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

        entry $ bindMatch "test:git:log:index:flat:search:linear:test" $ nil_ $ \case
          [ StringLike fn ] -> do

            lbs <- liftIO $ LBS.readFile fn

            hashes <- S.toList_ $ runConsumeLBS lbs $ flip fix 0 \go n -> do
              done <- consumed
              if done then pure ()
                else do
                  ssize  <- readBytesMaybe 4
                                >>= orThrow SomeReadLogError
                                <&> fromIntegral . N.word32 . LBS.toStrict

                  hash   <- readBytesMaybe 20
                               >>= orThrow SomeReadLogError
                               <&> GitHash . LBS.toStrict

                  lift $ S.yield hash
                  go (succ n)

            for_ hashes $ \h ->do
              found <- linearSearchLBS h lbs
              liftIO $ print $ pretty h <+> pretty (isJust found)

          _ -> throwIO (BadFormException @C nil)


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

        entry $ bindMatch "test:git:log:index:flat:search:linear" $ nil_ $ \case
          [ StringLike ha, StringLike fn ] -> lift do
            hash <- fromStringMay @GitHash ha & orThrowUser "not a git hash"

            lbs <- liftIO $ LBS.readFile fn
            found <- linearSearchLBS hash lbs
            liftIO $ print $ pretty found

          _ -> throwIO (BadFormException @C nil)


        entry $ bindMatch "test:git:log:index:flat:search:linear2" $ nil_ $ \case
          [ StringLike ha, StringLike fn ] -> lift do
            hash <- fromStringMay @GitHash ha & orThrowUser "not a git hash"

            file <- liftIO $ mmapFileByteString fn Nothing

            found <- S.toList_ $ flip fix (0,file) \go (o,bs) -> do
              unless (BS.null bs) do
                let (hdr, rest) = BS.splitAt 24 bs
                let hx = BS.take 20 $ BS.drop 4 hdr

                when (hx == coerce @_ @BS.ByteString hash ) do
                  S.yield o

                go (o+1, rest)

            liftIO $ print $ listToMaybe found

          _ -> throwIO (BadFormException @C nil)


        entry $ bindMatch "test:git:log:index:entry" $ nil_ $ \case
          [LitIntVal i, StringLike fn] -> lift do

            bs <- liftIO $ mmapFileByteString fn Nothing
            let index = fromIntegral i
            let offset = index * 24

            let record = BS.take 24 (BS.drop offset bs)
            let n = BS.take 4 record & N.word32
            let key = BS.take 20 $ BS.drop 4 record
            liftIO $ print $ pretty n <+> pretty (GitHash key)

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:log:index:flat" $ nil_ $ \syn -> lift do
          let (_, argz) = splitOpts [] syn
          let fnames = [ x | StringLike x <- argz]

          s <- randomIO @Word16
          liftIO $ withBinaryFile (show ("index" <> pretty s <> ".idx")) AppendMode $ \fh -> do

            all <- S.toList_ do

              for_ fnames $ \f -> do
                theLog <- liftIO $ LBS.readFile f

                void $ runConsumeLBS (ZstdL.decompress theLog) $  readLogFileLBS () $ \h s lbs -> do
                  lift $ S.yield (coerce @_ @BS.ByteString h)
                  debug $ "object" <+> pretty h

            let sorted = Set.toList $ Set.fromList all

            for_ sorted $ \ghs -> do
              let ks = BS.length ghs
              let entrySize = N.bytestring32 (fromIntegral ks)
              BS.hPutStr fh entrySize
              BS.hPutStr fh ghs


        entry $ bindMatch "test:sqlite" $ nil_ $ \case
          [ StringLike fn ] -> lift do
              db <- newDBPipeEnv dbPipeOptsDef fn
              withDB db do
                all <- select_ @_ @(Only Text) [qc|select hash from githash|]
                for_ all $ \x -> do
                  n <- select @(Only Int) [qc|select 1 from githash where hash = ?|] (Only (fromOnly x))
                           <&> L.null
                  unless n do
                    liftIO $ print $ pretty (fromOnly x)

          _ -> throwIO (BadFormException @C nil)


        entry $ bindMatch "test:git:zstd:packed:import" $ nil_ $ \syn -> lift $ flip runContT pure do
          let (opts, argz) = splitOpts [] syn
          let logs = [ x| StringLike x <- argz ]

          d <- findGitDir >>= orThrowUser "not a git directory"

          gitCatCheck <- contWorkerPool 8 do
            che <- ContT withGitCatCheck
            pure $ gitCheckObjectFromHandle che

          lift $ forConcurrently_ logs $ \lfn -> do

            debug $ pretty lfn

            lbs <- liftIO $ LBS.readFile lfn

            runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \h s lbs -> do
              let (t, body) = LBS.splitAt 1 lbs

              let tp = fromStringMay @(Short GitObjectType) (LBS8.unpack t)
                           & maybe Blob coerce

              here <- isJust <$> lift (gitCatCheck h)

              let gitHash = show $ pretty h
              let (prefix,name) = L.splitAt 2 gitHash
              let path = joinPath [d, "objects", prefix, name]

              let signature = [qc|{pretty tp} {pretty $ LBS.length body}|] <> "\x00" :: LBS8.ByteString
              let o = signature <> body

              unless here $ liftIO do

                debug $ "FUCKING IMPORT OBJECT" <+> pretty here <+> pretty h <+> pretty tp

                touch path

                debug $ pretty tp <+> pretty s <+> pretty h <+> pretty path

                let params = Zlib.defaultCompressParams { Zlib.compressMethod = Zlib.deflateMethod }
                UIO.withBinaryFileAtomic path WriteMode $ \fh -> do
                  let contents = Zlib.compressWith params  o
                  LBS.hPutStr fh contents


        entry $ bindMatch "test:git:reflog:index:list:fast" $ nil_ $ const $ lift do
          files <- listObjectIndexFiles
          forConcurrently_ files  $ \(f,_) -> do
            bs <- liftIO $ mmapFileByteString f Nothing
            scanBS bs $ \segment  -> do
              let (sha1,blake) = BS.splitAt 20 segment
                                      & over _1 (coerce @_ @GitHash)
                                      & over _2 (coerce @_ @HashRef)

              notice $ pretty sha1 <+> pretty blake

        entry $ bindMatch "reflog:index:list" $ nil_ $ const $ lift do
          files <- listObjectIndexFiles
          for_ files  $ \(ifn,_) -> do
            lbs <- liftIO $ LBS.readFile ifn

            void $ runConsumeLBS lbs $ readSections $ \s ss -> do

              let (sha1, blake) = LBS.splitAt 20 ss
                                      & over _1 (coerce @_ @GitHash . LBS.toStrict)
                                      & over _2 (coerce @_ @HashRef . LBS.toStrict)

              liftIO $ hPrint stdout $ pretty sha1 <+> pretty blake

        entry $ bindMatch "test:git:reflog:index:sqlite" $ nil_ $ \syn -> lift $ connectedDo do

          reflog <- getGitRemoteKey >>= orThrowUser "reflog not set"

          api <- getClientAPI @RefLogAPI @UNIX

          sto <- getStorage

          flip runContT pure do

            what' <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 2) api reflog
                      >>= orThrowUser "rpc timeout"

            what <- ContT $ maybe1 what' none

            idxPath <- getStatePath (AsBase58 reflog) <&> (</> "index")
            mkdir idxPath

            notice $ "STATE" <+> pretty idxPath

            sink <- S.toList_ do
              walkMerkle (coerce what) (getBlock sto) $ \case
                Left{} -> throwIO MissedBlockError
                Right (hs :: [HashRef]) -> do
                  for_ hs $ \h -> void $ runMaybeT do

                    tx <- getBlock sto (coerce h)
                             >>= toMPlus

                    RefLogUpdate{..} <- deserialiseOrFail @(RefLogUpdate L4Proto) tx
                                          & toMPlus

                    AnnotatedHashRef _ href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict _refLogUpdData)
                                                 & toMPlus

                    -- FIXME: error logging
                    lbs <- liftIO (runExceptT (getTreeContents sto href))
                             >>= orThrow MissedBlockError

                    pieces <- S.toList_ do
                      void $ runConsumeLBS (ZstdL.decompress lbs) $ readLogFileLBS () $ \o s _ -> do
                        lift $ S.yield o

                    lift $ S.yield (h, pieces)

            file <- liftIO $ Temp.emptyTempFile "" "index.db"

            db <- newDBPipeEnv dbPipeOptsDef file

            liftIO $ withDB db do

              ddl [qc|create table object (sha1 text not null primary key, tx text not null)|]

              for_ sink $ \(h, pieces) -> do
                transactional do
                  for_ pieces $ \p -> do
                    void $ insert [qc|insert into
                                        object (sha1,tx)
                                      values(?,?)
                                      on conflict (sha1)
                                      do update set tx = excluded.tx|] (p,h)

        entry $ bindMatch "test:git:reflog:index:merge" $ nil_ $ \case
          [ StringLike f1, StringLike f2] -> lift do
            mergeSortedFiles (LBS.take 20) f1 f2 "jopakita"

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:reflog:index:compact" $ nil_ $ \syn -> lift do
          reflog <- getGitRemoteKey >>= orThrowUser "reflog not set"
          idxPath <- getStatePath (AsBase58 reflog) <&> (</> "index")
          mkdir idxPath

          files <- dirFiles idxPath
                     <&> filter ((== ".idx") .  takeExtension)

          out <- liftIO $ emptyTempFile idxPath "objects-.idx"

          mergeSortedFilesN (BS.take 20) files out

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
          r <- newTVarIO ( mempty :: HashSet HashRef )
          index <- openIndex
          enumEntries index $ \bs -> do
            atomically $ modifyTVar r (HS.insert (coerce $ BS.take 32 $ BS.drop 20 bs))
          z <- readTVarIO r <&> HS.toList
          liftIO $ mapM_ ( print . pretty ) z

        entry $ bindMatch "reflog:index:build" $ nil_ $ const $ lift $ connectedDo do
          writeReflogIndex

        entry $ bindMatch "test:reflog:index:lookup" $ nil_ \case
          [ GitHashLike h ] -> lift do
            idx <- openIndex
            what <- indexEntryLookup idx h >>= orThrowUser "object not found"
            liftIO $ print $ pretty ( coerce @_ @HashRef what )

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "git:commit:list:objects:new" $ nil_ $ \case
          [ StringLike what ] -> lift do

            commit  <- gitRevParseThrow what

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

            idx <- lift openIndex
            let req h = lift $ indexEntryLookup idx h <&> isNothing

            (t1,r) <- timeItT (lift $ readCommitChainHPSQ req Nothing h0 dontHandle)

            let s = HPSQ.size r
            notice $ pretty s <+> "new commits read at" <+> pretty (realToFrac @_ @(Fixed E3) t1)

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

            notice $ "all shit read" <+> pretty (realToFrac @_ @(Fixed E2) t3)

            (t4,new) <- lift $ timeItT $ readTVarIO uniq_ >>= indexFilterNewObjects idx

            notice $ pretty (length new) <+> "new objects" <+> "at" <+> pretty (realToFrac @_ @(Fixed E2) t4)

            -- x <- readTVarIO uniq_ <&> HS.size

            -- blmn <- readTVarIO blmn_
            -- notice $ "all shit filter" <+> parens (pretty x) <+> brackets (pretty blmn) <+> pretty (realToFrac @_ @(Fixed E2) t4)

            -- notice $ pretty (length new)


            -- notice $ "total objects" <+> pretty
            -- notice $ "present" <+> pretty nhere

           -- liftIO $ print $ pretty (HS

            -- fix \next -> do
            --   h' <- atomically do
            --          pollSTM r >>= \case
            --            Just{}  -> pure Nothing
            --            Nothing -> readTQueue out <&> Just

            --   maybe1 h' none $ \h ->do
            --     liftIO $ print $ pretty h
            --     next

        entry $ bindMatch "test:git:export" $ nil_ $ \syn -> lift $ connectedDo do
          let (opts, argz) = splitOpts [("--index",1),("--ref",1)] syn

          let useIndex = headMay [ f | ListVal [StringLike "--index", StringLike f] <- opts ]

          let hd = headDef "HEAD" [ x | StringLike x <- argz]
          h <- gitRevParseThrow hd

          let refs = [ gitNormaliseRef (fromString x)
                     | ListVal [StringLike "--ref", StringLike x] <- opts
                     ]

          mmaped <- runMaybeT do
                      fname <- toMPlus useIndex
                      liftIO $ mmapFileByteString fname Nothing

          _already <- newTVarIO mempty

          level    <- getCompressionLevel
          segment  <- getPackedSegmetSize
          env      <- ask

          let
            notWrittenYet :: forall m . MonadIO m => GitHash -> m Bool
            notWrittenYet x = do
                already <- readTVarIO _already <&> HS.member x
                alsoInIdx <- maybe1 mmaped (pure False) $ \m -> do
                  found <- binarySearchBS 24 (BS.take 20 . BS.drop 4) (coerce x) m
                  pure (isJust found)
                pure (not already &&  not alsoInIdx)

          hs <- maybe1 useIndex (pure mempty) $ \fn -> readIndexFromFile fn

          debug $ "INDEX" <+> pretty (HS.size hs)

          hpsq <- readCommitChainHPSQ notWrittenYet Nothing h (\c -> debug $ "commit" <+> pretty c)

          let r = HPSQ.toList hpsq
                    & sortBy (comparing (view _2))
                    & fmap (view _1)

          let total = HPSQ.size hpsq
          bytes_ <- newTVarIO 0

          debug $ "TOTAL" <+> pretty total

          liftIO $ flip runContT pure do

            tn <- getNumCapabilities

            sourceQ <- newTBQueueIO (fromIntegral tn * 1024)
            hbs2Q   <- newTBQueueIO @_ @(Maybe FilePath) 100

            hbs2 <- liftIO $ async $ void $ withGit3Env env do
                      sto <- getStorage
                      reflogAPI <- getClientAPI @RefLogAPI @UNIX

                      reflog <- getGitRemoteKey
                                    >>= orThrowUser "reflog not set"

                      lift $ fix \next -> atomically (readTBQueue hbs2Q) >>= \case
                        Nothing -> none
                        Just fn -> void $ flip runContT pure do
                          ContT $ bracket none (const $ rm fn)
                          lift do
                            ts <- liftIO getPOSIXTime <&> round
                            lbs <- LBS.readFile fn
                            let meta = mempty
                            let gk = Nothing
                            href <- createTreeWithMetadata sto gk meta lbs >>= orThrowPassIO
                            writeLogEntry ("tree" <+> pretty ts <+> pretty href)
                            debug $ "SENDING" <+> pretty href <+> pretty fn

                            let payload = pure $ LBS.toStrict $ serialise (AnnotatedHashRef Nothing href)
                            tx <- mkRefLogUpdateFrom (coerce reflog) payload

                            r <- callRpcWaitMay @RpcRefLogPost (TimeoutSec 2) reflogAPI tx
                                    >>= orThrowUser "rpc timeout"

                            rm fn
                            next

            link hbs2

            l <- lift (async (segmentWriter env bytes_ sourceQ hbs2Q) >>= \x -> link x >> pure x)

            let chunkSize = if total > tn*2 then total `div` tn else total
            let commitz = chunksOf chunkSize r

            progress_ <- newTVarIO 0

            gitCatBatchQ <- contWorkerPool 16 do
              che <- ContT withGitCat
              pure $ gitReadObjectMaybe che

            -- void $ ContT $ bracket (pure pool) cancel

            let lastCommit = last r

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
                    atomically $ modifyTVar _already (HS.insert gh)
                    -- debug $ "object" <+> pretty gh
                    (_t,lbs) <- lift (gitCatBatchQ gh)
                                   >>= orThrow (GitReadError (show $ pretty gh))

                    let e = [ Builder.byteString (coerce gh)
                            , Builder.char8 (headDef 'B' $ show $ pretty $ Short _t)
                            , Builder.lazyByteString lbs
                            ] & Builder.toLazyByteString . mconcat

                    atomically do
                      writeTBQueue sourceQ (Just e)

                  when (commit == lastCommit) do

                    ts <- liftIO $ getPOSIXTime <&> round

                    let brefs = [ LBS8.pack (show $ pretty ts <+> pretty commit <+> pretty x)
                                | x <- refs
                                ] & LBS8.unlines

                    let sha1 = gitHashBlobPure brefs

                    debug $ green "THIS IS THE LAST COMMIT BLOCK" <+> pretty commit <+> "ADDING REF INFO" <+> pretty sha1

                    let e = [ Builder.byteString (coerce sha1)
                            , Builder.char8 'R'
                            , Builder.lazyByteString brefs
                            ] & Builder.toLazyByteString . mconcat

                    atomically do
                      writeTBQueue sourceQ (Just e)

            t0 <- getTimeCoarse
            ContT $ withAsync $ do

              liftIO $ hPrint stderr $
                                   "segment" <+> pretty segment <> comma
                                <> "compression level" <+> pretty level

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

                liftIO $ IO.hPutStr stderr  $ show $
                    "                 \r"
                    <+> pretty tspent <> "s"
                    <+> pretty mbytes <> "mb"
                    <+> pretty dbdt <> "mbs"
                    <+> pretty pp <> "%"

                next (t1,b)


            mapM_ link workers
            mapM_ wait workers

            atomically do
              writeTBQueue sourceQ Nothing

            mapM_  wait [hbs2,l]

     where

        writeLogEntry e = do
          path <- getConfigPath <&> (</> "log")
          touch path
          liftIO (IO.appendFile path (show $ e <> line))

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
              loop (ECCFinalize True fn fh sn)

            ECCWrite bnum fn fh sn -> do
              atomically (readTBQueue sourceQ) >>= \case
                 Nothing  -> loop (ECCFinalize False fn fh sn)
                 Just s -> do
                    lbs <- S.toList_ (writeSection s $ S.yield) <&> mconcat

                    sz_ <- newTVarIO 0

                    sn1 <- writeCompressedChunkZstd (write sz_ fh) sn (Just lbs)

                    sz <- readTVarIO sz_ <&> fromIntegral
                    atomically $ modifyTVar bytes_ (+ fromIntegral sz)

                    loop (ECCWrite (bnum + sz) fn fh sn1)

            ECCFinalize again fn fh sn -> do
              void $ writeCompressedChunkZstd (write bytes_ fh) sn Nothing
              hClose fh
              atomically $ writeTBQueue hbs2Q (Just fn)
              debug $ "POST SEGMENT" <+> pretty fn
              when again $ loop ECCInit
              atomically $ writeTBQueue hbs2Q Nothing

         where
            write sz_ fh ss = do
               LBS.hPutStr fh ss
               atomically $ modifyTVar sz_ (+ LBS.length ss)

contWorkerPool :: (MonadUnliftIO m)
  => Int
  -> ContT () m (a -> m b)
  -> ContT () m (a -> m b)
contWorkerPool n w = fmap join <$> contWorkerPool' n w

-- | здесь: a -> m (m b)
-- первое m - чтобы задать вопрос
-- второе m - чтобы получить ответ
contWorkerPool' :: (MonadUnliftIO m)
  => Int
  -> ContT () m (a -> m b)
  -> ContT () m (a -> m (m b))
contWorkerPool' n contWorker = do
    inQ <- newTQueueIO
    -- запускаем воркеров
    replicateM_ n do
      (link <=< ContT . withAsync) do
        runContT contWorker \w -> do
          (fix . (>>)) do
            (a, reply) <- atomically $ readTQueue inQ
            reply =<< tryAny (w a)
    -- возвращаем функцию, с помощью которой отправлять воркерам запрос
    -- и получать ответ
    pure \a -> do
      tmv <- newEmptyTMVarIO
      atomically $ writeTQueue inQ (a, atomically . STM.putTMVar tmv)
      pure do
        either throwIO pure =<< atomically (readTMVar tmv)


limitedResourceWorkerRequestQ :: MonadUnliftIO m
                              => Int
                              -> m r                  -- ^ create resource
                              -> ( r -> m () )        -- ^ destroy resource
                              -> m ( Async (), (r -> m b) -> m b )

limitedResourceWorkerRequestQ n create destroy = do
  inQ <- newTQueueIO
  ass <- async $ flip runContT pure do
    replicateM_  n do
        (link <=< ContT . withAsync) $ flip runContT pure do
          r <- ContT $ bracket create destroy
          (fix . (>>)) $ atomically (readTQueue inQ) >>= \(a,reply) -> do
            lift (tryAny (a r)) >>= reply

  pure $ (ass, \fn -> do
    tmv <- newEmptyTMVarIO
    atomically $ writeTQueue inQ (fn, atomically . STM.putTMVar tmv)
    atomically (readTMVar tmv) >>= either throwIO pure)

linearSearchLBS hash lbs = do

  found <- S.toList_ $ runConsumeLBS lbs $ flip fix 0 \go n -> do
    done <- consumed
    if done then pure ()
      else do
        ssize  <- readBytesMaybe 4
                        >>= orThrow SomeReadLogError
                        <&> fromIntegral . N.word32 . LBS.toStrict

        hash1   <- readBytesMaybe 20
                       >>= orThrow SomeReadLogError
                       <&> LBS.toStrict

        void $ readBytesMaybe 32

        case (compare hash1 (coerce hash)) of
          EQ -> lift $ S.yield n
          _  -> go (succ n)

  pure $ listToMaybe found



-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

main :: IO ()
main = flip runContT pure do

  setupLogger

  ContT $ bracket none $ const do
    silence

  argz <- liftIO $ E.getArgs
  cli <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  env <- nullGit3Env

  void $ lift $ withGit3Env env do
    conf <- readLocalConf
    let dict = theDict
    recover $ setupLogger >> run dict (conf <> cli)
      `finally` silence

