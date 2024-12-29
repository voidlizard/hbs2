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

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Base58
import HBS2.Merkle
import HBS2.Data.Detect hiding (Blob)
import HBS2.Data.Detect qualified as Detect

import HBS2.Storage
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.CLI.Run.Internal.Merkle (getTreeContents)

-- move to a sepatate library
import HBS2.Data.Log.Structured


import HBS2.CLI.Run.Internal.Merkle (createTreeWithMetadata)
import HBS2.CLI.Run.RefLog (getCredentialsForReflog,mkRefLogUpdateFrom)

import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff as Exported

import HBS2.Git3.Types
import HBS2.Git3.State.Direct
import HBS2.Git3.Config.Local
import HBS2.Git3.Git

import Data.Config.Suckless.Script
import DBPipe.SQLite

import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Streaming qualified as ZstdS
import Codec.Compression.Zstd.Streaming (Result(..))
import Codec.Compression.Zstd (maxCLevel)
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
import Data.Ord (comparing)
import Data.Generics.Labels
import Data.Generics.Product
import Lens.Micro.Platform

import Streaming.Prelude qualified as S
import System.Exit qualified as Q
import System.Environment qualified as E
import System.Process.Typed
import Control.Applicative
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.State qualified as State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Control.Concurrent.STM qualified as STM
import System.Directory (setCurrentDirectory)
import System.Random hiding (next)
import System.IO.MMap (mmapFileByteString)
import System.IO qualified as IO
import System.IO (hPrint)
import System.IO.Temp as Temp

import Data.Either
import Data.Coerce
import Data.Kind
import Data.Vector qualified as Vector
import Data.Vector.Algorithms.Search qualified as MV

import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.IO.File qualified as UIO

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}


defSegmentSize :: Int
defSegmentSize = 50 * 1024 * 1024

defCompressionLevel :: Int
defCompressionLevel = maxCLevel

type HBS2GitPerks m = (MonadUnliftIO  m)

quit :: MonadUnliftIO m => m ()
quit = liftIO Q.exitSuccess

class Cached cache k v | cache -> k, cache -> v where
  isCached :: forall m . MonadIO m => cache -> k -> m Bool
  cached   :: forall m . MonadIO m => cache -> k -> m v -> m v
  uncache  :: forall m . MonadIO m => cache -> k -> m ()


class GitWritePacksOpts a where
  excludeParents :: a -> Bool

instance GitWritePacksOpts () where
  excludeParents = const True

data GitWritePacksOptVal =
  WriteFullPack
  deriving stock (Eq,Ord,Show,Generic)

instance Hashable GitWritePacksOptVal

instance GitWritePacksOpts (HashSet GitWritePacksOptVal) where
  excludeParents o = not $ HS.member WriteFullPack o

data Git3Exception =
  Git3PeerNotConnected
  deriving (Show,Typeable,Generic)

instance Exception Git3Exception

data Git3Env =
    Git3Disconnected
    { gitRefLog   :: TVar (Maybe GitRemoteKey)
    , gitPackedSegmentSize :: TVar Int
    , gitCompressionLevel  :: TVar Int
    }
  | Git3Connected
    { stateDb     :: DBPipeEnv
    , peerSocket  :: FilePath
    , peerStorage :: AnyStorage
    , peerAPI     :: ServiceCaller PeerAPI UNIX
    , reflogAPI   :: ServiceCaller RefLogAPI UNIX
    , gitRefLog   :: TVar (Maybe GitRemoteKey)
    , gitPackedSegmentSize :: TVar Int
    , gitCompressionLevel  :: TVar Int
    }

class HasGitRemoteKey m where
  getGitRemoteKey :: m (Maybe GitRemoteKey)
  setGitRemoteKey :: GitRemoteKey -> m ()

instance (MonadIO m, MonadReader Git3Env m) => HasGitRemoteKey m where
  getGitRemoteKey = do
    e <- ask
    liftIO $ readTVarIO (gitRefLog e)

  setGitRemoteKey k = do
    e <- ask
    liftIO $ atomically $ writeTVar (gitRefLog e) (Just k)

instance (MonadIO m, MonadReader Git3Env m) => HasExportOpts m where
  getPackedSegmetSize = asks gitPackedSegmentSize >>= readTVarIO
  setPackedSegmedSize x = do
    e <- asks gitPackedSegmentSize
    atomically $ writeTVar e x

  getCompressionLevel = asks gitCompressionLevel >>= readTVarIO
  setCompressionLevel x = do
    e <- asks gitCompressionLevel
    atomically $ writeTVar e (min maxCLevel x)

instance (MonadIO m) => HasStateDB (Git3 m) where
  getStateDB = asks stateDb

instance (MonadIO m, MonadReader Git3Env m) => HasStorage m where
  getStorage = do
    e <- ask
    case e of
      Git3Disconnected{} -> throwIO Git3PeerNotConnected
      Git3Connected{..} -> pure peerStorage

newtype Git3 (m :: Type -> Type) a = Git3M { fromGit3 :: ReaderT Git3Env m a }
                   deriving newtype ( Applicative
                                    , Functor
                                    , Monad
                                    , MonadIO
                                    , MonadUnliftIO
                                    , MonadReader Git3Env
                                    , MonadTrans
                                    )

type Git3Perks m = ( MonadIO m
                   , MonadUnliftIO m
                   )


instance MonadUnliftIO m => HasClientAPI PeerAPI UNIX (Git3 m) where
  getClientAPI = do
    ask  >>= \case
       Git3Disconnected{} -> throwIO Git3PeerNotConnected
       Git3Connected{..} -> pure peerAPI

instance (MonadUnliftIO m, MonadReader Git3Env m) => HasClientAPI RefLogAPI UNIX m where
  getClientAPI = do
    ask  >>= \case
       Git3Disconnected{} -> throwIO Git3PeerNotConnected
       Git3Connected{..} -> pure reflogAPI

nullGit3Env :: MonadIO m => m Git3Env
nullGit3Env = Git3Disconnected
                <$> newTVarIO Nothing
                <*> newTVarIO defSegmentSize
                <*> newTVarIO defCompressionLevel

connectedDo :: (MonadIO m, MonadReader Git3Env m) => m a -> m a
connectedDo what = do
  env <- ask
  debug $ red "connectedDo"
  case env of
    Git3Disconnected{} -> do
      throwIO Git3PeerNotConnected

    _ -> what

withGit3Env :: Git3Perks m => Git3Env -> Git3 m a -> m a
withGit3Env env a = runReaderT (fromGit3 a) env

runGit3 :: Git3Perks m => Git3Env -> Git3 m b -> m b
runGit3 env action = withGit3Env env action

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

        dbPath <- getStatePathDB (AsBase58 ref)

        touch dbPath
        db <- newDBPipeEnv dbPipeOptsDef dbPath

        let sto = AnyStorage (StorageClient storageAPI)

        connected <- Git3Connected db soname sto peerAPI refLogAPI
                        <$> newTVarIO (Just ref)
                        <*> newTVarIO defSegmentSize
                        <*> newTVarIO defCompressionLevel

        liftIO $ withGit3Env connected (evolveState >> again)

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
                       , HasStateDB m
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

readLogFileLBS :: forall opts m . ( MonadIO m, ReadLogOpts opts, BytesReader m )
               => opts
               -> ( GitHash -> Int -> ByteString -> m () )
               -> m Int

readLogFileLBS _ action = flip fix 0 \go n -> do
  done <- noBytesLeft
  if done then pure n
    else do
      ssize <- readBytesMaybe 4
                  >>= orThrow SomeReadLogError
                  <&> fromIntegral . N.word32 . LBS.toStrict

      hash  <- readBytesMaybe 20
                  >>= orThrow SomeReadLogError
                  <&> GitHash . BS.copy . LBS.toStrict

      sdata <- readBytesMaybe ( ssize - 20 )
                  >>= orThrow SomeReadLogError

      void $ action hash (fromIntegral ssize) sdata
      go (succ n)


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

class HasExportOpts m where
  setPackedSegmedSize :: Int -> m ()
  getPackedSegmetSize :: m Int
  getCompressionLevel :: m Int
  setCompressionLevel :: Int -> m ()


theDict :: forall m . ( HBS2GitPerks m
                      , HasClientAPI PeerAPI UNIX m
                      , HasStorage m
                      , HasGitRemoteKey m
                      , HasStateDB m
                      , MonadReader Git3Env m
                      ) => Dict C m
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

        entry $ bindMatch "test:state:init" $ nil_ $ \case
          [ ] -> do
            lift $ connectedDo do
              r <- getGitRemoteKey >>= orThrowUser "git remote not set"
              p <- getStatePathDB (AsBase58 r)
              debug $ "test:state:init" <+> pretty p

          _ -> throwIO (BadFormException @C nil)

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

        entry $ bindMatch "test:git:read-commit-chain" $ nil_ $ \syn -> do
          (mpath, hss) <- case syn of
            [ HashLike s ] -> pure (Nothing, s)
            [ StringLike path , HashLike s ] -> pure (Just path, s)
            [ StringLike path ] -> pure (Just path, "HEAD")
            [] -> pure (Nothing, "HEAD")
            _ -> throwIO (BadFormException @C nil)
          liftIO $ mapM_ setCurrentDirectory mpath
          -- let hss = headDef "HEAD" [ x | StringLike x <- snd (splitOpts [] syn) ]
          h <- gitRevParseThrow hss
          r <- lift $ readCommitChainHPSQ (const $ pure True) Nothing h dontHandle
          liftIO $ print ( HPSQ.size r )

        entry $ bindMatch "test:git:read-commit-chain-dfs" $ nil_ $ \syn -> lift do
          let (_, argz) = splitOpts [] syn
          let hd = headDef "HEAD" [ x | StringLike x <- argz]
          h <- gitRevParseThrow hd
          r <- readCommitChainHPSQ (const $ pure True) Nothing h (\c -> debug $ "commit" <+> pretty c)
                  <&> HPSQ.toList
                  <&> sortBy (comparing (view _2))
          for_ r $ \(c,_,_) -> do
            liftIO $ print $ pretty c


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

        entry $ bindMatch "test:git:log:index:flat:search:binary:test" $ nil_ \case
          [ StringLike fn ] -> do

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

                  lift $ S.yield hash
                  go (succ n)

            file <- liftIO $ mmapFileByteString fn Nothing

            for_ hashes $ \h -> do
             -- found <- binSearchBS 24 (BS.take 20 . BS.drop 4) ( show . pretty . GitHash ) (coerce h) file
              found <- liftIO $ binarySearchBS 24 (BS.take 20 . BS.drop 4) (coerce h) file
              liftIO $ print $ pretty h <+> pretty (isJust found)

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:log:index:flat:search:binary" $ nil_ $ \syn -> lift do
          let (_, argz) = splitOpts [] syn

          let argzz = [ x | StringLike x <- argz ]

          hash <- headMay argzz
                    >>= fromStringMay @GitHash
                    & orThrowUser "no hash specified"

          idxName <- headMay (tail argzz)
                        & orThrowUser "no index specified"

          file <- liftIO $ mmapFileByteString idxName Nothing
          r <- liftIO $ binarySearchBS 24 (BS.take 20 . BS.drop 4) (coerce hash) file

          liftIO $ print $ pretty r

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

        case (compare hash1 (coerce hash)) of
          EQ -> lift $ S.yield n
          _  -> go (succ n)

  pure $ listToMaybe found


binarySearchBS :: Monad m
             => Int           -- ^ record size
             -> ( BS.ByteString -> BS.ByteString ) -- ^ key extractor
             -> BS.ByteString -- ^ key
             -> BS.ByteString -- ^ source
             -> m (Maybe Int)

binarySearchBS rs getKey s source = do
  let maxn = BS.length source `div` rs
  loop 0 maxn
  where
    loop l u | u <= l = pure Nothing
             | otherwise = do
                 let e = getKey (BS.drop ( k * rs ) source)
                 case compare e s of
                  EQ -> pure $ Just (k * rs)
                  LT -> loop (k+1) u
                  GT -> loop l k

      where k = (l + u) `div` 2

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

