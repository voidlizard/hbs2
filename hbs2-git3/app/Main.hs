{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language MultiWayIf #-}
{-# Language FunctionalDependencies #-}
{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
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

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2.CLI.Run.Internal.Merkle (createTreeWithMetadata)

import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff as Exported

import HBS2.Git3.Types
import HBS2.Git3.State.Direct
import HBS2.Git3.Config.Local

import Data.Config.Suckless.Script
import DBPipe.SQLite

import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Streaming qualified as ZstdS
import Codec.Compression.Zstd.Streaming (Result(..))
import Codec.Compression.Zstd (maxCLevel)

import Codec.Compression.Zlib qualified as Zlib

import qualified Data.Attoparsec.ByteString as A
import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ (HashPSQ)
import Data.Maybe
import Data.List qualified as L
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder as Builder
import Text.InterpolatedString.Perl6 (qc)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.IntMap qualified as IntMap
import Data.HashSet qualified as HS
import Data.HashSet (HashSet(..))
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict (HashMap(..))
import Data.Word

import Streaming.Prelude qualified as S
import System.Exit qualified as Q
import System.Environment qualified as E
import System.Process.Typed
import Control.Applicative
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent.STM qualified as STM
import System.IO (hPrint,hGetLine,IOMode(..))
import System.IO qualified as IO

import Data.Either
import Data.Coerce
import Data.Kind
import Data.List (sortOn)
import Data.Ord (Down(..))

import UnliftIO
import UnliftIO.IO.File qualified as UIO

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

type HBS2GitPerks m = (MonadUnliftIO  m)

quit :: MonadUnliftIO m => m ()
quit = liftIO Q.exitSuccess

class Cached cache k v | cache -> k, cache -> v where
  isCached :: forall m . MonadIO m => cache -> k -> m Bool
  cached   :: forall m . MonadIO m => cache -> k -> m v -> m v
  uncache  :: forall m . MonadIO m => cache -> k -> m ()

data GitException =
    CompressionError String
  | DecompressionError String
  | InvalidObjectFormat GitObjectType (Maybe GitHash)
  | InvalidGitPack ByteString
  | OtherGitError String
  | UnknownRev String
  | GitReadError String
  deriving stock (Eq,Show,Typeable,Generic)

instance Exception GitException

data GitTreeEntry =
  GitTreeEntry
  { gitEntryAttribs :: Word16
  , gitEntryType    :: GitObjectType
  , gitEntryHash    :: GitHash
  , gitEntrySize    :: Maybe Word32
  , gitEntryName    :: FilePath
  }

pattern GitTreeEntryView :: GitTreeEntry -> [ByteString]
pattern GitTreeEntryView e <- (isGitLsTreeEntry -> Just e)

gitNormaliseRef :: GitRef -> GitRef
gitNormaliseRef r@(GitRef what) =
  if BS8.isPrefixOf "refs/" what || what == "HEAD" then
    r
  else
    fromString (joinPath $ splitPath $ "refs" </> "heads" </> BS8.unpack what)

isGitLsTreeEntry :: [ByteString] -> Maybe GitTreeEntry
isGitLsTreeEntry = \case
  [sa,st,sh,ss,sn] -> do
    GitTreeEntry <$> readMay @Word16 (LBS8.unpack sa)
                 <*> fromStringMay (LBS8.unpack st)
                 <*> fromStringMay (LBS8.unpack sh)
                 <*> pure (readMay (LBS8.unpack ss))
                 <*> pure (LBS8.unpack sn)

  _ -> Nothing

gitReadTree :: (Pretty what, MonadIO m) => what -> m [GitTreeEntry]
gitReadTree what =
  gitRunCommand [qc|git ls-tree -t -l -r {pretty what}|]
     >>= orThrow (GitReadError (show $ pretty what))
     <&> fmap LBS8.words . LBS8.lines
     <&> mapMaybe \case
          GitTreeEntryView v -> do
            Just v
          _ -> Nothing
     <&> \s -> HM.elems (HM.fromList [ (gitEntryHash e, e) | e <- s])


class GitObjectReader a where
  gitReadObjectMaybe :: forall m . MonadIO m => a -> GitHash -> m (Maybe (GitObjectType, ByteString))

gitReadObjectThrow :: (Pretty h, MonadIO m) => GitObjectType -> h -> m ByteString
gitReadObjectThrow t h = do
  gitRunCommand [qc|git cat-file {pretty t} {pretty h}|]
    >>= orThrowPassIO

gitRevParse :: (Pretty ref, MonadIO m) => ref -> m (Maybe GitHash)
gitRevParse ref = do
  gitRunCommand [qc|git rev-parse {pretty ref}|]
    >>= orThrowPassIO
    <&> LBS8.words
    <&> maybe Nothing (fromStringMay . LBS8.unpack) . headMay

gitRevParseThrow :: (Pretty ref, MonadIO m) => ref -> m GitHash
gitRevParseThrow r = gitRevParse r >>= orThrow (UnknownRev (show $ pretty r))

gitReadHEAD :: MonadIO m => m (Maybe GitRef)
gitReadHEAD = runMaybeT do
  gitRunCommand [qc|git symbolic-ref HEAD|]
   >>= toMPlus
   <&> headMay . LBS8.lines
   >>= toMPlus
   <&> GitRef . LBS8.toStrict

withGitCat ::  (MonadIO m) => (Process Handle Handle () -> m a) -> m a
withGitCat action = do
  let cmd = "git"
  let args = ["cat-file", "--batch"]
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args
  p <- startProcess config
  action p

instance GitObjectReader (Process Handle Handle ()) where
  gitReadObjectMaybe ph co = liftIO do

    let ssin = getStdin ph
    let ssout = getStdout ph

    hPrint ssin $ pretty co
    hFlush ssin

    s <- hGetLine ssout

    runMaybeT do

      case words s of
        [_,t,ss] -> do
          n <- readMay @Int ss & toMPlus
          o <- fromStringMay @GitObjectType t & toMPlus
          bs <- lift $ LBS.hGet ssout n
          void $ lift $ hGetLine ssout
          pure (o,bs)

        [_,"missing"] -> do
          mzero

        w -> throwIO (GitReadError $ show (pretty w))

newtype Short x = Short x

instance Pretty (Short GitObjectType) where
  pretty = \case
    (Short Tree)   -> "T"
    (Short Blob)   -> "B"
    (Short Commit) -> "C"


sortGitTreeEntries :: [GitTreeEntry] -> [GitTreeEntry]
sortGitTreeEntries = sortOn (\e -> (gitEntryType e, gitEntrySize e))

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
    }
  | Git3Connected
    { stateDb     :: DBPipeEnv
    , peerSocket  :: FilePath
    , peerStorage :: AnyStorage
    , peerAPI     :: ServiceCaller PeerAPI UNIX
    , gitRefLog   :: TVar (Maybe GitRemoteKey)
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
                                    )

type Git3Perks m = ( MonadIO m
                   , MonadUnliftIO m
                   )


instance MonadUnliftIO m => HasClientAPI PeerAPI UNIX (Git3 m) where
  getClientAPI = do
    ask  >>= \case
       Git3Disconnected{} -> throwIO Git3PeerNotConnected
       Git3Connected{..} -> pure peerAPI

nullGit3Env :: MonadIO m => m Git3Env
nullGit3Env = Git3Disconnected <$> newTVarIO Nothing

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

        connected <- Git3Connected db soname sto peerAPI <$> newTVarIO (Just ref)

        liftIO $ withGit3Env connected (evolveState >> again)

gitReadCommitParents :: MonadIO m => ByteString -> m [GitHash]
gitReadCommitParents bs = do
   what <- LBS8.lines bs
             & takeWhile ( not . LBS8.null )
             & LBS8.unpack . LBS8.unlines
             & parseTop
             & orThrow (OtherGitError "invalid commit format")

   pure $ [ fromStringMay @GitHash hash
          | ListVal [ StringLike "parent", StringLike hash ] <- what
          ] & catMaybes


gitReadCommitTree :: MonadIO m => ByteString -> m GitHash
gitReadCommitTree bs = do

   what <- LBS8.lines bs
             & takeWhile ( not . LBS8.null )
             & LBS8.unpack . LBS8.unlines
             & parseTop
             & orThrow (OtherGitError "invalid commit format")

   let r = [ fromStringMay @GitHash hash
           | ListVal [ StringLike "tree", StringLike hash ] <- what
           ]

   catMaybes r & headMay & orThrow (InvalidObjectFormat Commit Nothing)

gitObjectExists :: (MonadIO m, Pretty what) => what -> m Bool
gitObjectExists what = do
  gitRunCommand [qc|git cat-file -e {pretty what}|] <&> isRight

data UState =
  UHead Word32 ByteString

data IOpType
  = IGitObject GitObjectType GitHash (Maybe ByteString)
  | ISetRef GitRef Int (Maybe GitHash)
  deriving (Show, Eq)

data IOp =
  IOp { iopOffset :: Word32
      , iopSize   :: Word32
      , iopType   :: IOpType
      }
  deriving (Show, Eq)

data ES =
  ES [BS.ByteString] Result


class EnumGitPackObjectsOpts a where
  enumObjectPayload :: a -> Bool

instance EnumGitPackObjectsOpts () where
  enumObjectPayload = const False

data DoEnumPayload = DoEnumPayload

instance EnumGitPackObjectsOpts DoEnumPayload where
  enumObjectPayload = const True

enumGitPackObjectsFromLBS :: (MonadIO m, EnumGitPackObjectsOpts opts)
                          => opts
                          -> ByteString
                          -> ( IOp -> m Bool )
                          -> m ()
enumGitPackObjectsFromLBS opts lbs action = do

  let chunks = LBS.toChunks lbs

  stream <- liftIO ZstdS.decompress

  chunks <- S.toList_ do

    flip fix (ES chunks stream) $ \go -> \case
        ES _ (Error s1 s2) -> throwIO (DecompressionError (s1 <> s2))

        ES [] (Consume work) ->
          go . ES [] =<< liftIO (work mempty)

        ES (r:rs) (Consume work) -> do
          go . ES rs =<< liftIO (work r)

        ES rs (Produce s continue) -> do
          S.yield s
          go . ES rs =<< liftIO continue

        ES _ (Done s) -> do
          S.yield s

  void $ flip fix (UHead 0 (LBS.fromChunks chunks)) $ \next -> \case
    UHead off chunk -> do
      let (skipped1,s0) = LBS8.span (=='\n') chunk
      -- read += len skipped

      unless (LBS.null s0) do
        let (hdr,rest) = LBS8.break (=='\n') s0
        -- read += len hdr

        let o = LBS.drop 1 rest -- skip '\n'. read+1
        -- read += 1

        let skipped2 = fromIntegral $ LBS8.length skipped1
                                    + LBS8.length hdr
                                    + 1

        let entryOffset = off + fromIntegral skipped2

        iop@(IOp{..}) <- unpackIOp 0 (LBS8.words hdr) & orThrow (InvalidGitPack hdr)

        let (rn, rest2) = LBS.splitAt (fromIntegral iopSize) o
        -- read += len rn        --

        let consumed = fromIntegral $ skipped2 + LBS.length rn

        let pl = case ( enumObjectPayload opts, iopType ) of
                   (True, IGitObject t h _) -> IGitObject t h (Just rn)
                   (_, t) -> t

        let actualIop  = iop { iopOffset = entryOffset
                             , iopType = pl
                             }

        continue <- action actualIop

        when continue do
          next (UHead (off + consumed) rest2)

data ExportState =
    ExportGetCommit
  | ExportProcessCommit GitHash ByteString
  | ExportCheck
  | ExportStart
  | ExportExit


data EOp =
    EGitObject GitObjectType GitHash (Maybe GitTreeEntry) ByteString
  | EGitRef    GitRef Int (Maybe GitHash)


unpackIOp :: Word32 -> [ByteString] -> Maybe IOp
unpackIOp off = \case
  ("C" : s : h : _) -> do
    size <- fromLBS s
    hash <- fromLBS' h
    pure $ IOp off size (IGitObject Commit hash Nothing)

  ("B" : s : h : _) -> do
    size <- fromLBS s
    hash <- fromLBS' h
    pure $ IOp off size (IGitObject Blob hash Nothing)

  ("T" : s : h : _) -> do
    size <- fromLBS s
    hash <- fromLBS' h
    pure $ IOp off size (IGitObject Tree hash Nothing)

  ("R" : s : n : r : rest) -> do
    size <- fromLBS s
    weight <- fromLBS n
    refName <- pure (GitRef $ LBS8.toStrict r)
    hash <- case rest of
      (h : _) -> Just <$> fromStringMay (LBS8.unpack h)
      _       -> pure Nothing
    pure $ IOp off size (ISetRef refName weight hash)

  _ -> Nothing

  where
    fromLBS :: forall a . Read a => ByteString -> Maybe a
    fromLBS = readMay . LBS8.unpack

    fromLBS' :: forall a. FromStringMaybe a => ByteString -> Maybe a
    fromLBS' = fromStringMay . LBS8.unpack

data EWState =
    EWAcc Int [GitTreeEntry] Int [EOp]

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

export :: ( HBS2GitPerks m
          , MonadUnliftIO m
          , MonadReader Git3Env m
          , HasStorage m
          , HasStateDB m
          )
       => Maybe GitRef -> GitHash -> m ()
export mref' r = connectedDo $ flip runContT pure do
  debug $ green "export"  <+> pretty r

  let mref = gitNormaliseRef <$> mref'

  q <- newTVarIO ( HPSQ.empty @GitHash @Double @() )
  done <- newTVarIO ( mempty :: HashSet GitHash )

  atomically $ modifyTVar q (HPSQ.insert r 1.0 ())

  sto <- lift getStorage

  reader  <- ContT $ withGitCat

  let commitCacheSize = 2000

  commits <- newCacheFixedHPSQ commitCacheSize

  ContT $ bracket none $ const do
    hClose $ getStdin reader

  -- ContT $ withAsync $ replicateM_ 2 $ forever do
  --   join $ atomically (readTQueue deferred)

  lift $ flip fix ExportStart $ \next -> \case

    ExportStart -> do
      here <- withState $ selectCBlock r <&> isJust
      if here then next ExportCheck else next ExportGetCommit

    ExportGetCommit -> do

      co' <- atomically $ stateTVar q $ HPSQ.alterMin \case
              Nothing      -> (Nothing, Nothing)
              Just (k,p,v) -> (Just (k,p), Nothing)

      case co' of
        Nothing -> do
          debug $ red "go ExportCheck"
          next ExportCheck

        Just (co,prio) -> do
          debug $ "Process commit" <+> pretty co <+> pretty prio
          debug $ "check-pack-for" <+> pretty prio <+> pretty co

          isDone <- readTVarIO done <&> HS.member co

          let already = isDone

          if already
            then do
              next ExportGetCommit
            else do
              (_,bs) <- liftIO (cached commits co (gitReadObjectMaybe reader co))
                            >>= orThrow (GitReadError (show $ pretty co <+> pretty prio))

              parents <- gitReadCommitParents bs

              n <- for (zip [1..] parents) $ \(i,gh) -> do

                     exists <- liftIO (cached commits gh (gitReadObjectMaybe reader gh))
                                  <&> isJust

                     here <- withState $ selectCBlock gh <&> isJust

                     unless exists do
                      warn $ red "missed!" <+> pretty gh
                      -- atomically $ modifyTVar done (HS.insert gh)

                     atomically do
                       pdone <- readTVar done <&> HS.member gh
                       if pdone || here || not exists then do -- for shallow commits?
                         pure 0
                       else do
                         modifyTVar q (HPSQ.insert gh (prio-i) ())
                         pure 1

              if sum n == 0 then do
                uncache commits co
                next $ ExportProcessCommit co bs
              else do
                -- error "FUCK!"
                debug $ yellow "put commit back" <+> pretty co
                atomically $ modifyTVar q (HPSQ.insert co prio ())
                next ExportGetCommit

    ExportProcessCommit co bs -> do
      debug $ "write pack for" <+> pretty co

      l <- readTVarIO q<&> HPSQ.keys

      let lastBlock = co == r && L.null l

      hhead <- gitRevParse co
                     >>= orThrow (OtherGitError $ show $ "can't parse" <+> pretty co)

      parents <- gitReadObjectThrow Commit hhead
                    >>= gitReadCommitParents

      tree <- gitReadCommitTree bs

      skip <- if not (excludeParents ()) then do
                   pure mempty
                 else do
                    skip' <- S.toList_ $ for parents $ \p -> do
                        lift (try @_ @GitException (gitReadTree p))
                          <&> fromRight mempty
                          <&> fmap gitEntryHash >>= S.each

                    pure $ HS.fromList skip'

      r <- gitReadTree hhead
             <&> L.filter (\GitTreeEntry{..} -> L.null parents || not (HS.member gitEntryHash skip))
             -- <&> L.filter (\GitTreeEntry{..} -> gitEntryType /= Tree)
             <&> sortGitTreeEntries

      let blkMax = 1048576

      out <- newTQueueIO

      now <- liftIO getPOSIXTime <&> round

      let ref = maybeToList $ EGitRef <$> mref <*> pure now <*> pure (Just co)

      (_,tbs) <- gitReadObjectMaybe reader tree
                   >>= orThrow (GitReadError (show $ pretty tree))

      let commitItself = [EGitObject Tree tree Nothing tbs, EGitObject Commit co Nothing bs]
      let seed = (if lastBlock then ref else mempty) <> commitItself

      flip fix (EWAcc 1 r 0 seed) $ \go -> \case

        EWAcc _ [] _ [] -> none

        EWAcc i [] l acc -> do
          writePack sto l acc >>= atomically . writeTQueue out . (i,)

        EWAcc i (r@GitTreeEntry{..}:rs) l acc | gitEntrySize >= Just (fromIntegral blkMax) -> do
          writeLargeBlob sto reader r >>= atomically . writeTQueue out . (i,)
          go (EWAcc (succ i) rs l acc)

        EWAcc i rs l acc | l >= blkMax -> do
          writePack sto l acc >>=  atomically . writeTQueue out . (i,)
          go (EWAcc (succ i) rs 0 mempty)

        EWAcc i (e@GitTreeEntry{..}:rs) l acc -> do

          lbs <- gitReadObjectMaybe reader gitEntryHash
                   >>= orThrow (GitReadError (show $ pretty gitEntryHash))
                   <&> snd

          let new = EGitObject gitEntryType gitEntryHash (Just e) lbs
          go (EWAcc i rs (l + fromIntegral (LBS.length lbs)) (new : acc))


      packs <- atomically $ STM.flushTQueue out

      phashes <- catMaybes <$> withState (for parents selectCBlock)

      let v = "hbs2-git 3.0 zstd"
      let pps = vcat $ mconcat $ for phashes $ \p -> ["p" <+> pretty p]
      let meta = LBS8.pack $ show $ pretty v <> line <> pps

      hmeta <- putBlock sto meta >>= orThrow StorageError <&> HashRef

      let cblock = hmeta : uniqAndOrdered phashes <> uniqAndOrderedByKey packs

      let pt = toPTree (MaxSize 1024) (MaxNum 1024) cblock

      root <- makeMerkle 0 pt $ \(_,_,s) -> do
                 void $ putBlock sto s

      withState $ transactional do
        insertCBlock co (HashRef root)

      notice $ "cblock" <+> pretty root

      atomically do
        modifyTVar done (HS.insert co)
        modifyTVar q (HPSQ.delete co)

      next ExportGetCommit

    ExportCheck -> do
      debug $ "ExportCheck dummy" <+> pretty r
      c <- withState $ selectCBlock r >>= orThrowUser "export failed"
      liftIO $ hPrint stdout (pretty c)
      next ExportExit

    ExportExit -> none

  where

    uniqAndOrderedByKey xs = L.sortOn fst xs & uniq
      where
        uniq items = flip fix (items, mempty, mempty) $ \next -> \case
          ([],   _, acc) -> L.reverse acc
          ((_,v):es, seen, acc) | HS.member v seen -> next (es, seen, acc)
          ((_,v):es, seen, acc) -> next (es, HS.insert v seen, v:acc)

    uniqAndOrdered = Set.toList . Set.fromList

    writeLargeBlob sto reader GitTreeEntry{..} = liftIO do
      size <- gitEntrySize & orThrow (GitReadError (show $ "expected blob" <+> pretty gitEntryHash))
      let p =    Builder.byteString [qc|{pretty $ Short gitEntryType} {pretty size} {pretty gitEntryHash} {gitEntryName}|]
              <> Builder.byteString "\n"
              &  LBS.toStrict . Builder.toLazyByteString

      -- liftIO $ print $ "MOTHERFUCKER1" <+> pretty gitEntryHash

      -- TODO: check-if-work-on-large-files
      pieces <- S.toList_ do

        stream <- lift $ ZstdS.compress maxCLevel

        (t,lbs) <- gitReadObjectMaybe reader gitEntryHash
                      >>= orThrow (GitReadError (show $ pretty gitEntryHash))

        let chunks = p : LBS.toChunks lbs

        flip fix (chunks, stream) $ \go r ->
          case r of
          (c, Produce chunk continue)  -> do
            S.yield chunk
            w <- lift continue
            go (c,w)

          ([], Consume consume) -> do
            x <- lift $ consume mempty
            go ([],x)

          (s:ss, Consume consume) -> do
            x <- lift $ consume s
            go (ss,x)

          (_,Done bs) -> do
            S.yield bs

          (_,Error s1 s2) -> do
            throwIO (CompressionError (s1  <> " " <> s2))

      -- liftIO $ print $ "MOTHERFUCKER2" <+> pretty gitEntryHash

      -- TODO: check-if-work-on-large-files
      r <- createTreeWithMetadata sto mzero mempty (LBS.fromChunks pieces)
             >>= orThrowPassIO

      debug $ yellow $ "write large object" <+> pretty r <+> pretty gitEntryHash

      pure r
      -- liftIO $ print $ "WRITTEN" <+> pretty gitEntryHash <+> pretty w
      -- pure w

    writePack sto l racc = do
      -- write
      -- pack
      -- merkle
      --
      let acc = reverse racc
      debug $ green "write pack of objects" <+> pretty l <+> pretty (length acc)

      let refs =  [ Builder.byteString [qc|R 0 {w} {show $ pretty ref} {show $ pretty h}|]
                  | EGitRef ref w h <- acc
                  ] & mconcat & (<> Builder.byteString "\n")

      parts <- for [ (h,t,e,lbs) | EGitObject t h e lbs <- acc ] $ \(h,t,e,lbs) -> liftIO do
        let ename = [qc|{fromMaybe mempty $ gitEntryName <$> e}|] :: ByteString

        -- notice $ "pack" <+> pretty h <+> pretty t
        let p =    Builder.byteString [qc|{pretty $ Short t} {pretty (LBS.length lbs)} {pretty h} {ename}|]
                <> Builder.byteString "\n"
                <> Builder.lazyByteString lbs
                <> Builder.byteString "\n"
        pure p

      let packed = Zstd.compress maxCLevel (LBS.toStrict $ Builder.toLazyByteString $ refs <> mconcat parts)

      createTreeWithMetadata sto mzero mempty (LBS.fromStrict packed)
        >>= orThrowPassIO

data CBlockReadError =
    EmptyCBlock
  | BadMetaData
  | MissedCBlock
  deriving stock (Show,Eq,Typeable)

data CBlockReadException =
 CBlockReadException HashRef CBlockReadError
 deriving stock (Show,Typeable)

instance Exception CBlockReadException

data CBlockSection =
    CBlockParents [HashRef]
  | CBlockData [HashRef]

readCBlock :: forall m . ( MonadIO m
                         )
           => AnyStorage
           -> HashRef
           -> ( CBlockSection  -> m () )
           -> m ()

readCBlock sto hash action = do

    hzz <- S.toList_ $ walkMerkle (coerce hash) (getBlock sto)  $ \case
      Left h -> throwIO MissedBlockError
      Right ( hs :: [HashRef] ) -> S.each hs

    hmeta <- headMay hzz & orThrow (CBlockReadException hash EmptyCBlock)

    what <- getBlock sto (coerce hmeta)
               >>= orThrow (CBlockReadException hmeta BadMetaData)
               <&> LBS8.unpack
               <&> parseTop
               <&> fromRight mempty

    _ <- headMay [ ()
                 | ListVal [ StringLike "hbs2-git", _, StringLike "zstd" ] <- what
                 ] & orThrow (CBlockReadException hash BadMetaData)

    let pps = [ ph
              | ListVal [ StringLike "p", HashLike ph ] <- what
              ] & HS.fromList

    let rs = filter (\x -> not (HS.member x pps)) (tail hzz)

    action $ CBlockParents (HS.toList pps)
    action $ CBlockData rs

listOnlyCommitsFromCBlock :: forall m . MonadIO m
                          => AnyStorage
                          -> HashRef
                          -> m [GitHash]

listOnlyCommitsFromCBlock sto cblock = do
  cbs <- S.toList_ $ readCBlock sto cblock $ \case
            CBlockData rs -> S.each rs
            _ -> none

  S.toList_ $ flip runContT pure $ callCC \exit -> do
    for_ cbs  $ \c -> do
      what <- liftIO $ runExceptT (getTreeContents sto c) >>= orThrowPassIO
      enumGitPackObjectsFromLBS () what $ \case
        IOp _ _ (IGitObject Commit h _) -> lift (S.yield h) >> pure True
        IOp _ _ (ISetRef{}) -> pure True
        _ -> exit ()

data WState =
    WStart
  | WNextSBlock
  | WReadSBlock Int HashRef
  | WProcessCBlock Int HashRef

traverseToCBlock :: forall m . MonadIO m
           => AnyStorage
           -> HashRef
           -> ( HashRef -> m Bool )
           -> ( Int -> HashRef -> [HashRef] -> m () )
           -> m ()
traverseToCBlock sto cblock dig process = do

  q <- newTVarIO ( HPSQ.empty @HashRef @Int @() )
  done <- newTVarIO ( mempty :: HashSet HashRef )
  cache <- newCacheFixedHPSQ 1000

  flip fix WStart $ \next -> \case
    WStart -> do
      atomically $ modifyTVar q (HPSQ.insert cblock 1 ())
      next WNextSBlock

    WNextSBlock -> do

      blk' <- atomically $ stateTVar q $ HPSQ.alterMin \case
                Nothing      -> (Nothing, Nothing)
                Just (k,p,_) -> (Just (k,p), Nothing)

      debug $ "WNextSBlock" <+> pretty blk'

      maybe1 blk' none $ \(k,p) -> do
        next (WReadSBlock p k)

    WReadSBlock prio h -> do
      debug $ "WReadSBlock" <+> pretty h

      deeper <- dig h

      if not deeper then do
        atomically $ modifyTVar done ( HS.insert h )
        next WNextSBlock
      else do
        sections <- cached cache h $ S.toList_ (readCBlock sto h S.yield)

        for_ sections $ \case
          CBlockData _ -> none
          CBlockParents p -> do
            debug $ "parents" <+> pretty p
            next =<< atomically do
              d <- readTVar done
              for_ (zip [1..] p) $ \(i,x) -> do
                unless (HS.member x d) do
                  modifyTVar q (HPSQ.insert x (prio-i) ())

              let hDone = HS.member h d

              unless hDone do
                modifyTVar q (HPSQ.insert h prio ())

              qq <- readTVar q
              if not (any (`HPSQ.member` qq) p) && not hDone then do
                pure $ WProcessCBlock prio h
              else do
                pure WNextSBlock

    WProcessCBlock i h -> do
      what <- cached cache h $ S.toList_ (readCBlock sto h S.yield)
      atomically $ modifyTVar done ( HS.insert h )
      uncache cache h

      debug $ "process cblock" <+> pretty h

      for_ what \case
        CBlockParents{} -> do
          none

        CBlockData hrefs -> do
          process i h hrefs

      next $ WNextSBlock

indexCBlockCommits :: forall m . ( MonadIO m
                          , HasStateDB m
                          , HasStorage m
                          )
           => HashRef -> m ()

indexCBlockCommits cb = do
  pure ()

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
  where

    myHelpEntry = do
        entry $ bindMatch "--help" $ nil_ $ \case
          HelpEntryBound what -> do
            helpEntry what
            quit

          _ -> helpList False Nothing >> quit

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

        entry $ bindMatch "test:git:cblock:list:only:commits" $ nil_ $ \case
          [ HashLike ha ] -> lift do
            sto <- getStorage
            co <- listOnlyCommitsFromCBlock sto ha
            liftIO $ mapM_ ( print . pretty ) co

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:cblock:list" $ nil_ $ \syn -> lift do
          hash <- headMay [ x | HashLike x <- syn ] & orThrowUser "cblock hash not given"

          sto <- getStorage

          liftIO do

            readCBlock sto hash $ \case
              CBlockParents{} -> none
              CBlockData rs -> do
                for_ rs $ \r -> do
                  what <- runExceptT (getTreeContents sto r) >>= orThrowPassIO
                  debug $ yellow "reading" <+> pretty r
                  enumGitPackObjectsFromLBS () what $ \case
                    IOp o s (IGitObject t h _) -> do
                     putStrLn $ show $ pretty t <+> pretty h <+> pretty o <+> pretty s
                     pure True

                    IOp _ _ (ISetRef ref w h ) -> do
                      putStrLn $ show $ "ref" <+> pretty ref <+> pretty w <+> pretty h
                      pure True


        entry $ bindMatch "test:git:cblock:object:cat" $ nil_ $ \case
          [ HashLike cblock, StringLike g ] -> lift do

            sto <- getStorage
            h0 <- fromStringMay @GitHash g & orThrowUser "invalid git hash"

            readCBlock sto cblock $ \case
              CBlockParents{} -> none
              CBlockData rs -> do
                for_ rs $ \r -> do
                  what <- runExceptT (getTreeContents sto r) >>= orThrowPassIO
                  debug $ yellow "reading" <+> pretty r
                  enumGitPackObjectsFromLBS DoEnumPayload what $ \case

                    IOp _ _ (IGitObject _ h (Just body)) | h == h0 -> do
                      liftIO $ LBS.putStr body
                      pure False

                    _ -> pure True

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "zlib:deflate" $ nil_ $ const $ liftIO do
          LBS.hGetContents stdin <&> Zlib.decompress >>= LBS.hPutStr stdout

        entry $ bindMatch "test:git:cblock:import" $ nil_ $ \syn -> lift do
          let opts = splitOpts [("--deep",0),("--shallow",0),("--dry",0)] syn

          d <- findGitDir >>= orThrowUser "not a git directory"

          debug $ pretty opts

          debug $ "DIR" <+> pretty d

          cb  <- [ x | HashLike x <- snd opts ] & headMay & orThrowUser "import: cblock not set"

          indexCBlockCommits cb

          let shallow = or [ True | ListVal [StringLike "--shallow"] <- fst opts ]
          let deep    = or [ True | ListVal [StringLike "--deep"]    <- fst opts ] && not shallow

          debug $ "cblock" <+> pretty deep <+> pretty cb

          sto <- getStorage

          let whatever cblock = do
                co <- listOnlyCommitsFromCBlock sto cblock
                e <- mapM gitObjectExists co <&> and
                debug $ "WHATEVER" <+> pretty e <+> pretty cb <+> pretty co
                pure $ not e

          traverseToCBlock sto cb whatever  $ \i h hs -> do
            debug $ green "process cblock data" <+> pretty i <+> pretty h

            for_ hs $ \hx -> do

              what <- runExceptT (getTreeContents sto hx) >>= orThrowPassIO

              enumGitPackObjectsFromLBS DoEnumPayload what $ \case

                IOp _ s (IGitObject t h (Just body)) -> liftIO do
                  let signature = [qc|{pretty t} {pretty s}|] <> "\x00" :: LBS8.ByteString
                  let gitHash = show $ pretty h
                  let (prefix,name) = L.splitAt 2 gitHash
                  let path = joinPath [d, "objects", prefix, name]

                  here <- doesPathExist path

                  unless here do

                    touch path

                    debug $ pretty t <+> pretty s <+> pretty h <+> pretty path

                    let params = Zlib.defaultCompressParams { Zlib.compressMethod = Zlib.deflateMethod }
                    UIO.withBinaryFileAtomic path WriteMode $ \fh -> do
                      let contents = Zlib.compressWith params (signature <> body)
                      LBS.hPutStr fh contents

                  pure True

                IOp _ _ (ISetRef ref w h ) -> do
                  let path = d </> show (pretty $ gitNormaliseRef ref)
                  touch path
                  UIO.writeBinaryFileAtomic path ( BS8.pack ( show (pretty h) <> "\n" ) )
                  pure True

                _ -> pure True

              pure ()

        entry $ bindMatch "test:git:cblock:size:deep" $ nil_ $ \case
          [ HashLike cblock ] -> lift do

            sto <- getStorage

            _s <- newTVarIO 0

            deepScan ScanDeep
                (\_ -> throwIO MissedBlockError)
                (coerce cblock)
                (liftIO . getBlock sto)
                $ \h -> do
                    blk <- hasBlock sto h <&> fromMaybe 0
                    atomically $ modifyTVar _s (+ blk)

            s <- readTVarIO _s
            notice $ pretty s

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:cblock:blocks:all" $ nil_ $ \case
          [ HashLike cblock ] -> lift do

            sto <- getStorage

            blkz <- S.toList_ $ deepScan ScanDeep
                                        (\_ -> throwIO MissedBlockError)
                                        (coerce cblock)
                                        (liftIO . getBlock sto)
                                        S.yield
            for_ blkz $ \b -> do
              s <- fromMaybe 0 <$> hasBlock sto b
              notice $ pretty b <+> pretty s

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:cblock:scan" $ nil_ $ \case
          [ HashLike cblock ] -> lift do

            sto <- getStorage

            let whatever _ = pure True

            traverseToCBlock sto cblock whatever  $ \i h hs -> do
              notice $ pretty i <+> pretty h <+> pretty (length hs)

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:tree:export" $ nil_ $ \syn -> lift do


            (w, r) <- case syn of
                    [] -> (Nothing,) <$> gitRevParseThrow "HEAD"
                    [ StringLike co ] -> (Just (fromString co),) <$> gitRevParseThrow co
                    _ -> throwIO (BadFormException @C nil)

            let re = headMay [ GitRef (BS8.pack x) | ListVal [StringLike "--ref", StringLike x ] <- syn ]
            hd <- gitReadHEAD

            export (w <|> re <|> hd) r

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

