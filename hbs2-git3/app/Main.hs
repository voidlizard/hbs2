{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

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

-- import Codec.Compression.GZip as GZ1
-- import Codec.Compression.Zlib.Internal qualified as GZ

import Codec.Compression.BZip as BZ1
import Codec.Compression.BZip.Internal qualified as BZ
-- import Codec.Compression.Zlib.Internal qualified as GZ

import Data.HashPSQ qualified as HPSQ
import Data.Maybe
import Data.List qualified as L
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Text.InterpolatedString.Perl6 (qc)
import Data.HashSet qualified as HS
import Data.HashSet (HashSet(..))
import Data.HashMap.Strict qualified as HM
import Data.Word

import Streaming.Prelude qualified as S
import System.Exit qualified as Q
import System.Environment qualified as E
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import System.IO (hPrint,hGetLine,IOMode(..))
import System.IO qualified as IO

import Data.Coerce
import Data.Kind
import Data.List (sortOn)
import Data.Ord (Down(..))

import UnliftIO

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

type HBS2GitPerks m = (MonadUnliftIO  m)

quit :: MonadUnliftIO m => m ()
quit = liftIO Q.exitSuccess

data GitException =
    CompressionError String
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
     >>= orThrowPassIO
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

        _ -> mzero

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

-- instance (MonadUnliftIO m, HasClientAPI api proto m) => HasClientAPI api proto (RunM c m) where
--   getClientAPI = lift (getClientAPI @api @proto)

-- instance (MonadUnliftIO m, HasClientAPI api proto m) => HasClientAPI api proto (ContT a (RunM c m)) where
--   getClientAPI = lift $ getClientAPI @api @proto

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

gitWriteCommitPackIO :: (GitWritePacksOpts opt, GitObjectReader reader, Pretty what) => opt -> reader -> what -> ( BS.ByteString -> IO () ) -> IO ()
gitWriteCommitPackIO opts reader what action = do
    hhead <- gitRevParse what >>= orThrow (OtherGitError $ show $ "can't parse" <+> pretty what)

    parents <- gitReadObjectThrow Commit hhead >>= gitReadCommitParents

    skip <- if not (excludeParents opts) then do
                 pure mempty
               else do
                  skip' <- S.toList_ $ for parents $ \p -> do
                            gitReadTree p <&> fmap gitEntryHash >>= S.each
                  pure $ HS.fromList skip'


    r <- gitReadTree hhead
           <&> L.filter (\GitTreeEntry{..} -> not (HS.member gitEntryHash skip))
           <&> sortGitTreeEntries

    flip runContT pure do

      inq <- newTQueueIO

      atomically do
        writeTQueue inq (Commit, hhead)
        for_ r $ \GitTreeEntry{..} -> do
          writeTQueue inq (gitEntryType, gitEntryHash)

      let params = defaultCompressParams
      let compressStream = BZ.compressIO params

      lift $ flip fix compressStream $ \go -> \case
        BZ.CompressInputRequired next -> do

         inO <- atomically $ tryReadTQueue inq

         case inO of
          Nothing -> go =<< next mempty

          Just (t,ha) -> do
            (tt,bs) <- gitReadObjectMaybe reader ha >>= orThrow (GitReadError (show $ pretty ha))
            let header = [qc|{pretty (Short tt)} {pretty $ LBS.length bs} {pretty ha}|]
            go =<< next (LBS.toStrict (LBS8.intercalate "\n" [header, bs]))

        BZ.CompressOutputAvailable outchunk next -> do
         action outchunk
         go =<< next

        BZ.CompressStreamEnd -> pure ()

data UState =
  UHead ByteString

pattern PEntryView :: GitObjectType -> Word32 -> GitHash -> [ByteString]
pattern PEntryView t s h <- ( unpackPEntry -> Just (t,s,h) )

unpackPEntry :: [ByteString] -> Maybe (GitObjectType, Word32, GitHash)
unpackPEntry = \case
  ["C", s, h] -> (Commit,,) <$> readMay (LBS8.unpack s) <*> fromStringMay (LBS8.unpack h)
  ["B", s, h] -> (Blob,,)   <$> readMay (LBS8.unpack s) <*> fromStringMay (LBS8.unpack h)
  ["T", s, h] -> (Tree,,)   <$> readMay (LBS8.unpack s) <*> fromStringMay (LBS8.unpack h)
  _ -> Nothing

data ExportState =
    ExportGetCommit
  | ExportCheck
  | ExportStart

data WState =
    WStart
  | WReadSBlock HashRef
  | WCheckSBlock HashRef ByteString
  | WWalkSBlock HashRef (MTree [HashRef])
  | WGetInput
  | WEnd

data WInput =
    WInputSBlock
  | WInputCBlock

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
            liftIO $ print $ pretty gitEntryHash <+> pretty gitEntryType <+> pretty gitEntrySize <+> pretty gitEntryName


        entry $ bindMatch "reflog" $ nil_ $ \case
          [ SignPubKeyLike what ] -> do
            debug $ "set reflog" <+> pretty (AsBase58 what)
            lift $ setGitRemoteKey what

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:state:init" $ nil_ $ \case
          [ ] -> do
            lift $ connectedDo do
              r <- getGitRemoteKey >>= orThrowUser "git remote not set"
              p <- getStatePathDB (AsBase58 r)
              debug $ "test:state:init" <+> pretty p

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:tree:pack:dump" $ nil_ $ \case
          [ StringLike fn ] -> do

            content <- BZ.decompress defaultDecompressParams <$> liftIO (LBS8.readFile fn)

            flip fix (UHead content) $ \next -> \case
              UHead "" -> none
              UHead bs -> do

                let (hd,rest) = LBS8.span (/='\n') bs

                case LBS8.words hd of
                  PEntryView t s h -> do
                    liftIO $ print $ pretty h <+> pretty t <+> pretty s
                    next $ UHead (LBS8.drop (1 + fromIntegral s) rest)

                  _ -> throwIO (InvalidGitPack hd)

          _ -> throwIO (BadFormException @C nil)


        entry $ bindMatch "test:hbs2:peer:poke" $ nil_ $ \syn -> do
          peer <- getClientAPI @PeerAPI @UNIX
          r    <- callRpcWaitRetry @RpcPoke (TimeoutSec 0.5) 2 peer () >>= orThrowUser "hbs2-peer not found"
          notice $ pretty r


        entry $ bindMatch "test:git:tree:pack:write" $ nil_ $ \syn -> flip runContT pure do

          let o = [ WriteFullPack | StringLike "--full" <- syn ] & HS.fromList

          (what,to) <- case syn of

             ( StringLike rev : StringLike fn : _) -> do
              -- let co = headDef "HEAD" $ [ GitRef (LBS8.toStrict $ LBS8.pack what) | StringLike what  <- syn ]
               fh <- ContT $ bracket (liftIO (IO.openFile fn WriteMode)) hClose
               pure (rev, fh)

             ( StringLike rev : _ ) -> pure  ( rev, stdout )

             _ -> pure ( "HEAD", stdout )

          rd <- ContT withGitCat

          liftIO $ gitWriteCommitPackIO o rd what $ \bs -> do
            BS.hPut to bs


        entry $ bindMatch "test:git:tree:walk" $ nil_ $ \syn -> do
          sref <- case syn of
                    [ HashLike s ] -> pure s
                    _ -> throwIO (BadFormException @C nil)

          sto <- lift getStorage
          lift $ connectedDo $ flip runContT pure $ do

            _p  <- newTVarIO 0
            wq  <- newTVarIO ( HPSQ.empty @HashRef @Int @WInput )
            notice $ "sblock" <+> pretty sref

            atomically $ modifyTVar wq (HPSQ.insert sref 0 WInputSBlock)

            flip fix WGetInput \next -> \case
              WStart -> do
                debug $ "start" <+> pretty sref
                next WEnd --  (WReadSBlock sref)

              WReadSBlock h -> do
                blk' <- getBlock sto (coerce h)
                maybe1 blk' (next WEnd) (next . WCheckSBlock h)

              WCheckSBlock h bs -> do
                let what = tryDetect (coerce h) bs
                case what of
                  Merkle mt -> next (WWalkSBlock h mt)
                  _ -> next WEnd

              WWalkSBlock self x -> case x of
                MLeaf ( (c:parents) :: [HashRef]) -> do

                  debug $ "walk sblock yay!" <+> pretty self <+> pretty parents
                  debug $ "sblok content" <+> pretty c

                  atomically do
                    p0 <- stateTVar _p $ \x -> (x, pred x)
                    for_ (zip [1 ..] parents) $ \(i,p) -> do
                      modifyTVar _p $ \x -> x - i
                      modifyTVar wq (HPSQ.insert p (p0-i) WInputSBlock)

                    modifyTVar wq (HPSQ.insert c (p0+1) WInputCBlock)

                  next WGetInput

                _ -> next WEnd

              WGetInput -> do
                n <- readTVarIO wq <&> HPSQ.size
                debug $ "get input!" <+> pretty n

                inp <- atomically $ stateTVar wq $ HPSQ.alterMin \case
                         Nothing      -> (Nothing, Nothing)
                         Just (k,p,v) -> (Just (k,p,v), Nothing)

                case inp of
                  Just (h, _, WInputSBlock) -> do
                    debug $ "goto sblock" <+> pretty h
                    next (WReadSBlock h)

                  Just (h, _, WInputCBlock) -> do
                    debug $ "process cblock" <+> pretty h
                    next WGetInput

                  Nothing -> next WEnd

              WEnd -> do
                debug "exit"


        entry $ bindMatch "test:git:tree:export" $ nil_ $ \syn -> do

            mkdir "export"

            r <- case syn of
                    [] -> gitRevParseThrow "HEAD"
                    [ StringLike co ] -> gitRevParseThrow co
                    _ -> throwIO (BadFormException @C nil)

            debug $ "process commit"  <+> pretty r

            q <- newTVarIO ( HPSQ.empty @GitHash @Double @() )
            done <- newTVarIO ( mempty :: HashSet GitHash )

            atomically $ modifyTVar q (HPSQ.insert r 1.0 ())

            lift $ connectedDo do

              sto <- getStorage

              flip runContT pure do

                reader <- ContT $ withGitCat

                ContT $ bracket none $ const do
                  hClose $ getStdin reader

                flip fix ExportGetCommit $ \next -> \case

                  ExportStart -> do
                    here <- lift $ withState $ selectGitPack r <&> isJust
                    if here then next ExportCheck else next ExportGetCommit

                  ExportGetCommit -> do

                    co' <- atomically $ stateTVar q $ HPSQ.alterMin \case
                            Nothing      -> (Nothing, Nothing)
                            Just (k,p,v) -> (Just (k,p), Nothing)

                    case co' of
                      Nothing -> next ExportCheck

                      Just (co,prio) -> do
                        debug $ "Process commit" <+> pretty co
                        debug $ "check-pack-for" <+> pretty prio <+> pretty co

                        isDone <- readTVarIO done <&> HS.member co

                        let already = isDone

                        if already
                          then next ExportGetCommit
                          else do
                            (t,bs) <- liftIO (gitReadObjectMaybe reader co)
                                          >>= orThrow (GitReadError (show $ pretty co))

                            parents <- gitReadCommitParents bs

                            n <- for (zip [1..] parents) $ \(i,gh) -> do
                                   here <- lift $ withState $ selectGitPack gh <&> isJust
                                   atomically do
                                     pdone <- readTVar done <&> HS.member gh
                                     if pdone || here then do
                                       pure 0
                                     else do
                                       modifyTVar q (HPSQ.insert gh (prio-i) ())
                                       pure 1

                            if sum n == 0 then lift do
                              debug $ "write pack for" <+> pretty co

                              let fn = "export" </> show (pretty co) <> ".pack"

                              liftIO $ withFile fn WriteMode $ \to -> do
                                gitWriteCommitPackIO () reader co $ \pss -> do
                                  BS.hPut to pss

                              -- FIXME: support-encryption!
                              lbs <- liftIO $ LBS.readFile fn

                              href <- createTreeWithMetadata sto mzero mempty lbs
                                        >>= orThrowUser "can't write merkle tree"

                              debug $ "pack-merkle-tree-hash" <+> pretty href

                              debug $ "make cblock"

                              phashes <- withState $ for parents \p -> do
                                             selectCBlock p
                                              >>= orThrowUser ("pack export failed" <+> pretty p)

                              debug $ "write cblock"

                              let cblock = href : phashes
                              let pt = toPTree (MaxSize 1024) (MaxNum 1024) cblock

                              root <- makeMerkle 0 pt $ \(_,_,s) -> do
                                         void $ putBlock sto s

                              withState $ transactional do
                                insertGitPack co href
                                insertCBlock co (HashRef root)

                              atomically $ modifyTVar done (HS.insert co)
                            else do
                              atomically $ modifyTVar q (HPSQ.insert co prio ())

                            next ExportGetCommit

                  ExportCheck -> do
                    debug $ "ExportCheck dummy" <+> pretty r
                    c <- lift $ withState $ selectCBlock r >>= orThrowUser "export failed"
                    liftIO $ hPrint stdout (pretty c)


-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
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
    recover $ run dict (conf <> cli)

