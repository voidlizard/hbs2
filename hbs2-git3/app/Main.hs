{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie

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

import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff as Exported

import Data.Config.Suckless.Script

-- import Codec.Compression.GZip as GZ1
-- import Codec.Compression.Zlib.Internal qualified as GZ

import Codec.Compression.BZip as BZ1
import Codec.Compression.BZip.Internal qualified as BZ
-- import Codec.Compression.Zlib.Internal qualified as GZ

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
import Control.Monad.Reader
import System.IO (hPrint,hGetLine,IOMode(..))
import System.IO qualified as IO

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

withGitCat ::  (MonadIO m) => (Process Handle Handle () -> m a) -> m a
withGitCat action = do
  let cmd = "git"
  let args = ["cat-file", "--batch"]
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args
  p <- startProcess config
  action p

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
  | Git3Connected
    { peerSocket  :: FilePath
    , peerAPI :: ServiceCaller PeerAPI UNIX
    }

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
       Git3Disconnected -> throwIO Git3PeerNotConnected
       Git3Connected{..} -> pure peerAPI

instance (MonadUnliftIO m, HasClientAPI api proto m) => HasClientAPI api proto (RunM c m) where
  getClientAPI = lift (getClientAPI @api @proto)

instance (MonadUnliftIO m, HasClientAPI api proto m) => HasClientAPI api proto (ContT a (RunM c m)) where
  getClientAPI = lift $ getClientAPI @api @proto

nullGit3Env :: MonadIO m => m Git3Env
nullGit3Env = pure Git3Disconnected

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

        let connected = Git3Connected soname peerAPI

        liftIO $ withGit3Env connected again

gitWriteCommitPackIO :: (GitWritePacksOpts opt, Pretty what) => opt -> what -> ( BS.ByteString -> IO () ) -> IO ()
gitWriteCommitPackIO opts what action = do
    hhead <- gitRevParse what >>= orThrow (OtherGitError $ show $ "can't parse" <+> pretty what)

    co <- gitReadObjectThrow Commit hhead
           <&> LBS8.lines
           <&> takeWhile ( not . LBS8.null )
           <&> LBS8.unpack . LBS8.unlines
           <&> parseTop
           >>= orThrow (OtherGitError "invalid commit format")

    let parents =  [ fromStringMay @GitHash hash
                   | ListVal [ StringLike "parent", StringLike hash ] <- co
                   ] & catMaybes

    -- debug $ "EXCLUDE PARENTS" <+> pretty (excludeParents opts)

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
      ph <- ContT withGitCat
      let ssin  = getStdin ph
      let ssout = getStdout ph

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

            liftIO $ hPrint ssin $ pretty ha
            liftIO $ hFlush ssin

            s <- liftIO $ hGetLine ssout

            case words s of
              [_,_,s] -> do
                n <- readMay @Int s & orThrow (OtherGitError "git cat-file --batch error")
                co <- liftIO $ LBS.hGet ssout n
                void $ liftIO $ hGetLine ssout
                let header = [qc|{pretty (Short t)} {s} {pretty ha}|]
                go =<< next (LBS.toStrict (LBS8.intercalate "\n" [header, co]))

              e -> throwIO $ OtherGitError ("git cat-file --batch error: " <> show e)

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

theDict :: forall m . ( HBS2GitPerks m
                      , HasClientAPI PeerAPI UNIX m
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

        entry $ bindMatch "test:git:tree:export" $ nil_ $ \syn -> flip runContT pure do
          pure ()

        entry $ bindMatch "test:git:tree:pack:write" $ nil_ $ \syn -> flip runContT pure do

          let o = [ WriteFullPack | StringLike "--full" <- syn ] & HS.fromList

          (what,to) <- case syn of

             ( StringLike rev : StringLike fn : _) -> do
              -- let co = headDef "HEAD" $ [ GitRef (LBS8.toStrict $ LBS8.pack what) | StringLike what  <- syn ]
               fh <- ContT $ bracket (liftIO (IO.openFile fn WriteMode)) hClose
               pure (rev, fh)

             ( StringLike rev : _ ) -> pure  ( rev, stdout )

             _ -> pure ( "HEAD", stdout )

          liftIO $ gitWriteCommitPackIO o what $ \bs -> do
            BS.hPut to bs

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
    let dict = theDict
    recover $ run dict cli

