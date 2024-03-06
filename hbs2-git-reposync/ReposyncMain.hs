{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Actors.Peer
import HBS2.Net.Proto.Notify
import HBS2.Peer.Proto
import HBS2.Peer.RPC.Client.Unix hiding (Cookie)
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.Notify

import HBS2.System.Logger.Simple hiding (info)

import Data.Config.Suckless

import Data.Char qualified as Char
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Except (runExceptT,throwError)
import Control.Monad.Cont
import Control.Monad.Reader
import Data.ByteString.Builder hiding (writeFile)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Applicative
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import Control.Concurrent.STM (flushTQueue)
import UnliftIO
import Web.Scotty hiding (header,next)

import Network.HTTP.Types
import Network.Wai

import System.Exit qualified as Exit
import System.IO.Unsafe (unsafePerformIO)

import Streaming.Prelude qualified as S

-- TODO: support-encrypted-repoes

die :: (MonadIO m, Show msg) => msg -> m a
die msg = liftIO $ Exit.die [qc|{msg}|]

data RepoInitException = RepoInitException FilePath deriving (Show, Typeable)
instance Exception RepoInitException

debugPrefix :: SetLoggerEntry
debugPrefix  = toStdout . logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = toStdout . logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix  = toStdout . logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix  = toStdout . logPrefix "[notice] "

data ReposyncRootKey
data ReposyncHttpPort

instance Monad m => HasCfgKey ReposyncRootKey (Maybe String) m where
  key = "root"

instance Monad m => HasCfgKey ReposyncHttpPort (Maybe Int) m where
  key = "http-port"

data RepoEntry =
  RepoEntry
  { repoPath :: FilePath
  , repoRef  :: RefLogKey HBS2Basic
  , repoKeys :: [FilePath]
  , repoHash :: TVar (Maybe HashRef)
  }
  deriving stock (Eq)


data ReposyncState =

  ReposyncState
  { _rpcSoname         :: FilePath
  , _rpcRefLog         :: ServiceCaller RefLogAPI UNIX
  , _rpcNotifySink     :: NotifySink (RefLogEvents L4Proto) UNIX
  , _reposyncBaseDir   :: FilePath
  , _reposyncPort      :: Int
  , _reposyncEntries   :: TVar [RepoEntry]
  }

makeLenses 'ReposyncState

newtype ReposyncM m a =
  App { unReposyncM :: ReaderT ReposyncState m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadThrow
                   , MonadReader ReposyncState
                   , MonadUnliftIO
                   , MonadTrans
                   )


myName :: FilePath
myName = "hbs2-git-reposync"

reposyncDefaultDir :: FilePath
reposyncDefaultDir = unsafePerformIO do
  getXdgDirectory XdgData (myName </> "repo")
{-# NOINLINE reposyncDefaultDir #-}

newState :: MonadUnliftIO m
         => FilePath
         -> ServiceCaller RefLogAPI UNIX
         -> NotifySink (RefLogEvents L4Proto) UNIX
         -> m ReposyncState

newState so refLog sink =
  ReposyncState so refLog sink reposyncDefaultDir 4017 <$> newTVarIO mempty

{- HLINT ignore "Functor law" -}
withConfig :: forall a m . (MonadUnliftIO m) => Maybe FilePath -> ReposyncM m a -> ReposyncM m ()
withConfig cfg m = do

  let defDir  = reposyncDefaultDir

  defConfDir <- liftIO $ getXdgDirectory XdgConfig myName

  realCfg <- case cfg of
             Just f -> pure f
             Nothing -> do
                liftIO do
                  let conf = defConfDir </> "config"
                  void $ try @_ @IOException $ createDirectoryIfMissing True defConfDir
                  debug $ "config-dir" <+> pretty defConfDir
                  void $ try @_ @IOException $ appendFile conf ""
                  pure conf

  syn <- liftIO (readFile realCfg) <&> parseTop
                                   <&> fromRight mempty

  debug $ "config" <+> pretty realCfg <> line <> pretty syn

  ev <- asks (view reposyncEntries)

  let root = runReader (cfgValue @ReposyncRootKey) syn
              & fromMaybe defDir

  let port = runReader (cfgValue @ReposyncHttpPort) syn
              & fromMaybe 4017

  es <- entries root syn
  atomically $ modifyTVar ev (\x -> List.nub ( x <> es))

  local ( set reposyncBaseDir root .
          set reposyncPort port
        ) (void m)

  where
    entries root syn = do

      let findKeys w = [ Text.unpack p
                       | ListVal (Key "decrypt" [LitStrVal p]) <- w
                       ]

      let reflogs = catMaybes [ (,) <$> fromStringMay @(RefLogKey HBS2Basic) (Text.unpack o)
                                    <*> pure (findKeys args)
                              | ListVal (Key "reflog" (LitStrVal o : args)) <- syn
                              ]

      forM reflogs $ \(repo, keys) -> do
        let path = show $ pretty repo
        mt <- newTVarIO Nothing
        pure $ RepoEntry (root </> path) repo keys mt



data S = S0 (Builder, LBS.ByteString)
       | S1 (LBS.ByteString, Builder, LBS.ByteString)
       | S2 LBS.ByteString

data R = Hdr Header
       | HdrS (Maybe Status)
       | Content LBS.ByteString
       deriving (Data,Generic)

parseResp :: MonadIO m => LBS.ByteString -> m (Maybe Status, [(HeaderName, BS8.ByteString)], LBS.ByteString)
parseResp lbs = do

  let yieldHeader (h, v) = do
        if fmap Char.toLower (LBS.unpack h) == "status" then do
          case LBS.words v of
            (code : rest) -> do
              let cnum = readMay @Int (LBS.unpack code)
              st <- forM cnum $ \n -> pure $ mkStatus n (LBS.toStrict (LBS.unwords rest))
              S.yield $ HdrS st

            _             -> S.yield (HdrS Nothing)
        else do
          S.yield $ Hdr (fromString $ LBS.unpack h, LBS.toStrict v)

  chunks <- S.toList_ do
    void $ flip fix (S0 (mempty,lbs)) $ \next -> \case
      S0 (h,s) -> case LBS.uncons s of
        Nothing          -> pure ()

        Just (':', rest) -> next (S1 (toLazyByteString h, mempty, LBS.dropWhile (`elem` "\t ") rest))
        Just (c,   rest) -> next (S0 (h <> char8 c, rest))

      S1 (h, v, s) -> case LBS.uncons s of
        Nothing -> do
          yieldHeader (h,toLazyByteString v)
          pure ()

        Just ('\r',rest) -> do
          yieldHeader (h,toLazyByteString v)
          next (S2 rest)

        Just (c,rest) -> next (S1 (h, v <> char8 c, rest))

      S2 rest -> do
        let (fin, content) = LBS.splitAt 3 rest
        if fin == "\n\r\n" then do
          S.yield (Content content)
        else do
          next (S0 (mempty, LBS.drop 1 rest))


  let hdr = [ s | Hdr s <- chunks ]
  let st = headDef Nothing [ s | HdrS s <- chunks ]
  let content = mconcat    [ s | Content s <- chunks ]

  pure (st, hdr, content)

runSync :: (MonadUnliftIO m, MonadThrow m) => ReposyncM m ()
runSync = do
  es <- asks (view reposyncEntries) >>= readTVarIO
  so <- asks (view rpcSoname)

  refLogRPC <- asks (view rpcRefLog)
  sink      <- asks (view rpcNotifySink)

  port <- asks (fromIntegral . view reposyncPort)

  http <- async $ liftIO $ scotty port $ do
              -- middleware $ staticPolicy (addBase root)
              middleware $ (\a req r2 -> do

                 let env = [ ("REQUEST_METHOD", BS8.unpack $ requestMethod req),
                             ("PATH_INFO",      BS8.unpack $ rawPathInfo req),
                             ("QUERY_STRING",   BS8.unpack $ rawQueryString req),
                             ("CONTENT_TYPE",   maybe "" BS8.unpack $ lookup "Content-Type" $ requestHeaders req),
                             ("CONTENT_LENGTH", maybe "" BS8.unpack $ lookup "Content-Length" $ requestHeaders req),
                             ("GIT_PROJECT_ROOT", "/home/dmz/.local/share/hbs2-reposync/repo"),
                             ("GIT_HTTP_EXPORT_ALL", "")
                          ]

                 let p = shell "/usr/bin/env git-http-backend" & setEnv env -- & setStderr closed
                 (code, out) <- readProcessStdout p

                 liftIO $ LBS.putStrLn out

                 (s, h, body) <- parseResp out

                 let st = fromMaybe status200 s

                 r2 $ responseLBS st h body
                )
              middleware logStdoutDev

  r <- forM es $ \entry -> async $ void $ flip runContT pure do
          let ref = repoRef entry
          let rk = fromRefLogKey ref
          tv <- newTVarIO Nothing

          upd <- newTQueueIO

          debug $ "STARTED WITH" <+> pretty (repoPath entry)

          let notif =
                liftIO $ async do
                          debug $ "Subscribed" <+> pretty ref
                          runNotifySink sink (RefLogNotifyKey ref) $ \(RefLogUpdateNotifyData _ h) -> do
                            debug $ "Got notification" <+> pretty ref <+> pretty h
                            atomically $ writeTQueue upd ()

          void $ ContT $ bracket notif cancel

          lift $ initRepo entry

          lift $ syncRepo entry


          fix \next -> do

            void $ liftIO $ race (pause @'Seconds 60) (atomically (peekTQueue upd))
            pause @'Seconds 5
            liftIO $ atomically $ flushTQueue upd

            rr' <- liftIO $ race (pause @'Seconds 1) do
                      callService @RpcRefLogGet refLogRPC rk
                          <&> fromRight Nothing

            rr <- either (const $ pause @'Seconds 1 >> warn "rpc call timeout" >>  next) pure rr'

            debug $ "REFLOG VALUE:" <+> pretty rr

            r0 <- readTVarIO tv

            unless ( rr == r0 ) do
              debug $ "Syncronize repo!" <+> pretty (repoPath entry)
              fix \again -> do
                lift (syncRepo entry) >>= \case
                  Left{} -> do
                    debug $ "Failed to update:" <+> pretty (repoPath entry)
                    pause @'Seconds 5
                    again

                  Right{} -> do
                    atomically $ writeTVar tv rr

            next

  void $ waitAnyCatchCancel (http : r)

data SyncError = SyncError

syncRepo :: (MonadUnliftIO m, MonadThrow m) => RepoEntry -> m (Either SyncError ())
syncRepo (RepoEntry{..}) = runExceptT do

  -- let cfg = shell [qc|git fetch origin && git remote update origin|] & setWorkingDir repoPath
  let cfg = shell [qc|git remote update origin && git remote prune origin|] & setWorkingDir repoPath
  code <- runProcess cfg

  case code of
    ExitFailure{} -> do
      err $ "Unable to sync repo" <+> pretty repoPath
      throwError SyncError

    _             -> debug $ "synced" <+> pretty repoPath


  let readLocalBranches = shell [qc|git for-each-ref refs/heads|]
                            & setWorkingDir repoPath

  let readBranches = shell [qc|git ls-remote origin|]
                       & setWorkingDir repoPath

  (_, o, _) <- readProcess readBranches

  let txt = TE.decodeUtf8 (LBS.toStrict o)

  let ls = Text.lines txt & fmap Text.words

  let refs = [ (b,a) | [a,b] <- ls ]

  -- TODO: remove-only-vanished-refs
  unless (null refs) do

    (_, o, _) <- readProcess readLocalBranches
    let out = TE.decodeUtf8 (LBS.toStrict o)
                 & Text.lines
                 & fmap Text.words

    let refs = [ r | [_,_,r] <- out ]
    forM_ refs $ \r -> do
      -- debug $ "REMOVING REF" <+> pretty r
      let cmd = shell [qc|git update-ref -d {r}|] & setWorkingDir repoPath
      void $ runProcess cmd

  forM_ refs $ \(ref, val) -> do
    -- debug $ "SET REFERENCE" <+> pretty ref <+> pretty val
    let updateBranch = shell [qc|git update-ref {ref} {val}|]
                        & setWorkingDir repoPath
                        & setStdout closed
                        & setStderr closed

    void $ readProcess updateBranch

    void $ runProcess (shell "git update-server-info" & setWorkingDir repoPath)

  -- let gc = shell [qc|git gc|] & setWorkingDir repoPath
  -- void $ runProcess gc

regenConfig :: MonadUnliftIO m => RepoEntry -> ReposyncM m ()
regenConfig RepoEntry{..} = do

  let hbs2conf = repoPath </> ".hbs2/config"
  rpc <- asks (view rpcSoname)

  let config = ";; generated by hbs2-reposync" <> line
               <> "rpc" <+> "unix" <+> viaShow rpc <> line
               <> line
               <> vcat (fmap (("decrypt"<+>) . dquotes.pretty) repoKeys)

  liftIO $ writeFile hbs2conf (show config)

initRepo :: (MonadUnliftIO m, MonadThrow m) => RepoEntry -> ReposyncM m ()
initRepo e@(RepoEntry{..}) = do
  debug $ "initRepo" <+> pretty repoPath

  let gitDir = repoPath
  gitHere <- liftIO $ doesDirectoryExist gitDir

  liftIO $ createDirectoryIfMissing True gitDir
  debug $ "create dir" <+> pretty gitDir

  let hbs2 = gitDir </> ".hbs2"
  liftIO $ createDirectoryIfMissing True hbs2

  regenConfig e

  unless gitHere do

    let cfg = shell [qc|git init --bare && git remote add origin hbs2://{pretty repoRef}|]
                 & setWorkingDir repoPath

    code <- runProcess cfg

    case code of
      ExitFailure{} -> do
        err $ "Unable to init git repository:" <+> pretty gitDir
        throwM $ RepoInitException gitDir

      _ -> pure ()


detectRPC :: (MonadUnliftIO m) => m (Maybe FilePath)
detectRPC = do

  (_, o, _) <- readProcess (shell [qc|hbs2-peer poke|])
  let answ = parseTop (LBS.unpack o) & fromRight mempty

  pure (headMay [ Text.unpack r | ListVal (Key "rpc:" [LitStrVal r]) <- answ  ])

withApp :: forall a m . MonadUnliftIO m
        => Maybe FilePath
        -> ReposyncM m a
        -> m ()

withApp cfg m = do

  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix

  -- lrpc =

  forever $ handleAny cleanup $ do

    soname <- detectRPC `orDie` "RPC not found"

    let o = [MUWatchdog 20, MUDontRetry]

    client <- race ( pause @'Seconds 1) (newMessagingUnixOpts o False 1.0 soname)
                `orDie` "hbs2-peer rpc timeout!"

    clientN <- newMessagingUnixOpts o False 1.0 soname

    rpc <- makeServiceCaller (fromString soname)

    messaging <- async $ runMessagingUnix client

    mnotify   <- async $ runMessagingUnix clientN

    sink <- newNotifySink

    wNotify <- liftIO $ async $ flip runReaderT clientN $ do
                debug "notify restarted!"
                runNotifyWorkerClient sink

    nProto <- liftIO $ async $ flip runReaderT clientN $ do
        runProto @UNIX
          [ makeResponse (makeNotifyClient @(RefLogEvents L4Proto) sink)
          ]

    let endpoints = [ Endpoint @UNIX  rpc
                    ]

    c1 <- async $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

    state <- newState soname rpc sink

    r <- async $ void $ runReaderT (unReposyncM $ withConfig cfg m) state

    void $ waitAnyCatchCancel [c1, messaging, mnotify, nProto, wNotify, r]

    notice "exiting"

  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE


  where
    cleanup e = do
      err (viaShow e)
      warn "Something bad happened. Retrying..."
      pause @'Seconds 2.5

main :: IO ()
main = runMe .  customExecParser (prefs showHelpOnError) $
  info (helper <*> ((,) <$> opts <*> parser))
  (  fullDesc
  <> header "hbs2-reposync"
  <> progDesc "syncronizes hbs2-git repositories"
  )
  where
    -- parser ::  Parser (IO ())
    parser = hsubparser (  command "run" (info pRun (progDesc "run syncronization"))
                        )

    runMe x = do
      (o, run) <- x
      withApp o run

    opts = optional $ strOption (short 'c' <> long "config")

    pRun = do
      pure runSync

