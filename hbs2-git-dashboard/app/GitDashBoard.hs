{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Git.DashBoard.Prelude

import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto
import HBS2.Net.Proto.Service

import HBS2.System.Dir
import HBS2.OrDie
import HBS2.Polling

import HBS2.Actors.Peer
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.Git.Web.Assets
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.State.Index
import HBS2.Git.DashBoard.State.Commits
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.Fixme
import HBS2.Git.DashBoard.Manifest
import HBS2.Git.Web.Html.Root
import HBS2.Git.Web.Html.Issue
import HBS2.Git.Web.Html.Repo
import HBS2.Git.Web.Html.Fixme

import HBS2.Peer.CLI.Detect

import Data.Config.Suckless.Script

import Lucid (renderTextT,HtmlT(..),toHtml)
import Data.Either
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static hiding ((<|>))
import Network.Wai.Middleware.StaticEmbedded as E
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans as Scotty
import Control.Monad.Except
import System.Random
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Control.Concurrent.STM (flushTQueue)
import System.FilePath
import System.Process.Typed
import System.Directory (XdgDirectory(..),getXdgDirectory)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import System.Environment
import System.Exit
import System.IO.Temp

{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Functor law" -}

getRPC :: Monad m => HasConf m => m (Maybe FilePath)
getRPC = pure Nothing

data CallRPC
data PingRPC
data IndexNowRPC

type MyRPC = '[ PingRPC, IndexNowRPC, CallRPC ]

instance HasProtocol UNIX  (ServiceProto MyRPC UNIX) where
  type instance ProtocolId (ServiceProto MyRPC UNIX) = 0xFAFABEBE
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

type instance Input CallRPC = String
type instance Output CallRPC = String

type instance Input PingRPC = ()
type instance Output PingRPC = String

type instance Input IndexNowRPC = ()
type instance Output IndexNowRPC = ()

class HasDashBoardEnv m where
  getDashBoardEnv :: m DashBoardEnv

instance (MonadIO m) => HandleMethod m CallRPC where
  handleMethod n = do
    debug $ "RPC CALL" <+> pretty n
    pure ""

instance (MonadIO m, HasDashBoardEnv m) => HandleMethod m PingRPC where
  handleMethod _ = do
    debug $ "RPC PING"
    pure "pong"

instance (DashBoardPerks m, HasDashBoardEnv m) => HandleMethod m IndexNowRPC where
  handleMethod _ = do
    e <- getDashBoardEnv
    debug $ "rpc: index:now"
    withDashBoardEnv e $ addJob (liftIO $ withDashBoardEnv e updateIndex)

instance HasLimit (FromParams 'FixmeDomain [Param]) where
  -- TODO: optimal-page-size
  limit (FromParams p) = Just limits
    where
      pageSize = fromIntegral fixmePageSize
      page     = fromMaybe 0 $ headMay [ readDef 0 (Text.unpack n) | ("$page", n) <- p ]
      offset   = page
      limits   = (fromIntegral offset, fromIntegral pageSize)

instance HasPredicate (FromParams 'FixmeDomain [Param]) where
  predicate (FromParams args) = do
    flip fix seed $ \next -> \case
      [] -> All
      ( clause : rest ) -> And clause (next rest)

    where
      seed = [ AttrLike a b | (a,b) <- args, a /= "$page" ]

readConfig :: DashBoardPerks m => m [Syntax C]
readConfig = do

  xdgConf <- liftIO $ getXdgDirectory XdgConfig hbs2_git_dashboard

  let confPath = xdgConf
  let confFile = confPath </> "config"

  touch confFile

  runExceptT (liftIO $ readFile confFile)
     <&> fromRight mempty
     <&> parseTop
     <&> fromRight mempty

runDashBoardM :: DashBoardPerks m => DashBoardM m a -> m a
runDashBoardM m = do

  xdgData <- liftIO $ getXdgDirectory XdgData hbs2_git_dashboard

  let dataDir = xdgData
  let dbFile = xdgData </> "state.db"

  -- FIXME: unix-socket-from-config
  soname <- detectRPC `orDie` "hbs2-peer rpc not found"

  let errorPrefix  = toStderr . logPrefix "[error] "
  let warnPrefix   = toStderr . logPrefix "[warn] "
  let noticePrefix = toStderr . logPrefix ""
  let debugPrefix  = toStderr . logPrefix "[debug] "

  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @DEBUG  debugPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix

  flip runContT pure do


    client <- liftIO $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
              >>= orThrowUser ("can't connect to" <+> pretty soname)

    void $ ContT $ withAsync $ runMessagingUnix client

    peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
    refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
    refChanAPI <- makeServiceCaller @RefChanAPI (fromString soname)
    storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
    lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

    let sto = AnyStorage (StorageClient storageAPI)

    let endpoints = [ Endpoint @UNIX  peerAPI
                    , Endpoint @UNIX  refLogAPI
                    , Endpoint @UNIX  refChanAPI
                    , Endpoint @UNIX  lwwAPI
                    , Endpoint @UNIX  storageAPI
                    ]

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

    env <- newDashBoardEnv
                dataDir
                peerAPI
                refLogAPI
                refChanAPI
                lwwAPI
                sto

    void $ ContT $ withAsync do
      q <- withDashBoardEnv env $ asks _pipeline
      forever do
        liftIO (atomically $ readTQueue q) & liftIO . join

    lift $ withDashBoardEnv env m
      `finally` do
        setLoggingOff @DEBUG
        setLoggingOff @INFO
        setLoggingOff @ERROR
        setLoggingOff @WARN
        setLoggingOff @NOTICE


data WebOptions =
  WebOptions
  { _assetsOverride :: Maybe FilePath
  }

orFall :: m r -> Maybe a -> ContT r m a
orFall a mb = ContT $ maybe1 mb a

renderHtml :: forall m a . MonadIO m => HtmlT (ActionT m) a -> ActionT m ()
renderHtml m = renderTextT m >>= html

runDashboardWeb :: WebOptions ->  ScottyT (DashBoardM IO) ()
runDashboardWeb WebOptions{..} = do
  middleware logStdout

  case _assetsOverride of
    Nothing -> do
      middleware (E.static assetsDir)
    Just f -> do
      middleware $ staticPolicy (noDots >-> addBase f)

  get (routePattern RepoListPage) do
    renderHtml dashboardRootPage


  get "/:lww" do
    lww <- captureParam @String "lww"  <&> fromStringMay @(LWWRefKey 'HBS2Basic)
                >>= orThrow (itemNotFound "repository key")

    redirect (LT.fromStrict  $ toURL (RepoPage (CommitsTab Nothing) lww))

  get (routePattern (RepoPage "tab" "lww")) do
    lww <- captureParam @String "lww"  <&> fromStringMay
                >>= orThrow (itemNotFound "repository key")

    tab <- captureParam @String "tab"
              <&> fromStringMay
              <&> fromMaybe (CommitsTab Nothing)

    qp <- queryParams

    renderHtml (repoPage tab lww qp)

  get (routePattern (RepoManifest "lww")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    flip runContT pure do
      lww <- lwws' & orFall (status status404)
      TopInfoBlock{..} <- lift $ getTopInfoBlock lww
      lift $ html (LT.fromStrict manifest)

  get (routePattern (RepoRefs "lww")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)

    -- setHeader "HX-Push-Url" [qc|/{show $ pretty lwws'}|]

    flip runContT pure do
      lww <- lwws' & orFall (status status404)
      lift $ renderHtml (repoRefs lww)

  get (routePattern (RepoTree "lww" "co" "hash")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    hash' <- captureParam @String "hash" <&> fromStringMay @GitHash
    co'   <- captureParam @String "co" <&> fromStringMay @GitHash

    flip runContT pure do
      lww  <- lwws' & orFall (status status404)
      hash <- hash' & orFall (status status404)
      co   <- co'   & orFall (status status404)
      lift $ renderHtml (repoTree lww co hash)

  get (routePattern (RepoBlob "lww" "co" "hash" "blob")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    hash' <- captureParam @String "hash" <&> fromStringMay @GitHash
    co'   <- captureParam @String "co" <&> fromStringMay @GitHash
    blob' <- captureParam @String "blob" <&> fromStringMay @GitHash

    flip runContT pure do
      lww       <- lwws' & orFall (status status404)
      hash      <- hash' & orFall (status status404)
      co        <- co'   & orFall (status status404)
      blobHash  <- blob' & orFall (status status404)

      blobInfo <- lift (selectBlobInfo (BlobHash blobHash))
                    >>= orFall (status status404)

      lift $ renderHtml (repoBlob lww (TreeCommit co) (TreeTree hash) blobInfo)

  get (routePattern (RepoSomeBlob "lww" "syntax" "blob")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    syn   <- captureParamMaybe @Text "syntax" <&> fromMaybe "default"
    blob' <- captureParam @String "blob" <&> fromStringMay @GitHash

    flip runContT pure do
      lww  <- lwws' & orFall (status status404)
      blob <- blob' & orFall (status status404)
      lift $ renderHtml (repoSomeBlob lww syn blob)

  get (routePattern (RepoCommitDefault  "lww" "hash")) (commitRoute RepoCommitSummary)
  get (routePattern (RepoCommitSummaryQ "lww" "hash")) (commitRoute RepoCommitSummary)
  get (routePattern (RepoCommitPatchQ   "lww" "hash")) (commitRoute RepoCommitPatch)

  get (routePattern (RepoForksHtmx "lww")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    flip runContT pure do
      lww       <- lwws' & orFall (status status404)
      lift $ renderHtml (repoForks lww)
      -- lift $ renderHtml (toHtml $ show $ pretty lww)

  get (routePattern (IssuePage "lww" "fixme")) do

    r <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    f <- captureParam @String "fixme" <&> fromStringMay @FixmeKey

    debug $ blue "AAAA" <+> pretty r <+> pretty f

    flip runContT pure do
      lww       <- r & orFall (status status404)
      fme       <- f & orFall (status status404)

      lift $ renderHtml (issuePage (RepoLww lww) fme)

  get (routePattern (RepoFixmeHtmx mempty "lww")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    p <- queryParams
    debug $ "FIXME: GET QUERY" <+> pretty p
    flip runContT pure do
      lww   <- lwws' & orFall (status status404)
      lift $ renderHtml (repoFixme (FromParams @'FixmeDomain p) lww)

  get (routePattern (RepoCommits "lww")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)

    let pred = mempty & set commitPredOffset 0
                      & set commitPredLimit 100

    flip runContT pure do
      lww       <- lwws' & orFall (status status404)
      lift $ renderHtml (repoCommits lww (Right pred))

  get (routePattern (RepoCommitsQ "lww" "off" "lim")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    off   <- captureParam  @Int "off"
    lim   <- captureParam  @Int "lim"

    let pred = mempty & set commitPredOffset off
                      & set commitPredLimit lim

    flip runContT pure do

      lww       <- lwws' & orFall (status status404)

      -- FIXME: this
      referrer <- lift (Scotty.header "Referer")
                    >>= orFall (redirect $ LT.fromStrict $ toURL (RepoPage (CommitsTab Nothing) lww))

      lift $ renderHtml (repoCommits lww (Left pred))

  -- "pages"

  where
    commitRoute style = do
      lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey HBS2Basic)
      co    <- captureParam @String "hash" <&> fromStringMay @GitHash

      referrer <- Scotty.header "Referer"
      debug $ yellow "COMMIT-REFERRER" <+> pretty referrer

      flip runContT pure do
        lww   <- lwws' & orFall (status status404)
        hash  <- co & orFall (status status404)
        lift $ renderHtml (repoCommit style lww hash)


runScotty :: DashBoardPerks m => DashBoardM m ()
runScotty  = do
    pno <- getHttpPortNumber
    wo <- WebOptions <$> getDevAssets

    env <- ask

    notice "evolving db"
    withState evolveDB

    notice "running config"
    conf <- readConfig

    run theDict conf

    flip runContT pure do
      void $ ContT $ withAsync updateIndexPeriodially
      void $ ContT $ withAsync runRPC
      scottyT pno (withDashBoardEnv env) (runDashboardWeb wo)


data RPCEnv = RPCEnv
  { rpcMessaging :: MessagingUnix
  , dashBoardEnv :: DashBoardEnv
  }

newtype RunRPCM m a = RunRPCM { fromRunRPC :: ReaderT RPCEnv m a }
                      deriving newtype ( Applicative
                                       , Functor
                                       , Monad
                                       , MonadIO
                                       , MonadUnliftIO
                                       , MonadTrans
                                       , MonadReader RPCEnv
                                       )
runRPCMonad :: DashBoardEnv -> MessagingUnix -> RunRPCM m a -> m a
runRPCMonad env s m = runReaderT (fromRunRPC m) (RPCEnv s env)

instance HasFabriq UNIX (RunRPCM IO) where
  getFabriq = asks (Fabriq . rpcMessaging)

instance HasOwnPeer UNIX (RunRPCM IO) where
  ownPeer = asks ( msgUnixSelf . rpcMessaging)

instance HasDashBoardEnv (ResponseM UNIX (RunRPCM IO)) where
  getDashBoardEnv = lift $ asks dashBoardEnv

runRPC :: DashBoardPerks m => DashBoardM m ()
runRPC = do
  debug $ green "runRPC loop"

  env <- ask

  liftIO $ flip runContT pure do

    soname <- ContT $ bracket (liftIO $ emptySystemTempFile "hbs2-git-dashboard-socket") rm

    liftIO $ withDashBoardEnv env do
      setRPCSocket soname

    void $ ContT $ bracket (pure soname) (\_ -> withDashBoardEnv env $ delRPCSocket)

    notice $ green "rpc-socket" <+> pretty soname

    server <- newMessagingUnix True 1.0 soname

    m1 <- ContT $ withAsync (runMessagingUnix server)

    p1 <- ContT $ withAsync $ runRPCMonad env server do
                    runProto @UNIX
                      [ makeResponse (makeServer @MyRPC)
                      ]

    void $ waitAnyCatchCancel [m1,p1]


updateIndexPeriodially :: DashBoardPerks m => DashBoardM m ()
updateIndexPeriodially = do


  api <- asks _refLogAPI

  env <- ask

  changes <- newTQueueIO

  flip runContT pure do

    p1 <- ContT $ withAsync $ forever do
      rs <- atomically $ peekTQueue changes >> flushTQueue changes
      addJob (withDashBoardEnv env updateIndex)
      pause @'Seconds 1

    p2 <- pollRepos changes

    p3 <- pollFixmies

    void $ waitAnyCatchCancel [p1,p2,p3]

  where

    pollFixmies = do

      env <- ask

      api <- asks _refChanAPI

      cached <- newTVarIO ( mempty :: HashMap MyRefChan HashRef )

      let chans = selectRepoFixme
                   <&> fmap (,60)

      ContT $ withAsync $ do
        polling (Polling 1 30) chans $ \(l,r) -> do
          debug $ yellow "POLL FIXME CHAN" <+> pretty (AsBase58 r)

          void $ runMaybeT do

            new <- lift (callRpcWaitMay @RpcRefChanGet (TimeoutSec 1) api (coerce r))
                    <&> join
                    >>= toMPlus

            old <- readTVarIO cached <&> HM.lookup r

            when (Just new /= old) $ lift do
              debug $ yellow "fixme refchan changed" <+> "run update" <+> pretty new
              addJob do
                -- TODO: this-is-not-100-percent-reliable
                --   $workflow: backlog
                --   откуда нам вообще знать, что там всё получилось?
                atomically $ modifyTVar cached (HM.insert r new)
                void $ try @_ @SomeException (withDashBoardEnv env $ updateFixmeFor l r)


    pollRepos changes = do

      cached <- newTVarIO ( mempty :: HashMap MyRefLogKey HashRef )

      api <- asks _refLogAPI
      let rlogs = selectRefLogs <&> fmap (over _1 (coerce @_ @MyRefLogKey)) . fmap (, 60)

      ContT $ withAsync $ do
        polling (Polling 1 30) rlogs $ \r -> do

          debug $ yellow "POLL REFLOG" <+> pretty r

          rv <- callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) api (coerce r)
                  <&> join

          old <- readTVarIO cached <&> HM.lookup r

          for_ rv $ \x -> do

            when (rv /= old) do
              debug $ yellow "REFLOG UPDATED" <+> pretty r <+> pretty x
              atomically $ modifyTVar cached (HM.insert r x)
              atomically $ writeTQueue changes r

              flip runContT pure $ callCC $ \exit -> do

                lww <- lift (selectLwwByRefLog (RepoRefLog r))
                         >>= maybe (exit ()) pure

                dir <- lift $ repoDataPath (coerce lww)

                here <- doesDirectoryExist dir

                unless here do
                  debug $ red "INIT DATA DIR" <+> pretty dir
                  mkdir dir
                  void $ runProcess $ shell [qc|git --git-dir {dir} init --bare|]

                let cmd = [qc|git --git-dir {dir} hbs2 import {show $ pretty lww}|]
                debug $ red "SYNC" <+> pretty cmd
                void $ runProcess $ shell cmd

                lift $ buildCommitTreeIndex (coerce lww)


quit :: DashBoardPerks m => m ()
quit = liftIO exitSuccess

withMyRPCClient :: ( MonadUnliftIO m )
                   -- , HasTimeLimits UNIX (ServiceProto MyRPC UNIX) m)
                => FilePath -> (ServiceCaller MyRPC UNIX -> IO b) -> m b
withMyRPCClient soname m = do
  liftIO do
    client <- newMessagingUnix False 1.0 soname
    flip runContT pure do
      mess <- ContT $ withAsync $ runMessagingUnix client
      caller <- makeServiceCaller @MyRPC @UNIX (msgUnixSelf client)
      p2 <- ContT $ withAsync $ runReaderT (runServiceClient caller) client
      void $ ContT $ bracket none (const $ cancel mess)
      void $ ContT $ bracket none (const $ cancel p2)
      liftIO $ m caller


theDict :: forall m . ( DashBoardPerks m
                      -- , HasTimeLimits UNIX (ServiceProto MyRPC UNIX) m
                      ) => Dict C (DashBoardM m)
theDict = do
  makeDict @C do
    -- TODO: write-man-entries
    myHelpEntry
    fixmeAllowEntry
    fixmeAllowDropEntry
    webEntry
    portEntry
    developAssetsEntry
    getRpcSocketEntry
    rpcPingEntry
    rpcIndexEntry
    debugEntries

  where

    myHelpEntry = do
        entry $ bindMatch "--help" $ nil_ $ \case
          HelpEntryBound what -> do
            helpEntry what
            quit

          [StringLike s] -> helpList False (Just s) >> quit

          _ -> helpList False Nothing >> quit

    fixmeAllowEntry =  do
      brief "allows fixme for given reflog" $
        args [arg "public-key" "reflog"] $
        examples [qc|
          fixme-allow BTThPdHKF8XnEq4m6wzbKHKA6geLFK4ydYhBXAqBdHSP
        |]
        $ entry $ bindMatch "fixme-allow" $ nil_ \case
          [SignPubKeyLike what] -> do
            lift $ insertFixmeAllowed (RepoRefLog (RefLogKey what))

          _ -> throwIO $ BadFormException @C nil

    fixmeAllowDropEntry =  do
      brief "drop all allowed fixme records" $
        examples [qc|
          fixme-allow:drop
        |]
        $ entry $ bindMatch "fixme-allow:drop" $ nil_ \case
          [] -> do
            lift $ deleteFixmeAllowed

          _ -> throwIO $ BadFormException @C nil

    webEntry = do
      brief "run web interface" $
        entry $ bindMatch "web" $ nil_ $ const do
          lift runScotty

    portEntry = do
      brief "set http port for web interface" $
        entry $ bindMatch "port" $ nil_ \case
          [LitIntVal n] -> do
            tp <- lift $ asks _dashBoardHttpPort
            atomically $ writeTVar tp (Just (fromIntegral n))

          _ -> throwIO $ BadFormException @C nil


    developAssetsEntry = do
      entry $ bindMatch "develop-assets" $ nil_ \case
        [StringLike s] -> do
          pure ()

        _ -> none

    getRpcSocketEntry = do
      entry $ bindMatch "rpc:socket" $ nil_ $ const do
        lift getRPCSocket >>= liftIO . maybe exitFailure putStr

    rpcPingEntry = do
      entry $ bindMatch "ping" $ nil_ $ const $ lift do
        so <- getRPCSocket >>= orThrowUser "rpc socket down"
        withMyRPCClient so $ \caller -> do
          what <- callService @PingRPC caller ()
          print what

    rpcIndexEntry = do
      entry $ bindMatch "index:now" $ nil_ $ const $ lift do
        so <- getRPCSocket >>= orThrowUser "rpc socket down"
        withMyRPCClient so $ \caller -> do
          void $ callService @IndexNowRPC caller ()

    -- TODO: ASAP-hide-debug-functions-from-help

    debugEntries = do

      entry $ bindMatch "debug:cache:ignore:on" $ nil_ $ const $ lift do
        t <- asks _dashBoardIndexIgnoreCaches
        atomically $ writeTVar t True

      entry $ bindMatch "debug:cache:ignore:off" $ nil_ $ const $ lift do
        t <- asks _dashBoardIndexIgnoreCaches
        atomically $ writeTVar t False

      entry $ bindMatch "debug:select-repo-fixme" $ nil_ $ const $ lift do
        rs <- selectRepoFixme
        for_ rs $ \(r,f) -> do
          liftIO $ print $ pretty r <+> pretty (AsBase58 f)

      entry $ bindMatch "debug:check-fixme-allowed" $ nil_ $ \case
        [SignPubKeyLike s] -> do
           what <- lift $ checkFixmeAllowed (RepoLww (LWWRefKey s))
           liftIO $ print $ pretty what

        _ -> throwIO $ BadFormException @C nil


      entry $ bindMatch "debug:test-with-fixme" $ nil_ $ \case
        [SignPubKeyLike s] -> lift do
           r <- listFixme (RepoLww (LWWRefKey s)) ()
           for_ r $ \f -> do
              liftIO $ print $ pretty f

        _ -> throwIO $ BadFormException @C nil

      entry $ bindMatch "debug:count-fixme" $ nil_ $ \case
        [SignPubKeyLike s] -> lift do
           r <- countFixme (RepoLww (LWWRefKey s))
           liftIO $ print $ pretty r

        _ -> throwIO $ BadFormException @C nil



main :: IO ()
main = do
  argz <- getArgs
  cli <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  let dict = theDict

  void $ runDashBoardM $ do
    run dict cli


