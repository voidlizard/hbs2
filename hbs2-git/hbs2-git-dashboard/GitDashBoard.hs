{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Git.DashBoard.Prelude

import HBS2.Net.Messaging.Unix
import HBS2.System.Dir
import HBS2.OrDie
import HBS2.Polling

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.Git.Web.Assets
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.State.Index
import HBS2.Git.DashBoard.State.Commits
import HBS2.Git.DashBoard.Types
import HBS2.Git.Web.Html.Root

import HBS2.Peer.CLI.Detect

import Lucid (renderTextT,HtmlT(..),toHtml)
import Options.Applicative as O
import Data.Either
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.ByteString.Lazy qualified as LBS
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


configParser :: DashBoardPerks m => Parser (m ())
configParser = do
  opts <- RunDashBoardOpts <$> optional (strOption
      ( long "config"
     <> short 'c'
     <> metavar "FILEPATH"
     <> help "Path to the configuration file"
     <> completer (bashCompleter "file")
      ))

  cmd <- subparser
              (  command "web" (O.info pRunWeb (progDesc "Run the web interface"))
              <> command "index" (O.info pRunIndex (progDesc "update index"))
              )

  pure $ cmd opts


pRunWeb :: DashBoardPerks m => Parser (RunDashBoardOpts -> m ())
pRunWeb = pure $ \x -> runDashBoardM x runScotty

pRunIndex :: DashBoardPerks m => Parser (RunDashBoardOpts -> m ())
pRunIndex = pure $ \x -> runDashBoardM x do
  updateIndex

{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Functor law" -}

getRPC :: Monad m => HasConf m => m (Maybe FilePath)
getRPC = pure Nothing


runDashBoardM :: DashBoardPerks m => RunDashBoardOpts -> DashBoardM m a -> m a
runDashBoardM cli m = do


  let hbs2_git_dashboard = "hbs2-git-dashboard"
  xdgConf <- liftIO $ getXdgDirectory XdgConfig hbs2_git_dashboard
  xdgData <- liftIO $ getXdgDirectory XdgData hbs2_git_dashboard

  let cliConfPath = cli & configPath

  let confPath = fromMaybe xdgConf cliConfPath
  let confFile = confPath </> "config"

  let dbFile = xdgData </> "state.db"

  when (isNothing cliConfPath) do
    touch confFile

  conf <- runExceptT (liftIO $ readFile confFile)
           <&> fromRight mempty
           <&> parseTop
           <&> fromRight mempty

  liftIO $ print (pretty conf)

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
                conf
                dbFile
                peerAPI
                refLogAPI
                refChanAPI
                lwwAPI
                sto

    void $ ContT $ withAsync do
      q <- withDashBoardEnv env $ asks _pipeline
      forever do
        liftIO (atomically $ readTQueue q) & liftIO . join

    lift $ withDashBoardEnv env (withState evolveDB >> m)
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

runDashboardWeb :: WebOptions -> ScottyT (DashBoardM IO) ()
runDashboardWeb wo = do
  middleware logStdout

  let assets = _assetsOverride wo

  case assets of
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

      item <- lift (selectRepoList ( mempty
                                      & set repoListByLww (Just lww)
                                      & set repoListLimit (Just 1))
                                   )
                 <&> listToMaybe
                 >>= orFall (status status404)

      lift $ html =<< renderTextT (thisRepoManifest item)


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

  get (routePattern (RepoCommitDefault  "lww" "hash")) (commitRoute RepoCommitSummary)
  get (routePattern (RepoCommitSummaryQ "lww" "hash")) (commitRoute RepoCommitSummary)
  get (routePattern (RepoCommitPatchQ   "lww" "hash")) (commitRoute RepoCommitPatch)

  get (routePattern (RepoForksHtmx "lww")) do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey 'HBS2Basic)
    flip runContT pure do
      lww       <- lwws' & orFall (status status404)
      lift $ renderHtml (repoForks lww)
      -- lift $ renderHtml (toHtml $ show $ pretty lww)

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
    pno <- cfgValue @HttpPortOpt @(Maybe Int) <&> fromMaybe 8090
    wo  <- cfgValue @DevelopAssetsOpt @(Maybe FilePath) <&> WebOptions

    env <- ask

    flip runContT pure do

      void $ ContT $ withAsync updateIndexPeriodially

      scottyT pno (withDashBoardEnv env) (runDashboardWeb wo)

updateIndexPeriodially :: DashBoardPerks m => DashBoardM m ()
updateIndexPeriodially = do

  cached <- newTVarIO ( mempty :: HashMap MyRefLogKey HashRef )

  changes <- newTQueueIO

  api <- asks _refLogAPI

  env <- ask

  let rlogs = selectRefLogs <&> fmap (over _1 (coerce @_ @MyRefLogKey)) . fmap (, 30)

  flip runContT pure do

    void $ ContT $ withAsync $ forever do
      rs <- atomically $ peekTQueue changes >> flushTQueue changes
      addJob (withDashBoardEnv env updateIndex)
      pause @'Seconds 30

    lift do
      polling (Polling 1 10) rlogs $ \r -> do

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


main :: IO ()
main = do
  execParser opts & join
  where
    opts = O.info (configParser <**> helper)
      ( fullDesc
     <> progDesc "hbs2-git-dashboard"
     <> O.header "hbs2-git-dashboard" )


