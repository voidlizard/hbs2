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

import Data.Config.Suckless.Script

import Lucid (renderTextT,HtmlT(..),toHtml)
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
import System.Environment
import System.Exit

{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Functor law" -}

getRPC :: Monad m => HasConf m => m (Maybe FilePath)
getRPC = pure Nothing


hbs2_git_dashboard :: FilePath
hbs2_git_dashboard = "hbs2-git-dashboard"

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
      pause @'Seconds 60

    lift do
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


main :: IO ()
main = do
  argz <- getArgs
  cli <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  conf <- readConfig

  let dict = makeDict @C do

          -- TODO: write-man-entries

          myHelpEntry
          fixmeAllowEntry
          fixmeAllowDropEntry
          webEntry
          portEntry
          developAssetsEntry

  void $ runDashBoardM $ run dict (conf <> cli)

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

