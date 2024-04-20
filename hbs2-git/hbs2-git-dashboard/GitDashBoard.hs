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

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2.Git.Web.Assets
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.State.Index
import HBS2.Git.DashBoard.Types
import HBS2.Git.Web.Html.Root

import HBS2.Peer.CLI.Detect

import Lucid (renderTextT)
import Options.Applicative as O
import Data.Either
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static hiding ((<|>))
import Network.Wai.Middleware.StaticEmbedded as E
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Trans
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

runDashboardWeb :: WebOptions -> ScottyT (DashBoardM IO) ()
runDashboardWeb wo = do
  middleware logStdout

  let assets = _assetsOverride wo

  case assets of
    Nothing -> do
      middleware (E.static assetsDir)
    Just f -> do
      middleware $ staticPolicy (noDots >-> addBase f)

  get "/" do
    html =<< lift (renderTextT dashboardRootPage)

  get "/repo/:lww" do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey HBS2Basic)
    flip runContT pure do
      lww <- lwws' & orFall (status status404)

      item <- lift (selectRepoList ( mempty
                                      & set repoListByLww (Just lww)
                                      & set repoListLimit (Just 1))
                                   )
                 <&> listToMaybe
                 >>= orFall (status status404)

      lift $ html =<< renderTextT (repoPage item)

  get "/repo/:lww/manifest" do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey HBS2Basic)
    flip runContT pure do
      lww <- lwws' & orFall (status status404)

      item <- lift (selectRepoList ( mempty
                                      & set repoListByLww (Just lww)
                                      & set repoListLimit (Just 1))
                                   )
                 <&> listToMaybe
                 >>= orFall (status status404)

      lift $ html =<< renderTextT (repoManifest item)


  get "/repo/:lww/refs" do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey HBS2Basic)
    flip runContT pure do
      lww <- lwws' & orFall (status status404)
      refs <- lift $ gitShowRefs lww
      lift $ html =<< renderTextT (repoRefs lww refs)

  get "/repo/:lww/tree/:co/:hash" do
    lwws' <- captureParam @String "lww" <&> fromStringMay @(LWWRefKey HBS2Basic)
    hash' <- captureParam @String "hash" <&> fromStringMay @GitHash
    co'   <- captureParam @String "co" <&> fromStringMay @GitHash

    flip runContT pure do
      lww  <- lwws' & orFall (status status404)
      hash <- hash' & orFall (status status404)
      co   <- co'   & orFall (status status404)
      tree <- lift $ gitShowTree lww hash
      back <- lift $ selectParentTree co hash
      debug $ "selectParentTree" <+> pretty co <+> pretty hash <+> pretty back
      lift $ html =<< renderTextT (repoTree lww co hash tree back)


repoDataPath  :: (DashBoardPerks m, MonadReader DashBoardEnv m) => LWWRefKey 'HBS2Basic -> m FilePath
repoDataPath lw = asks _dataDir <&> (</> (show $ pretty lw)) >>= canonicalizePath


gitShowTree :: (DashBoardPerks m, MonadReader DashBoardEnv m)
            => LWWRefKey 'HBS2Basic
            -> GitHash
            -> m [(GitObjectType, GitHash, Text)]
gitShowTree what hash  = do
  path <- repoDataPath what
  let cmd = [qc|git --git-dir {path} ls-tree {show $ pretty hash}|]

  -- FIXME: extract-method
  gitRunCommand cmd
    >>= orThrowUser ("can't read git repo" <+> pretty path)
    <&> LBS8.lines
    <&> fmap LBS8.words
    <&> mapMaybe \case
         [_,tp,h,name] -> do
            (,,) <$> fromStringMay (LBS8.unpack tp)
                 <*> fromStringMay (LBS8.unpack h)
                 <*> pure (fromString (LBS8.unpack name))

         _          -> Nothing


gitShowRefs :: (DashBoardPerks m, MonadReader DashBoardEnv m) => LWWRefKey 'HBS2Basic -> m [(GitRef, GitHash)]
gitShowRefs what = do
  path <- repoDataPath what
  let cmd = [qc|git --git-dir {path} show-ref|]

  -- FIXME: extract-method
  gitRunCommand cmd
    >>= orThrowUser ("can't read git repo" <+> pretty path)
    <&> LBS8.lines
    <&> fmap LBS8.words
    <&> mapMaybe \case
         [val,name] -> (GitRef (LBS8.toStrict name),) <$> fromStringMay @GitHash (LBS8.unpack val)
         _          -> Nothing




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

              dir <- asks (view dataDir) <&> (</> (show $ pretty lww))

              here <- doesDirectoryExist dir

              unless here do
                debug $ red "INIT DATA DIR" <+> pretty dir
                mkdir dir
                void $ runProcess $ shell [qc|git --git-dir {dir} init --bare|]

              let cmd = [qc|git --git-dir {dir} hbs2 import {show $ pretty lww}|]
              debug $ red "SYNC" <+> pretty cmd
              void $ runProcess $ shell cmd

              lift $ buildCommitTreeIndex dir


main :: IO ()
main = do
  execParser opts & join
  where
    opts = O.info (configParser <**> helper)
      ( fullDesc
     <> progDesc "hbs2-git-dashboard"
     <> O.header "hbs2-git-dashboard" )


