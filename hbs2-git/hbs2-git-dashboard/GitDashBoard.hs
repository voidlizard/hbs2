{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.System.Dir
import HBS2.System.Logger.Simple.ANSI hiding (info)

import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Merkle
import HBS2.Storage
import HBS2.Net.Messaging.Unix
import HBS2.OrDie
import HBS2.Misc.PrettyStuff

import HBS2.Net.Proto.Service
import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient


import HBS2.Git.Web.Assets
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.Types
import HBS2.Git.Web.Html.Root

import HBS2.Peer.CLI.Detect

import Data.Config.Suckless

import DBPipe.SQLite

import Lucid
import Options.Applicative as O
import Data.Maybe
import Data.Either
import Control.Applicative
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static hiding ((<|>))
import Network.Wai.Middleware.StaticEmbedded as E
import Network.Wai.Middleware.RequestLogger
import Text.InterpolatedString.Perl6 (qc)
import Web.Scotty.Trans
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import System.Directory
import Control.Monad.Except
import Control.Monad.Trans.Cont

import UnliftIO



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
              ( command "web" (info pRunWeb (progDesc "Run the web interface")) )

  pure $ cmd opts


pRunWeb :: DashBoardPerks m => Parser (RunDashBoardOpts -> m ())
pRunWeb = pure $ \x -> runDashBoardM x runScotty


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

  env <- newDashBoardEnv conf dbFile

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

    void $ ContT $ withAsync do
      q <- withDashBoardEnv env $ asks _pipeline
      forever do
        liftIO (atomically $ readTQueue q) & liftIO . join

    client <- liftIO $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
              >>= orThrowUser ("can't connect to" <+> pretty soname)

    void $ ContT $ withAsync $ runMessagingUnix client

    peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
    refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
    storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
    lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

    let sto = AnyStorage (StorageClient storageAPI)

    let endpoints = [ Endpoint @UNIX  peerAPI
                    , Endpoint @UNIX  refLogAPI
                    , Endpoint @UNIX  lwwAPI
                    , Endpoint @UNIX  storageAPI
                    ]

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client


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
    html =<< renderTextT (dashboardRootPage mempty)


runScotty :: DashBoardPerks m => DashBoardM m ()
runScotty  = do
    pno <- cfgValue @HttpPortOpt @(Maybe Int) <&> fromMaybe 8090
    wo  <- cfgValue @DevelopAssetsOpt @(Maybe FilePath) <&> WebOptions

    env <- ask

    addJob (withDashBoardEnv env updateIndex)

    scottyT pno (withDashBoardEnv env) (runDashboardWeb wo)


main :: IO ()
main = do
  execParser opts & join
  where
    opts = info (configParser <**> helper)
      ( fullDesc
     <> progDesc "hbs2-git-dashboard"
     <> O.header "hbs2-git-dashboard" )


