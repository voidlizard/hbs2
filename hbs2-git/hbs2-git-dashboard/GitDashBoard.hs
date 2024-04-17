{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.System.Dir

import HBS2.Git.Web.Assets
import HBS2.Git.Web.Html.Root

import HBS2.Peer.CLI.Detect

import Data.Config.Suckless
import Data.Config.Suckless.KeyValue

import Lucid
import Options.Applicative as O
import Data.Maybe
import Data.Either
import Options.Applicative.BashCompletion
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

import UnliftIO

data HttpPortOpt

data DevelopAssetsOpt

instance HasConf m => HasCfgKey HttpPortOpt a m where
  key = "port"


instance HasConf m => HasCfgKey DevelopAssetsOpt a m where
  key = "develop-assets"

data RunDashBoardOpts = RunDashBoardOpts
  { configPath :: Maybe FilePath }


configParser :: Parser RunDashBoardOpts
configParser = RunDashBoardOpts <$>
  optional (strOption
      ( long "config"
     <> short 'c'
     <> metavar "FILEPATH"
     <> help "Path to the configuration file"
     <> completer (bashCompleter "file")
      ))

{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Functor law" -}

getRPC :: Monad m => HasConf m => m (Maybe FilePath)
getRPC = pure Nothing


data DashBoardEnv =
  DashBoardEnv
  { _dashBoardConf      :: TVar [Syntax C]
  }

newtype DashBoardM m a = DashBoardM { fromDashBoardM :: ReaderT DashBoardEnv m a }
                         deriving newtype
                         ( Applicative
                         , Functor
                         , Monad
                         , MonadIO
                         , MonadUnliftIO
                         , MonadTrans
                         , MonadReader DashBoardEnv
                         )

instance (MonadIO m, Monad m, MonadReader DashBoardEnv m) => HasConf m where
  getConf = do
    asks _dashBoardConf >>= readTVarIO


newDashBoardEnv :: MonadIO m => [Syntax C] -> m DashBoardEnv
newDashBoardEnv cfg = do
  tconf <- newTVarIO cfg
  pure $ DashBoardEnv tconf

withDashBoardEnv :: Monad m => DashBoardEnv -> DashBoardM m a -> m a
withDashBoardEnv env m = runReaderT (fromDashBoardM m) env


runDashBoardM :: MonadIO m => Maybe RunDashBoardOpts -> DashBoardM m a -> m a
runDashBoardM cli m = do

  xdg <- liftIO $ getXdgDirectory XdgConfig "hbs2-git-dashboard"
  let cliConfPath = cli >>= configPath

  let confPath = fromMaybe xdg cliConfPath
  let confFile = confPath </> "config"

  when (isNothing cliConfPath) do
    touch confFile

  conf <- runExceptT (liftIO $ readFile confFile)
           <&> fromRight mempty
           <&> parseTop
           <&> fromRight mempty

  liftIO $ print (pretty conf)

  env <- newDashBoardEnv conf

  withDashBoardEnv env m


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

main :: IO ()
main = do

  cli <- execParser opts

  runDashBoardM (Just cli) do

    -- FIXME: to-config
    pno <- cfgValue @HttpPortOpt @(Maybe Int) <&> fromMaybe 8090
    wo  <- cfgValue @DevelopAssetsOpt @(Maybe FilePath) <&> WebOptions

    soname <- runMaybeT (getRPC <|> detectRPC)
                `orDie` "hbs2-peer RPC not detected"

    env <- ask
    conf <- getConf

    scottyT pno (withDashBoardEnv env) (runDashboardWeb wo)

  where
    opts = info (configParser <**> helper)
      ( fullDesc
     <> progDesc "hbs2-git-dashboard"
     <> O.header "hbs2-git-dashboard" )


