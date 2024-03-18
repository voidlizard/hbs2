{-# LANGUAGE TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Schema
import HBS2.Polling
import HBS2.System.Dir
import HBS2.System.Logger.Simple.ANSI hiding (info)

import Data.Config.Suckless

import Control.Monad.Reader
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import UnliftIO
import Options.Applicative
import Data.Maybe
import Data.Either

{- HLINT ignore "Functor law" -}

data FixerEnv = FixerEnv
  { _config :: TVar [Syntax C]
  }

makeLenses ''FixerEnv


data Watch s =
  WatchRefLog (PubKey 'Sign s)
  deriving stock (Generic)

newtype FixerM m a = FixerM { runFixerM :: ReaderT FixerEnv m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader FixerEnv, MonadUnliftIO)

withConfig :: MonadUnliftIO m => Maybe FilePath -> FixerM m () -> FixerM m ()
withConfig cfgPath m = do
  defConfDir <- liftIO $ getXdgDirectory XdgConfig "hbs2-fixer"

  let configPath = fromMaybe (defConfDir </> "config") cfgPath

  unless (isJust cfgPath) do
    debug $ pretty configPath
    touch configPath

  syn <- liftIO (readFile configPath) <&> parseTop <&> fromRight mempty
  tsyn <- newTVarIO syn

  local (set config tsyn) (void m)

withApp :: Maybe FilePath -> FixerM IO () -> IO ()
withApp cfgPath action = do
  setLogging @DEBUG debugPrefix
  setLogging @INFO  defLog
  setLogging @ERROR errorPrefix
  setLogging @WARN  warnPrefix
  setLogging @NOTICE noticePrefix
  env <- FixerEnv <$> newTVarIO mempty
  runReaderT (runFixerM $ withConfig cfgPath action) env
    `finally` do
       setLoggingOff @DEBUG
       setLoggingOff @INFO
       setLoggingOff @ERROR
       setLoggingOff @WARN
       setLoggingOff @NOTICE
       pure ()

  where
    debugPrefix  = toStdout . logPrefix "[debug] "
    errorPrefix  = toStdout . logPrefix "[error] "
    warnPrefix   = toStdout . logPrefix "[warn] "
    noticePrefix = toStdout . logPrefix "[notice] "

mainLoop :: FixerM IO ()
mainLoop = forever $ do
  debug "hbs2-fixer. do stuff since 2024"
  pause @'Seconds 5


main :: IO ()
main = do
  runMe =<< customExecParser (prefs showHelpOnError)
              ( info (helper <*> opts)
                ( fullDesc
                <> header "hbs2-fixer"
                <> progDesc "Intermediary between hbs2-peer and external applications. Listen events / do stuff"
                ))

  where
    opts = optional $ strOption (short 'c' <> long "config" <> metavar "FILE" <> help "Specify configuration file")

    runMe opt = withApp opt mainLoop

