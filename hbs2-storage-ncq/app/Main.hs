{-# Language ViewPatterns #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Storage.NCQ3
import HBS2.System.Logger.Simple.ANSI
import HBS2.Storage.NCQ3.Internal.CLI as CLI

import Data.Config.Suckless.Script

import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict (HashMap)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Coerce
import System.Environment
import System.IO qualified as IO
import UnliftIO


setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


main :: IO ()
main = do

  instances <- initInstances

  tvd <- newTVarIO mempty

  setupLogger

  argz <- liftIO getArgs

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  let runScript dict argz what = liftIO do
        script <- either (error.show) pure $ parseTop what
        runM dict do
          bindCliArgs argz
          evalTop script

  let dict = makeDict do

       internalEntries

       entry $ bindMatch "--help" $ nil_ \case
         HelpEntryBound what -> helpEntry what
         [StringLike s]      -> helpList False (Just s)
         _                   -> helpList False Nothing

       entry $ bindMatch "#!" $ nil_ $ const none

       entry $ bindMatch "stdin" $ nil_ $ \case
         argz  -> do
           liftIO getContents >>= runScript dict argz

       entry $ bindMatch "file" $ nil_ $ \case
         ( StringLike fn : argz ) -> do
           liftIO (readFile fn) >>= runScript dict argz

         e -> error (show $ pretty $ mkList e)

       entry $ bindMatch "debug" $ nil_ \case

         [ LitBoolVal False ] -> do
            setLoggingOff @DEBUG

         [ StringLike "off" ] -> do
            setLoggingOff @DEBUG

         _ ->
            setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

       CLI.entries instances

  atomically $ writeTVar tvd dict

  flip runContT pure do

    ContT $ bracket none $ const do
      finalizeInstances instances
      flushLoggers

    eatNil display =<< lift do
      case forms of

        ( cmd@(ListVal [StringLike "file", StringLike fn]) : _ ) -> do
          run dict [cmd]

        ( cmd@(ListVal [StringLike "stdin"]) : _ ) -> do
          run dict [cmd]

        ( cmd@(ListVal [StringLike "--help"]) : _ ) -> do
          run dict [cmd]

        [] -> do
          eof <- liftIO IO.isEOF
          if eof then
            run dict [mkForm  "help" []]
          else do
            what <- liftIO getContents
                      >>= either (error.show) pure . parseTop

            run dict what

        e -> run dict e


