{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.CLI.Prelude
import HBS2.CLI.Run
import HBS2.CLI.Run.Help
import HBS2.CLI.Run.KeyMan
import HBS2.CLI.Run.Keyring
import HBS2.CLI.Run.GroupKey
import HBS2.CLI.Run.Sigil
import HBS2.CLI.Run.MetaData
import HBS2.CLI.Run.Tree
import HBS2.CLI.Run.Peer
import HBS2.CLI.Run.RefLog
import HBS2.CLI.Run.RefChan
import HBS2.CLI.Run.LWWRef
import HBS2.CLI.Run.Mailbox

import Data.Config.Suckless.Script.File as SF

import HBS2.Peer.RPC.Client.Unix

import HBS2.Net.Auth.Schema()

import System.Environment
import System.IO qualified as IO

type RefLogId = PubKey 'Sign 'HBS2Basic

{- HLINT ignore "Functor law" -}


setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStderr . logPrefix ""
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
main = do

  setupLogger

  cli <- getArgs <&> unlines . fmap unwords . splitForms
           >>= either (error.show) pure . parseTop

  let runScript dict argz what = liftIO do
        script <- either (error.show) pure $ parseTop what
        runHBS2Cli $ recover $ runM dict do
          bindCliArgs argz
          void $ evalTop script

  let dict = makeDict do

        internalEntries
        keymanEntries
        keyringEntries
        groupKeyEntries
        sigilEntries
        treeEntries
        metaDataEntries
        peerEntries
        reflogEntries
        refchanEntries
        lwwRefEntries
        mailboxEntries
        helpEntries

        SF.entries

        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

        entry $ bindMatch "debug:cli:show" $ nil_ \case
          _ -> display cli

        entry $ bindMatch "#!" $ nil_ $ const none

        entry $ bindMatch "stdin" $ nil_ $ \case
          argz  -> do
            liftIO getContents >>= runScript dict argz

        entry $ bindMatch "file" $ nil_ $ \case
          ( StringLike fn : argz ) -> do
            liftIO (readFile fn) >>= runScript dict argz

          e -> error (show $ pretty $ mkList e)

  runHBS2Cli do

    -- error (show $ pretty cli)

    case cli of

      ( cmd@(ListVal [StringLike "file", StringLike fn]) : _ ) -> do
        void $ run dict [cmd]

      ( cmd@(ListVal [StringLike "stdin"]) : _ ) -> do
        void $ run dict [cmd]

      ( cmd@(ListVal [StringLike "--help"]) : _ ) -> do
        void $ run dict [cmd]

      [] -> do
        eof <- liftIO IO.isEOF
        if eof then
          void $ run dict [mkForm  "help" []]
        else do
          what <- liftIO getContents
                    >>= either (error.show) pure . parseTop

          recover $ run dict what >>= eatNil display

      _ -> do
        recover $ run dict cli >>= eatNil display

