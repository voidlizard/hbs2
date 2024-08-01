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
import HBS2.CLI.Run.Peer
import HBS2.CLI.Run.RefLog
import HBS2.CLI.Run.RefChan
import HBS2.CLI.Run.LWWRef

import HBS2.Peer.RPC.Client.Unix

import HBS2.Net.Auth.Schema()

import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as Text
import System.Environment

type RefLogId = PubKey 'Sign 'HBS2Basic

{- HLINT ignore "Functor law" -}


setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
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

  let dict = makeDict do

        internalEntries
        keymanEntries
        keyringEntries
        groupKeyEntries
        sigilEntries
        metaDataEntries
        peerEntries
        reflogEntries
        refchanEntries
        lwwRefEntries
        helpEntries

        entry $ bindMatch "debug:cli:show" $ nil_ \case
          _ -> display cli


  case cli of
    [ListVal [SymbolVal "stdin"]] -> do
      what <- getContents
                >>= either (error.show) pure . parseTop
      run dict what >>= eatNil display

    [] -> do
      void $ run dict [mkForm  "help" []]

    _ -> do
      run dict cli >>= eatNil display

