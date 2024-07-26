{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.CLI.Prelude
import HBS2.CLI.Run
import HBS2.CLI.Run.KeyMan
import HBS2.CLI.Run.Keyring
import HBS2.CLI.Run.GroupKey
import HBS2.CLI.Run.Sigil
import HBS2.CLI.Run.MetaData
import HBS2.CLI.Run.Peer


import HBS2.Data.Types.Refs
import HBS2.Misc.PrettyStuff as All
import HBS2.System.Logger.Simple.ANSI as All

import HBS2.Peer.RPC.Client.Unix

import HBS2.Peer.Proto hiding (request)
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()

import HBS2.KeyMan.Keys.Direct
import HBS2.KeyMan.App.Types

import Data.Coerce
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import System.Environment

type RefLogId = PubKey 'Sign 'HBS2Basic


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


getCredentialsForReflog :: MonadUnliftIO m => String -> m (PeerCredentials 'HBS2Basic)
getCredentialsForReflog reflog = do
  puk <- orThrow (BadValueException reflog) (fromStringMay @(RefLogKey HBS2Basic) reflog)
  runKeymanClient (loadCredentials puk)
     >>= orThrowUser "credentials not found"

mkRefLogUpdateFrom :: MonadUnliftIO m => m ByteString -> String -> m (Syntax C)
mkRefLogUpdateFrom mbs  reflog = do
  what <- getCredentialsForReflog reflog
  let puk = view peerSignPk what
  let privk = view peerSignSk what
  txraw <- mbs
  w <- makeRefLogUpdate @L4Proto @'HBS2Basic (coerce puk) privk txraw
  let s = show $ pretty $ AsBase58 (serialise w)
  pure $ mkForm "cbor:base58" [ mkStr s ]



helpList :: MonadUnliftIO m => Maybe String -> RunM c m ()
helpList p = do

  let match = maybe (const True) (Text.isPrefixOf . Text.pack) p

  d <- ask >>= readTVarIO <&> fromDict
  let ks = [k | Id k <- List.sort (HM.keys d)
           , match k
           ]

  display_ $ vcat (fmap pretty ks)


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

        entry $ bindMatch "help" $ nil_ $ \syn -> do

            display_ $ "hbs2-cli tool" <> line

            case syn of
              (StringLike p : _) -> do
                helpList (Just p)

              [ListVal (SymbolVal "lambda" : SymbolVal what : _ )] -> do
                liftIO $ hPutDoc stdout $
                  "function" <+> ul (pretty what)
                  <> line

              _ -> helpList Nothing


        entry $ bindMatch "debug:cli:show" $ nil_ \case
          _ -> display cli


        entry $ bindMatch "hbs2:reflog:tx:create-raw" $ \case
          [SymbolVal "stdin", StringLike reflog] -> do
            mkRefLogUpdateFrom ( liftIO BS.getContents ) reflog

          [LitStrVal s, StringLike reflog] -> do
            mkRefLogUpdateFrom ( pure (TE.encodeUtf8 s) ) reflog

          _ -> throwIO (BadFormException @C nil)


  case cli of
    [ListVal [SymbolVal "stdin"]] -> do
      what <- getContents
                >>= either (error.show) pure . parseTop
      void $ run dict what

    [] -> do
      void $ run dict [mkForm  "help" []]

    _ -> do
      void $ run dict cli

