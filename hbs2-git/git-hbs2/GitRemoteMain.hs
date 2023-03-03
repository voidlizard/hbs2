{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.OrDie
import HBS2.Git.Types
import HBS2.Git.Local.CLI

import HBS2.System.Logger.Simple

import HBS2Git.Types
import HBS2Git.App
import HBS2Git.State

import Control.Monad.Reader
import Data.Maybe
import Data.Function
import Data.Functor
import Data.Foldable
-- import Data.Attoparsec.ByteString.Char8 qualified as Atto8
import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as Atto
import UnliftIO.IO as UIO
-- import System.IO
import System.Exit qualified as Exit
import System.Environment
-- import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import System.Posix.Signals
import Lens.Micro.Platform
import System.Directory

exitSuccess :: MonadIO m => m ()
exitSuccess = liftIO Exit.exitSuccess

exitFailure :: MonadIO m => m ()
exitFailure = liftIO Exit.exitFailure

die :: MonadIO m => String -> m a
die s = do
  liftIO $ Exit.die s

send :: MonadIO m => BS.ByteString -> m ()
send = liftIO . BS.hPutStr stdout

sendLn :: MonadIO m => BS.ByteString -> m ()
sendLn s = liftIO $ BS.hPutStrLn stdout s >> hFlush stdout

sendEol :: MonadIO m => m ()
sendEol = liftIO $ BS.hPutStrLn stdout "\n" >> hFlush stdout

receive :: MonadIO m => m BS.ByteString
receive = liftIO $ BS.hGetLine stdin

done :: MonadIO m => m Bool
done = UIO.hIsEOF stdin

parseRepoURL :: String -> Maybe HashRef
parseRepoURL url' = either (const Nothing) Just (parseOnly p url)
  where
    url = Text.pack url'
    p = do
      _ <- string "hbs2://"
      topic'  <- Atto.manyTill' anyChar endOfInput
      let topic = BS.unpack <$> fromBase58 (BS.pack topic')
      maybe (fail "invalid url") (pure . fromString) topic


capabilities :: BS.ByteString
capabilities = BS.unlines ["push","fetch"]

newtype RemoteEnv =
  RemoteEnv
  { _reHttpCat :: API
  }

makeLenses 'RemoteEnv


newtype GitRemoteApp m a =
  GitRemoteApp { fromRemoteApp :: ReaderT RemoteEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader RemoteEnv
                   )

runRemoteM :: MonadIO m => RemoteEnv -> GitRemoteApp m a -> m a
runRemoteM env m = runReaderT (fromRemoteApp m) env

instance MonadIO m => HasCatAPI (GitRemoteApp m) where
  getHttpCatAPI = view (asks reHttpCat)

loop :: MonadIO m => [String] -> GitRemoteApp m ()
loop args = do

  trace $ "ref:" <+> pretty args

  let ref' = case args of
              [_, s] -> Text.stripPrefix "hbs2://" (Text.pack s) <&> fromString @HashRef . Text.unpack
              _      -> Nothing

  ref <- pure ref' `orDie` ("invalid reference: "  <> show args)

  trace $ "ref:" <+> pretty ref

  -- TODO: having ref value import repository

  dbPath <- makeDbPath ref

  trace $ "dbPath:" <+> pretty dbPath

  db <- dbEnv dbPath

  hdRef <- withDB db stateGetHead `orDie` "can't read head for repository"

  trace $ "head read:" <+> pretty hdRef

  hd <- readObject hdRef `orDie` "can't read head block for repository"

  trace $ "head is:" <+> pretty (LBS.unpack hd)

  hashes <- withDB db stateGetAllObjects

  -- FIXME: asap-get-all-existing-objectsor-none-if-clone

  for_ hashes $ \(h,t) -> do
    o <- readObject h `orDie` "unable to fetch object from hbs2"
    trace $ "wtf" <+> pretty h
    -- gitStoreObject (GitObject t o)

  -- shutUp

  fix \next -> do

    eof <- done
    when eof exitFailure

    s <- receive

    let str = BS.unwords (BS.words s)
    let cmd = BS.words str

    trace $ pretty (fmap BS.unpack cmd)

    hFlush stderr


    case cmd of
      [] -> do
        shutUp
        exitSuccess

      ["capabilities"] -> do
          trace $ "send capabilities" <+> pretty (BS.unpack capabilities)
          send capabilities
          sendEol

      ("list":xs) -> do
        -- for_ (LBS.lines hd) (trace . pretty . BS.unpack . LBS.toStrict)
        --
        -- pwd <- liftIO getCurrentDirectory
        -- trace $ "PWD" <+> pretty pwd <+> pretty args <+> pretty (fmap BS.unpack xs)
        -- git <- liftIO $ doesDirectoryExist $ pwd
        -- trace $ "importing" <+> pretty pwd <+> pretty t <+> pretty h

        for_ (LBS.lines hd) (sendLn . LBS.toStrict)

      other -> die $ show other

    next

main :: IO ()
main = do

  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

  setLogging @DEBUG  debugPrefix
  setLogging @ERROR  errorPrefix
  setLogging @NOTICE noticePrefix
  setLogging @TRACE  tracePrefix
  setLogging @INFO   infoPrefix

  args <- getArgs

  void $ installHandler sigPIPE Ignore Nothing

  env <- RemoteEnv <$> detectHBS2PeerCatAPI

  runRemoteM env (loop args)

