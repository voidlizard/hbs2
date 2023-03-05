{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.OrDie
import HBS2.Git.Types
import HBS2.Git.Local.CLI

import HBS2.System.Logger.Simple

import HBS2Git.App
import HBS2Git.State

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable
import Data.Functor
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform
import System.Environment
import System.Exit qualified as Exit
import System.Posix.Signals
import System.ProgressBar
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.IO as UIO

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
sendLn s = do
  trace $ "sendLn" <+> pretty (show s)
  liftIO $ BS.hPutStrLn stdout s

sendEol :: MonadIO m => m ()
sendEol = liftIO $ BS.hPutStrLn stdout "" >> hFlush stdout

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

  -- FIXME: asap-get-all-existing-objects-or-all-if-clone
  --   если clone - доставать всё
  --   если fetch - брать список объектов и импортировать
  --   только те, которых нет в репо

  existed <- gitListAllObjects <&> HashSet.fromList

  jobz <- liftIO newTQueueIO

  -- TODO: check-if-fetch-really-works
  -- TODO: check-if-fetch-actually-works

  jobNumT <- liftIO $ newTVarIO 0
  liftIO $ atomically $ for_ hashes $ \o@(_,gh,_) -> do
    unless (HashSet.member gh existed) do
      modifyTVar' jobNumT succ
      writeTQueue jobz o

  hl <- liftIO $ readTVarIO jobNumT
  pb <- liftIO $ newProgressBar defStyle 10 (Progress 0 hl ())

  env <- ask

  -- FIXME: thread-num-hardcoded
  liftIO $ replicateConcurrently_ 4 $ fix \next -> do
    atomically (tryReadTQueue jobz) >>= \case
      Nothing -> pure ()
      Just (h,_,t) -> do
        runRemoteM env do
          -- FIXME: proper-error-handling
          o <- readObject h `orDie` [qc|unable to fetch object {pretty t} {pretty h}|]
          r <- gitStoreObject (GitObject t o)

          when (isNothing r) do
            err $ "can't write object to git" <+> pretty h

        liftIO $ incProgress pb 1
        next

  -- shutUp

  fix \next -> do

    eof <- done
    when eof exitFailure

    s <- receive

    let str = BS.unwords (BS.words s)
    let cmd = BS.words str

    trace $ pretty (fmap BS.unpack cmd)

    case cmd of
      [] -> do
        sendEol
        shutUp
        exitSuccess

      ["capabilities"] -> do
          trace $ "send capabilities" <+> pretty (BS.unpack capabilities)
          send capabilities >> sendEol

      ("list":xs) -> do
        for_ (LBS.lines hd) (sendLn . LBS.toStrict)
        sendEol
        next

      ["fetch", sha1, x] -> do
        trace $ "fetch" <+> pretty (BS.unpack sha1) <+> pretty (BS.unpack x)
        next

      other -> die $ show other

    next

main :: IO ()
main = do

  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout LineBuffering

  doTrace <- lookupEnv "HBS2TRACE" <&> isJust

  when doTrace do
    setLogging @DEBUG  debugPrefix
    setLogging @TRACE  tracePrefix

  setLogging @NOTICE noticePrefix
  setLogging @ERROR  errorPrefix
  setLogging @INFO   infoPrefix

  args <- getArgs

  void $ installHandler sigPIPE Ignore Nothing

  env <- RemoteEnv <$> detectHBS2PeerCatAPI

  runRemoteM env (loop args)

