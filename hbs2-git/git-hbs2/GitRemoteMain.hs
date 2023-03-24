module Main where

import HBS2.Prelude
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.OrDie
import HBS2.Git.Types
import HBS2.Git.Local.CLI

import HBS2.System.Logger.Simple

import HBS2Git.Types()
import HBS2Git.Types qualified as G
import HBS2Git.App
import HBS2Git.State
import HBS2Git.Update
import HBS2Git.Export
import HBS2Git.Config as Config

import GitRemoteTypes
import GitRemotePush

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
import System.Environment
import System.Exit qualified as Exit
import System.Posix.Signals
import System.ProgressBar
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.IO as UIO
import Control.Monad.Trans.Maybe


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


readHeadDef :: HasCatAPI m => DBEnv -> m LBS.ByteString
readHeadDef db  =
  withDB db stateGetHead >>=
   \r' -> maybe1 r' (pure "\n") \r -> do
          readObject r <&> fromMaybe "\n"

loop :: forall m . ( MonadIO m
                   , HasProgress (RunWithConfig (GitRemoteApp m))
                   ) => [String] -> GitRemoteApp m ()
loop args = do


  -- setLogging @TRACE tracePrefix

  trace $ "args:" <+> pretty args

  let ref' = case args of
              [_, s] -> Text.stripPrefix "hbs2://" (Text.pack s) <&> fromString @RepoRef . Text.unpack
              _      -> Nothing

  ref <- pure ref' `orDie` ("invalid reference: "  <> show args)

  trace $ "ref:" <+> pretty ref

  dbPath <- makeDbPath ref

  trace $ "dbPath:" <+> pretty dbPath

  db <- dbEnv dbPath

  --FIXME: git-fetch-second-time
  -- Разобраться, почему git fetch срабатывает со второго раза

  checkRef <- readRef ref <&> isJust

  unless checkRef do
    warn $ "reference" <+> pretty ref <+> "missing"
    warn "trying to init reference --- may be it's ours"
    liftIO $ runApp NoLog (runExport Nothing ref)

  hdRefOld <- readHeadDef db

  -- updateLocalState ref
  -- hd <- readHeadDef db

  hashes <- withDB db stateGetAllObjects

  -- FIXME: asap-get-all-existing-objects-or-all-if-clone
  --   если clone - доставать всё
  --   если fetch - брать список объектов и импортировать
  --   только те, которых нет в репо

  existed <- gitListAllObjects <&> HashSet.fromList

  jobz <- liftIO newTQueueIO

  jobNumT <- liftIO $ newTVarIO 0
  liftIO $ atomically $ for_ hashes $ \o@(_,gh,_) -> do
    unless (HashSet.member gh existed) do
      modifyTVar' jobNumT succ
      writeTQueue jobz o

  env <- ask

  batch <- liftIO $ newTVarIO False

  fix \next -> do

    eof <- done

    when eof do
      exitFailure

    s <- receive

    let str = BS.unwords (BS.words s)
    let cmd = BS.words str

    -- trace $ pretty (fmap BS.unpack cmd)
    -- hPrint stderr $ show $ pretty (fmap BS.unpack cmd)
    --

    isBatch <- liftIO $ readTVarIO batch

    case cmd of
      [] -> do
        liftIO $ atomically $ writeTVar batch False
        sendEol
        when isBatch next

      ["capabilities"] -> do
          trace $ "send capabilities" <+> pretty (BS.unpack capabilities)
          send capabilities >> sendEol
          next

      ["list"] -> do

        updateLocalState ref
        hd <- readHeadDef db

        hl <- liftIO $ readTVarIO jobNumT
        pb <- newProgressMonitor "storing git objects" hl

        -- FIXME: thread-num-hardcoded
        liftIO $ replicateConcurrently_ 4 $ fix \nl -> do
          atomically (tryReadTQueue jobz) >>= \case
            Nothing -> pure ()
            Just (h,_,t) -> do
              runRemoteM env do
                -- FIXME: proper-error-handling
                o <- readObject h `orDie` [qc|unable to fetch object {pretty t} {pretty h}|]
                r <- gitStoreObject (GitObject t o)

                when (isNothing r) do
                  err $ "can't write object to git" <+> pretty h

              G.updateProgress pb 1
              nl

        for_ (LBS.lines hd) (sendLn . LBS.toStrict)
        sendEol
        next

      ["list","for-push"] -> do
        for_ (LBS.lines hdRefOld) (sendLn . LBS.toStrict)
        sendEol
        next

      ["fetch", sha1, x] -> do
        trace $ "fetch" <+> pretty (BS.unpack sha1) <+> pretty (BS.unpack x)
        liftIO $ atomically $ writeTVar batch True
        sendEol
        next

      ["push", rr] -> do
        let bra = BS.split ':' rr
        let pu = fmap (fromString' . BS.unpack) bra
        liftIO $ atomically $ writeTVar batch True
        pushed <- push ref pu
        case pushed of
          Nothing  -> sendEol
          Just re -> sendLn [qc|ok {pretty re}|]
        next

      other -> die $ show other

  where
    fromString' "" = Nothing
    fromString' x  = Just $ fromString x

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
  setLogging @WARN   warnPrefix
  setLogging @INFO   infoPrefix

  args <- getArgs

  void $ installHandler sigPIPE Ignore Nothing

  env <- RemoteEnv <$> detectHBS2PeerCatAPI
                   <*> detectHBS2PeerSizeAPI
                   <*> liftIO (newTVarIO mempty)

  runRemoteM env do
    loop args

  shutUp


