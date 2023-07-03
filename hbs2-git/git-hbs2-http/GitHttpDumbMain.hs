module Main where

import HBS2.Prelude.Plated
import HBS2.Clock

import HBS2Git.App
import HBS2Git.State
import HBS2Git.Import (getLogFlags, importRefLogNew)
import HBS2Git.GitRepoLog
import HBS2.Git.Types
import HBS2.Data.Types.Refs
import HBS2.Data.Detect (deepScan,ScanLevel(..))
import HBS2.OrDie

import HBS2.System.Logger.Simple

import Codec.Compression.Zlib (compress)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function
import Data.Functor
import Data.HashMap.Strict as HashMap
import Data.List (sortOn)
import Data.Text.Lazy qualified as Text
import Lens.Micro.Platform
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger
import System.Directory
import System.FilePath.Posix
import System.IO.Temp
import System.Timeout (timeout)
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.Async

import Streaming.ByteString qualified as SB
import Streaming.Zip qualified as SZip

import Web.Scotty

instance Parsable RepoRef where
  parseParam txt = fromStringMay @RepoRef (Text.unpack txt)
                    & maybe (Left [qc|{txt}|]) Right

instance Parsable GitRef where
  parseParam txt = Right $ fromString (Text.unpack txt)

getAppStatePath :: MonadIO m => RepoRef -> m FilePath
getAppStatePath repo = do
  stateDir <- getAppStateDir
  pure $ stateDir </> show (pretty repo)


blockSource :: MonadIO m => API -> HashRef -> SB.ByteStream m ()
blockSource api h = do
  deepScan ScanDeep (const none) (fromHashRef h) (readBlockFrom api . HashRef) $ \ha -> do
    sec <- lift $ readBlockFrom api (HashRef ha) `orDie` [qc|missed block {pretty ha}|]
    when (h /= HashRef ha) do
      SB.fromLazy sec

unpackObjects :: MonadIO m => API -> HashRef -> FilePath -> m ()
unpackObjects catApi lo path = do

  let logPath = path </> show (pretty lo)
  let logFile = logPath </> "data"

  liftIO $ createDirectoryIfMissing True logPath

  flags <- getLogFlags (readBlockFrom catApi) lo
  let gzipped = or $ False : [ True | "gz" <- universeBi flags ]
  let unzipped = if gzipped then SZip.gunzip else id
  debug $ "GOT FLAGS:" <+> pretty lo <+> pretty flags

  liftIO $ do
    runResourceT $ SB.writeFile logFile $ unzipped $ blockSource catApi lo

    gitRepoLogScan True logFile $ \entry mlbs -> do

      let tp = case view gitLogEntryType entry of
                 GitLogEntryCommit -> Just Commit
                 GitLogEntryTree   -> Just Tree
                 GitLogEntryBlob   -> Just Blob
                 _                 -> Nothing


      let r = (,,) <$> tp
                   <*> view gitLogEntryHash entry
                   <*> mlbs

      maybe1 r none $ \(t, eh, lbs) -> do
        let fname = logPath </> show (pretty eh)
        let pref = fromString (show (pretty t) <> " " <> show (LBS.length lbs) <> "\0")
        -- debug $ "writing object" <+> pretty eh <+> pretty (LBS.unpack $ LBS.takeWhile (/= '\0') pref)
        let co = compress (pref <> lbs)
        liftIO $ LBS.writeFile fname co

retryFor :: RealFrac r => Int -> r -> Timeout 'Seconds  -> IO a -> IO (Maybe a)
retryFor num waity sleep action = timeout (ceiling $ waity * 1000000) $ go num
  where
    go 0 = action
    go n = ( (Just <$> action) `catch` handler ) >>= maybe (go (n-1)) pure
    handler (_ :: SomeException) = pause @'Seconds sleep >> pure Nothing

dumbHttpServe :: MonadUnliftIO m => Port -> m ()
dumbHttpServe pnum = do

  locks <- liftIO $ newMVar (HashMap.empty @HashRef @(MVar ()))

  catApi <- liftIO (retryFor 100 30 0.5 detectHBS2PeerCatAPI) `orDie` [qc|Can't locate hbs2-peer API|]

  notice $ "hbs2-peer API:" <+> pretty catApi

  -- TODO: lru-like-cache-for-unpacked-logs
  --  Деражть кэш, обновлять в нём таймстемпы
  --  доступа к логам.
  --  как только запись протухла - сносить каталог
  --  с логом, тогда в следующий раз будет обратно
  --  распакован

  updater <- async $ forever do
    pause @'Seconds 300
    pure ()

  runResourceT do

    let myTempDir = "hbs-git-http"
    temp <- liftIO getCanonicalTemporaryDirectory

    (_,dir) <- allocate (createTempDirectory temp myTempDir) removeDirectoryRecursive


    liftIO $ scotty pnum $ do

      middleware logStdoutDev

      get "/:repo/info/refs" $ do
        repo <- param @RepoRef "repo"
        res <- liftIO do
          db <- makeDbPath repo >>= dbEnvReadOnly
          refs <- withDB db stateGetActualRefs
          let answ = Text.unlines $ Text.pack <$> [ show (pretty h) <> "\t" <> show (pretty r) | (r,h) <- refs  ]
          shutdownDB db
          pure answ

        text res

      -- | REPO OBJECT REF
      get (regex "^/(.+)/(refs/.+)$") $ do
        repo <-  fromString <$> param "1"   -- reflog
        ref  <- param "2"  -- refname
        val <- liftIO do
          db <- makeDbPath repo >>= dbEnvReadOnly
          debug $ "QUERY: " <+> pretty ref
          val <- withDB db $ stateGetActualRefValue ref
          shutdownDB db
          pure val

        maybe1 val (status status404) $ \x -> do
          text $ Text.pack $ show $ pretty x

      get "/:repo/objects/:dd/:rest" $ do
        repo <- param @RepoRef "repo"
        dd   <- param @String "dd"
        rest <- param @String "rest"
        let ha = fromString ( dd <> rest )

        res <- runMaybeT do
                 db <- liftIO $ makeDbPath repo >>= dbEnvReadOnly
                 lo <- MaybeT $ liftIO $ withDB db $ stateGetGitLogObject ha
                 shutdownDB db

                 let logDir = dir </> show (pretty lo)
                 let fname = logDir </> show (pretty ha)

                 here <- liftIO $ doesFileExist fname

                 if here then do
                   pure fname
                 else do
                    lock <- liftIO $ getLock locks lo
                    MaybeT $ liftIO $ withMVar lock $ \_ -> do
                      unpackObjects catApi lo dir
                      here1 <- liftIO $ doesFileExist fname
                      if here1 then do
                        pure (Just fname)
                      else do
                        pure Nothing

        maybe1 res (status status404) $ \r -> do
          addHeader "content-type" "application/octet-stream"
          file r

      get "/:topic/HEAD" $ do
        repo <- fromString <$> param "topic"
        headRef <- liftIO $ do
          db <- liftIO $ makeDbPath repo >>= dbEnvReadOnly
          re <- withDB db stateGetActualRefs <&> headMay . sortOn guessHead . fmap (view _1)
          shutdownDB db
          pure re

        case headRef of
          Nothing -> status status404
          Just hd -> do
            text [qc|ref: {show $ pretty $ hd}|]

  cancel updater

  where
    getLock locks k = modifyMVar locks $ \m -> do
        case HashMap.lookup k m of
            Just lock -> return (m, lock)
            Nothing -> do
                lock <- newMVar ()
                pure (HashMap.insert k lock m, lock)



  -- TODO: hbs2-peer-http-api-reflog-fetch
  --  Ручка, что бы сделать reflog fetch

  -- TODO: hbs2-peer-dyn-reflog-subscribe
  --   Возможность динамически подписываться на рефлоги

  -- TODO: hbs2-peer-hook-on-reflog-update
  --  нужен хук, который даст возможность обрабатывать апдейты
  --  по нужному рефлогу. нужно много где


main :: IO ()
main = do

  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix
  setLoggingOff @TRACE

  -- TODO: hbs2-git-own-config

  -- FIXME: port-number-to-config
  dumbHttpServe 4017



