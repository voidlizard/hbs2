module HBS2.Git.Client.App
  ( module HBS2.Git.Client.App
  , module HBS2.Git.Client.App.Types
  ) where

import HBS2.Git.Client.Prelude hiding (info)
import HBS2.Git.Client.App.Types
import HBS2.Git.Client.Config
import HBS2.Git.Client.Progress
import HBS2.Git.Client.State

import HBS2.Git.Data.Tx

import HBS2.Git.Local.CLI

import HBS2.System.Dir

import Data.Maybe
import System.Environment
import System.IO (hPutStr)
import Data.Vector qualified as V
import Data.Vector ((!))

drawProgress :: MonadUnliftIO m => ProgressQ -> m ()
drawProgress (ProgressQ q) = do

  let spin = V.fromList ["--","\\","|","/"]
  let l = V.length spin
  i <- newTVarIO 0

  tl <- newTVarIO =<< getTimeCoarse

  let updateSpinner = do
        atomically  $ modifyTVar i succ

  let getSpinner = do
        j <- readTVarIO i <&> (`mod` l)
        pure $ spin ! j

  let
    limit :: MonadIO m => Timeout 'Seconds -> m () -> m ()
    limit dt m = do
       t0  <- readTVarIO tl
       now <- getTimeCoarse
       when (expired dt (now - t0)) do
        atomically $ writeTVar tl now
        m

  let loop = do
        flip fix False \next quiet -> do

          let put s | quiet = pure ()
                    | otherwise = putStatus s

          ev <- atomically $ readTQueue q

          case ev of
            ImportIdle -> do
              next quiet

            ImportSetQuiet qq -> do
              put ""
              next qq

            ImportRefLogStart puk -> do
              put ("wait reflog" <+> pretty (AsBase58 puk))
              next quiet

            ImportRefLogDone puk Nothing -> do
              updateSpinner
              c <- getSpinner
              put ("wait reflog" <+> pretty (AsBase58 puk) <+> pretty c)
              next quiet

            ImportRefLogDone _ (Just h) -> do
              put ("reflog value" <+> pretty h)
              next quiet

            ImportWaitTx h -> do
              updateSpinner
              c <- getSpinner
              put ("wait tx data" <+> pretty h <+> pretty c)
              next quiet

            ImportScanTx h -> do
              put ("scan tx" <+> pretty h)
              next quiet

            ImportApplyTx h -> do
              put ("apply tx" <+> pretty h)
              next quiet

            ImportApplyTxError h s -> do
              limit 0.25 $ put $ red ("failed" <+> pretty s) <+> pretty h
              next quiet

            ImportReadBundleChunk meta (Progress s _) -> do
              let h = bundleHash meta
              let e = if bundleEncrypted meta then yellow "@" else ""
              limit 0.5 $ put $ "read pack" <+> e <> pretty h <+> pretty s
              next quiet

            ExportWriteObject (Progress s _) -> do
              limit 0.5 $ put $ "write object" <+> pretty s
              next quiet

            ImportAllDone -> do
              put "\n"

  loop
    `finally` do
      putStatus ""

  where
    putStatus :: MonadUnliftIO m => Doc AnsiStyle -> m ()
    putStatus s = do
      liftIO $ hPutStr stderr $ toStringANSI $ "\r" <> fill 80 "" <> "\r" <> pretty (take 74 (toStringANSI s))
      liftIO $ hFlush stderr

runGitCLI :: (GitPerks m) => [GitOption] -> GitCLI m a -> m a
runGitCLI o m = do

  soname <- runExceptT getSocketName
              >>= orThrowUser "no rpc socket"

  flip runContT pure do

    client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                >>= orThrowUser ("can't connect to" <+> pretty soname)

    void $ ContT $ withAsync $ runMessagingUnix client

    peerAPI   <- makeServiceCaller @PeerAPI (fromString soname)
    refLogAPI <- makeServiceCaller @RefLogAPI (fromString soname)
    storageAPI <- makeServiceCaller @StorageAPI (fromString soname)

    let endpoints = [ Endpoint @UNIX  peerAPI
                    , Endpoint @UNIX  refLogAPI
                    , Endpoint @UNIX  storageAPI
                    ]

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

    conf <- lift $ readConfig True

    git <- gitDir
              >>= orThrowUser "git dir not set"
              >>= canonicalizePath

    q <- lift newProgressQ
    let ip = AnyProgress q

    cpath <- lift getConfigDir

    progress <- ContT $ withAsync (drawProgress q)

    env <- lift $ newGitEnv ip o git cpath conf peerAPI refLogAPI storageAPI
    lift $ runReaderT setupLogging env
    lift $ withGitEnv env (evolveDB >> m)
      `finally` do
        onProgress ip ImportAllDone
        cancel progress
        shutDownLogging

runDefault :: GitPerks m => GitCLI m ()
runDefault = do
  pure ()

setupLogging :: (GitPerks m, HasGitOpts m) => m ()
setupLogging = do

  traceEnv <- liftIO $ lookupEnv "HBS2TRACE" <&> isJust

  setLogging @INFO   defLog
  setLogging @ERROR  (logPrefix "" . toStderr)
  setLogging @WARN   (logPrefix "" . toStderr)
  setLogging @NOTICE (logPrefix "" . toStderr)

  dbg <- debugEnabled

  when (dbg || traceEnv) do
    setLogging @DEBUG  (logPrefix "" . toStderr)

  trc <- traceEnabled

  when (trc || traceEnv) do
    setLogging @TRACE  (logPrefix "" . toStderr)

shutDownLogging :: MonadUnliftIO m => m ()
shutDownLogging  = do
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @DEBUG
  setLoggingOff @TRACE

