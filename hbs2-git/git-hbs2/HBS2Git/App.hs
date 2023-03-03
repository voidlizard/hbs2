{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2Git.App
  ( module HBS2Git.App
  , module HBS2Git.Types
  )
  where

import HBS2.Prelude
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.OrDie
import HBS2.Hash
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Git.Types

import HBS2Git.Types
import HBS2Git.Config as Config
import HBS2Git.State

import Data.Config.Suckless

import Data.Either
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Set  qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform
import System.Directory
-- import System.FilePath
import System.FilePath
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import Network.HTTP.Simple
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue as Q
import System.Exit

logPrefix s = set loggerTr (s <>)

tracePrefix :: SetLoggerEntry
tracePrefix  = toStderr . logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = toStderr . logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = toStderr . logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = toStderr . logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = toStderr

infoPrefix :: SetLoggerEntry
infoPrefix = toStdout

instance HasCfgKey ConfBranch (Set String) where
  key = "branch"

instance HasCfgKey HeadBranch (Maybe GitRef) where
  key = "head-branch"

shutUp :: MonadIO m => m ()
shutUp = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE

data WithLog = NoLog | WithLog

withApp :: MonadIO m => AppEnv -> App m a -> m a
withApp env m = runReaderT (fromApp m) env

runApp :: MonadIO m => WithLog -> App m () -> m ()
runApp l m = do

  case l of
    NoLog   -> pure ()
    WithLog -> do
      setLogging @DEBUG  debugPrefix
      setLogging @ERROR  errorPrefix
      setLogging @NOTICE noticePrefix
      setLogging @TRACE  tracePrefix
      setLogging @INFO   infoPrefix

  (pwd, syn) <- Config.configInit

  xdgstate <- liftIO $ getXdgDirectory XdgData Config.appName
  -- let statePath = xdgstate </> makeRelative home pwd
  -- let dbPath = statePath </> "state.db"
  -- db <- dbEnv dbPath
  -- trace $ "state" <+> pretty statePath
  -- here <- liftIO $ doesDirectoryExist statePath
  -- unless here do
  --   liftIO $ createDirectoryIfMissing True statePath
  -- withDB db stateInit

  -- FIXME: hardcoded-hbs2-peer
  (_, o, _) <- readProcess (shell [qc|hbs2-peer poke|])

  trace $ pretty (LBS.unpack o)

  let dieMsg = "hbs2-peer is down or it's http is inactive"

  let answ = parseTop (LBS.unpack o) & fromRight mempty

  let po = headMay [ n | ListVal (Key "http-port:" [LitIntVal n]) <- answ  ]

  -- shutUp

  pnum <- pure po `orDie`  dieMsg

  debug $ pretty "using http port" <+> pretty po

  let req = [qc|http://localhost:{pnum}/cat|]

  let env = AppEnv pwd (pwd </> ".git") syn xdgstate req

  runReaderT (fromApp m) env

  debug $ vcat (fmap pretty syn)

  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE
  setLoggingOff @INFO

readBlock :: MonadIO m => HashRef -> App m (Maybe ByteString)
readBlock h = do
  req1 <- asks (view appPeerHttpCat)
  let reqs = req1 <> "/" <> show (pretty h)
  req  <- liftIO $ parseRequest reqs
  httpLBS req <&> getResponseBody <&> Just

readRefValue :: MonadIO m => HashRef -> App m (Maybe HashRef)
readRefValue r = do
  pure Nothing

-- FIXME: readObject is dangerous!
readObject :: forall m . MonadIO m => HashRef -> App m (Maybe ByteString)
readObject obj = do

  res <- liftIO newTQueueIO

  let walk h = walkMerkle h (readBlock @m . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
        case hr of
          Left hx -> void $ liftIO$ die $ show $ "missed block:" <+> pretty hx
          Right (hrr :: [HashRef]) -> do
             forM_ hrr $ \(HashRef hx) -> do
                 mblk <- readBlock (HashRef hx)
                 case mblk of
                   Nothing  -> void $ liftIO$ die $ show $ "missed block:" <+> pretty hx
                   Just blk -> liftIO $ atomically $ Q.writeTQueue res blk

  pieces <- liftIO $ atomically $ flushTQueue res

  pure $ Just $ mconcat pieces

storeObject :: MonadIO m => ByteString -> ByteString -> App m (Maybe HashRef)
storeObject = storeObjectHBS2Store

-- FIXME: support-another-apis-for-storage
storeObjectHBS2Store :: MonadIO m => ByteString -> ByteString -> App m (Maybe HashRef)
storeObjectHBS2Store meta bs = do

  let meta58 = show $ pretty $ B8.unpack $ toBase58 (LBS.toStrict meta)

  -- trace $ "meta58" <+> pretty meta58

  let input = byteStringInput bs
  let cmd = setStdin input $ setStderr closed
                           $ shell [qc|hbs2 store --short-meta-base58={meta58}|]

  (_, out, _) <- liftIO $ readProcess cmd

  case LBS.words out of
    ["merkle-root:", h] -> pure $ Just $ fromString (LBS.unpack h)
    _                   -> pure Nothing


makeDbPath :: MonadIO m => HashRef -> App m FilePath
makeDbPath h = asks (view appStateDir) <&> (</> (show $ pretty h))

