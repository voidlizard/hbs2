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
import HBS2.System.Logger.Simple

import HBS2Git.Types
import HBS2Git.Config as Config
import HBS2Git.State

import Data.Config.Suckless

import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Set  qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform
import System.Directory
-- import System.FilePath
import System.FilePath
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)

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
noticePrefix = toStderr . logPrefix ""

instance HasCfgKey ConfBranch (Set String) where
  key = "branch"

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

  let env = AppEnv pwd (pwd </> ".git") syn xdgstate
  runReaderT (fromApp m) env

  debug $ vcat (fmap pretty syn)

  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE

storeObject :: MonadIO m => ByteString -> ByteString -> App m (Maybe HashRef)
storeObject = storeObjectHBS2Store

-- FIXME: support-another-apis-for-storage
storeObjectHBS2Store :: MonadIO m => ByteString -> ByteString -> App m (Maybe HashRef)
storeObjectHBS2Store meta bs = do

  let meta58 = pretty $ AsBase58 $ toBase58 (LBS.toStrict meta)

  let input = byteStringInput bs
  let cmd = setStdin input $ setStderr closed
                           $ shell [qc|hbs2 store --short-meta-base58={meta58}|]

  (_, out, _) <- liftIO $ readProcess cmd

  case LBS.words out of
    ["merkle-root:", h] -> pure $ Just $ fromString (LBS.unpack h)
    _                   -> pure Nothing


