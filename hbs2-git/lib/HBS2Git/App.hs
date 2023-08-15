{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import HBS2.Net.Proto.Definition()
import HBS2.Net.Auth.Credentials hiding (getCredentials)
import HBS2.Net.Proto.RefLog
import HBS2.Defaults (defBlockSize)

import HBS2Git.Types
import HBS2Git.Config as Config

import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Either
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro.Platform
import System.Directory
-- import System.FilePath
import System.FilePath
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Control.Concurrent.STM
import Codec.Serialise
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Text qualified as Text
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Cache qualified as Cache
import System.Environment
import Prettyprinter.Render.Terminal

instance MonadIO m => HasCfgKey ConfBranch (Set String) m where
  key = "branch"

instance MonadIO m => HasCfgKey ConfBranch (Set GitRef) m where
  key = "branch"

instance MonadIO m => HasCfgKey HeadBranch (Maybe GitRef) m where
  key = "head-branch"

instance MonadIO m => HasCfgKey KeyRingFile (Maybe FilePath) m where
  key = "keyring"

instance MonadIO m => HasCfgKey KeyRingFiles (Set FilePath) m where
  key = "keyring"

instance MonadIO m => HasCfgKey StoragePref (Maybe FilePath) m where
  key = "storage"

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
infoPrefix = toStderr

data WithLog = NoLog | WithLog

instance MonadIO m => HasCatAPI (App m) where
  getHttpCatAPI = asks (view appPeerHttpCat)
  getHttpSizeAPI = asks (view appPeerHttpSize)
  getHttpPutAPI = asks (view appPeerHttpPut)
  getHttpRefLogGetAPI = asks (view appPeerHttpRefLogGet)

instance MonadIO m => HasRefCredentials (App m) where
  setCredentials ref cred = do
    asks (view appRefCred) >>= \t -> liftIO $ atomically $
      modifyTVar' t (HashMap.insert ref cred)

  getCredentials ref = do
    hm <- asks (view appRefCred) >>= liftIO . readTVarIO
    pure (HashMap.lookup ref hm) `orDie` "keyring not set"


withApp :: MonadIO m => AppEnv -> App m a -> m a
withApp env m = runReaderT (fromApp m) env

{-# NOINLINE hBS2PeerCatAPI #-}
hBS2PeerCatAPI :: IORef (Maybe API)
hBS2PeerCatAPI = unsafePerformIO (newIORef Nothing)

detectHBS2PeerCatAPI :: MonadIO m => m API
detectHBS2PeerCatAPI = do
  -- FIXME: hardcoded-hbs2-peer

  v <- liftIO $ readIORef hBS2PeerCatAPI

  case v of
    Just x -> pure x
    Nothing -> do
      (_, o, _) <- readProcess (shell [qc|hbs2-peer poke|])

      let dieMsg = "hbs2-peer is down or it's http is inactive"

      let answ = parseTop (LBS.unpack o) & fromRight mempty

      let po = headMay [ n | ListVal (Key "http-port:" [LitIntVal n]) <- answ  ]
      -- shutUp

      pnum <- pure po `orDie`  dieMsg

      debug $ pretty "using http port" <+> pretty po

      let api = [qc|http://localhost:{pnum}/cat|]

      liftIO $ writeIORef hBS2PeerCatAPI (Just api)

      pure api


detectHBS2PeerSizeAPI :: MonadIO m => m API
detectHBS2PeerSizeAPI = do
  api <- detectHBS2PeerCatAPI
  let new = Text.replace "/cat" "/size" $ Text.pack api
  pure $ Text.unpack new

detectHBS2PeerPutAPI :: MonadIO m => m API
detectHBS2PeerPutAPI = do
  api <- detectHBS2PeerCatAPI
  let new = Text.replace "/cat" "/" $ Text.pack api
  pure $ Text.unpack new

detectHBS2PeerRefLogGetAPI :: MonadIO m => m API
detectHBS2PeerRefLogGetAPI = do
  api <- detectHBS2PeerCatAPI
  let new = Text.replace "/cat" "/reflog" $ Text.pack api
  pure $ Text.unpack new

getAppStateDir :: forall m . MonadIO m => m FilePath
getAppStateDir = liftIO $ getXdgDirectory XdgData Config.appName

runApp :: MonadIO m => WithLog -> App m () -> m ()
runApp l m = do

  case l of
    NoLog   -> pure ()
    WithLog -> do
      setLogging @ERROR  errorPrefix
      setLogging @NOTICE noticePrefix
      setLogging @INFO   infoPrefix

  doTrace <- liftIO $ lookupEnv "HBS2TRACE" <&> isJust

  if doTrace then do
    setLogging @DEBUG  debugPrefix
    setLogging @TRACE  tracePrefix
  else do
    setLoggingOff @DEBUG
    setLoggingOff @TRACE

  pwd <- Config.getRepoDir
  (configPath, config) <- Config.configInit

  xdgstate <- getAppStateDir
  -- let statePath = xdgstate </> makeRelative home pwd
  -- let dbPath = statePath </> "state.db"
  -- db <- dbEnv dbPath
  -- trace $ "state" <+> pretty statePath
  -- here <- liftIO $ doesDirectoryExist statePath
  -- unless here do
  --   liftIO $ createDirectoryIfMissing True statePath
  -- withDB db stateInit

  reQ <- detectHBS2PeerCatAPI
  szQ <- detectHBS2PeerSizeAPI
  puQ <- detectHBS2PeerPutAPI
  rlQ <- detectHBS2PeerRefLogGetAPI

  mtCred <- liftIO $ newTVarIO mempty

  let env = AppEnv pwd (pwd </> ".git") config configPath xdgstate reQ szQ puQ rlQ mtCred

  runReaderT (fromApp m) env

  debug $ vcat (fmap pretty config)

  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE
  setLoggingOff @INFO


writeBlock :: forall m . (HasCatAPI m, MonadIO m) => ByteString -> m (Maybe (Hash HbSync))
writeBlock bs = do
  req <-  getHttpPutAPI
  writeBlockIO req bs

writeBlockIO :: forall m . MonadIO m => API -> ByteString -> m (Maybe (Hash HbSync))
writeBlockIO api bs = do
  req1 <-  liftIO $ parseRequest api
  let request = setRequestMethod "PUT"
                    $ setRequestHeader "Content-Type" ["application/octet-stream"]
                    $ setRequestBodyLBS bs req1

  resp <- httpLBS request

  case statusCode (getResponseStatus resp) of

    200 -> pure $ getResponseBody resp & LBS.unpack & fromStringMay
    _   -> pure Nothing


readBlock :: forall m . (HasCatAPI m, MonadIO m) => HashRef -> m (Maybe ByteString)
readBlock h = do
  req1 <-  getHttpCatAPI
  readBlockFrom req1 h

readBlockFrom :: forall m . (MonadIO m) => API -> HashRef -> m (Maybe ByteString)
readBlockFrom api h = do
  let reqs = api <> "/" <> show (pretty h)
  req  <- liftIO $ parseRequest reqs
  resp <- httpLBS req

  case statusCode (getResponseStatus resp) of
    200 -> pure $ Just (getResponseBody resp)
    _   -> pure Nothing


readRefHttp :: forall m . (HasCatAPI m, MonadIO m) => RepoRef -> m (Maybe HashRef)
readRefHttp re = do
  req0 <- getHttpRefLogGetAPI
  let req = req0 <> "/" <> show (pretty re)
  request <- liftIO $ parseRequest req
  resp <- httpLBS request

  case statusCode (getResponseStatus resp) of
    200 -> pure $ getResponseBody resp & LBS.unpack & fromStringMay
    _   -> pure Nothing


getBlockSize :: forall m . (HasCatAPI m, MonadIO m) => HashRef -> m (Maybe Integer)
getBlockSize h = do
  req1 <-  getHttpSizeAPI
  let reqs = req1 <> "/" <> show (pretty h)
  req  <- liftIO $ parseRequest reqs
  httpJSONEither req <&> getResponseBody  <&> either (const Nothing) Just

readRef :: (HasCatAPI m, MonadIO m) => RepoRef -> m (Maybe HashRef)
readRef = readRefHttp


readHashesFromBlock :: (MonadIO m, HasCatAPI m) => HashRef -> m [HashRef]
readHashesFromBlock (HashRef h) = do
  treeQ <- liftIO newTQueueIO
  walkMerkle h (readBlock . HashRef) $ \hr -> do
    case hr of
      Left{} -> pure ()
      Right (hrr :: [HashRef]) -> liftIO $ atomically $ writeTQueue treeQ hrr
  re <- liftIO $ atomically $ flushTQueue treeQ
  pure $ mconcat re

readRefCLI :: MonadIO m => RepoRef -> m (Maybe HashRef)
readRefCLI r = do
  let k = pretty (AsBase58 r)
  trace  [qc|hbs2-peer reflog get {k}|]
  let cmd = setStdin closed $ setStderr closed
                            $ shell [qc|hbs2-peer reflog get {k}|]
  (code, out, _)  <- liftIO $ readProcess cmd

  trace $ viaShow out

  case code of
    ExitFailure{} -> pure Nothing
    _  -> do
      let s = LBS.unpack <$> headMay (LBS.lines out)
      pure $ s >>= fromStringMay

type ObjType = MTreeAnn [HashRef]

readObject :: forall m . (MonadIO m, HasCatAPI m) => HashRef -> m (Maybe ByteString)
readObject h = runMaybeT do

  q <- liftIO newTQueueIO

  -- trace $ "readObject" <+> pretty h

  blk <- MaybeT $ readBlock h

  ann <- MaybeT $ pure $ deserialiseOrFail @(MTreeAnn [HashRef]) blk & either (const Nothing) Just

  walkMerkleTree (_mtaTree ann) (lift . readBlock . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
    case hr of
      Left{} -> mzero
      Right (hrr :: [HashRef]) -> do
        for_ hrr $ \(HashRef hx) -> do

            block <- MaybeT $ readBlock (HashRef hx)
            liftIO $ atomically $ writeTQueue q block

  mconcat <$> liftIO (atomically $ flushTQueue q)


postRefUpdate :: ( MonadIO m
                 , HasRefCredentials m
                 , IsRefPubKey Schema
                 )
              => RepoRef
              -> Integer
              -> HashRef
              -> m ()

postRefUpdate ref seqno hash = do
  trace $ "refPostUpdate"  <+> pretty seqno <+> pretty hash

  cred <- getCredentials ref
  let pubk = view peerSignPk cred
  let privk = view peerSignSk cred
  let tran = SequentialRef seqno (AnnotatedHashRef Nothing hash)
  let bs = serialise tran & LBS.toStrict

  msg <- makeRefLogUpdate @HBS2L4Proto pubk privk bs <&> serialise

  let input = byteStringInput msg
  let cmd = setStdin input $ shell [qc|hbs2-peer reflog send-raw|]

  (code, _, _)  <- liftIO $ readProcess cmd

  trace $ "hbs2-peer exited with code" <+> viaShow code

storeObject :: (MonadIO m, HasCatAPI m, HasConf m)
            => ByteString -> ByteString -> m (Maybe HashRef)
-- storeObject = storeObjectHBS2Store
storeObject = storeObjectHttpPut

storeObjectHttpPut :: (MonadIO m, HasCatAPI m, HasConf m)
                   => ByteString
                   -> ByteString
                   -> m (Maybe HashRef)

storeObjectHttpPut meta bs = do

  let chu = chunks (fromIntegral defBlockSize) bs

  rt <- liftIO $ Cache.newCache Nothing

  -- FIXME: run-concurrently
  hashes  <- forM chu $ \s -> do
               h <- writeBlock s `orDie` "cant write block"
               pure (HashRef h)

  let pt = toPTree (MaxSize 1024) (MaxNum 1024) hashes -- FIXME: settings

  -- trace $ viaShow pt

  root <- makeMerkle 0 pt $ \(h,t,bss) -> do
            liftIO $ Cache.insert rt h (t,bss)
            -- void $ writeBlock bss

  pieces' <- liftIO $  Cache.toList rt
  let pieces = [ bss | (_, (_,bss), _) <- pieces' ]

  api <- getHttpPutAPI

  liftIO $ mapConcurrently (writeBlockIO api) pieces

  mtree <- liftIO $ fst <$> Cache.lookup rt root `orDie` "cant find root block"

  let txt = LBS.unpack meta & Text.pack

  let ann = serialise (MTreeAnn (ShortMetadata txt) NullEncryption  mtree)

  writeBlock ann <&> fmap HashRef

-- FIXME: ASAP-store-calls-hbs2
--   Это может приводить к тому, что если пир и hbs2-peer
--   смотрят на разные каталоги --- ошибки могут быть очень загадочны.
--   Нужно починить.
--
-- FIXME: support-another-apis-for-storage
storeObjectHBS2Store :: (MonadIO m, HasConf m) => ByteString -> ByteString -> m (Maybe HashRef)
storeObjectHBS2Store meta bs = do

  stor <- cfgValue @StoragePref @(Maybe FilePath)

  -- FIXME: fix-temporary-workaround-while-hbs2-is-used
  --  Пока не избавились от hbs2 store для сохранения объектов
  --  можно использовать ключ storage в конфиге hbs2-git
  let pref =  maybe "" (mappend "-p ") stor

  let meta58 = show $ pretty $ B8.unpack $ toBase58 (LBS.toStrict meta)

  -- trace $ "meta58" <+> pretty meta58

  let input = byteStringInput bs
  let cmd = setStdin input $ setStderr closed
                           $ shell [qc|hbs2 store --short-meta-base58={meta58} {pref}|]

  (_, out, _) <- liftIO $ readProcess cmd

  case LBS.words out of
    ["merkle-root:", h] -> pure $ Just $ fromString (LBS.unpack h)
    _                   -> pure Nothing


makeDbPath :: MonadIO m => RepoRef -> m FilePath
makeDbPath h = do
  state <- getAppStateDir
  liftIO $ createDirectoryIfMissing True state
  pure $ state </> show (pretty (AsBase58 h))

loadCredentials :: ( MonadIO m
                   , HasConf m
                   , HasRefCredentials m
                   ) => [FilePath] -> m ()
loadCredentials fp = do
  krOpt' <- cfgValue @KeyRingFiles @(Set FilePath) <&> Set.toList

  let krOpt = List.nub $ fp <> krOpt'

  when (null krOpt) do
    die "keyring not set"

  for_ krOpt $ \fn -> do
    krData <- liftIO $ B8.readFile fn
    cred <- pure (parseCredentials @Schema (AsCredFile krData)) `orDie` "bad keyring file"
    let puk = view peerSignPk cred
    trace $ "got creds for" <+> pretty (AsBase58 puk)
    setCredentials (RefLogKey puk) cred
    pure ()


green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)

section :: Doc ann
section = line <> line

