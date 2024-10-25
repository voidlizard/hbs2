{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Polling
import HBS2.Misc.PrettyStuff
import HBS2.System.Dir
import HBS2.System.Logger.Simple.ANSI hiding (info)
import HBS2.Net.Messaging.Unix

import HBS2.Git.Data.LWWBlock

import HBS2.Net.Proto.Notify
import HBS2.Net.Proto.Service
import HBS2.Peer.Notify
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.Peer.CLI.Detect
import HBS2.Peer.Proto.RefLog

import Data.Config.Suckless

import Data.Time.Clock
import Data.Coerce
import Control.Monad.Reader
import Lens.Micro.Platform
import System.Directory
import Options.Applicative
import Data.Maybe
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as Text
import Data.Hashable
import Control.Exception qualified as E
import System.Process.Typed
import System.Environment qualified as Env
import System.Exit qualified as Exit
import Data.Cache qualified as Cache
import Data.Cache (Cache)

import System.Exit

{- HLINT ignore "Functor law" -}


type Config = [Syntax C]


type RLWW = LWWRefKey 'HBS2Basic
type RRefLog = RefLogKey 'HBS2Basic

newtype Watcher =
  Watcher [Syntax C]
  deriving newtype (Semigroup,Monoid)

data Ref =
    RefRefLog RRefLog
  | RefLWW    RLWW
  deriving stock (Eq,Generic)

instance Pretty Ref where
  pretty (RefRefLog r) = parens $ "reflog" <+> dquotes (pretty r)
  pretty (RefLWW r)    = parens $ "lwwref" <+> dquotes (pretty r)

newtype AnyPolledRef =
  AnyPolledRef (PubKey 'Sign 'HBS2Basic)
  deriving (Eq,Generic)

instance Hashable AnyPolledRef

-- FIXME: move-to-suckless-conf
deriving newtype instance Hashable Id

instance Pretty AnyPolledRef where
  pretty (AnyPolledRef r) = pretty (AsBase58 r)
-- deriving newtype instance Pretty (PubKey 'Sign 'HBS2Basic) => Pretty AnyPolledRef

instance FromStringMaybe AnyPolledRef where
  fromStringMay = fmap AnyPolledRef . fromStringMay

newtype PolledRef =
  PolledRef (Ref, NominalDiffTime)
  deriving stock (Eq,Generic)
  deriving newtype (Pretty)

instance Hashable Ref

instance Hashable PolledRef where
  hashWithSalt salt (PolledRef (r,_)) = hashWithSalt salt r

data FixerEnv = FixerEnv
  { _configFile :: Maybe FilePath
  , _lwwAPI     :: ServiceCaller LWWRefAPI UNIX
  , _refLogAPI  :: ServiceCaller RefLogAPI UNIX
  , _refLogSink :: NotifySink (RefLogEvents L4Proto) UNIX
  , _peerAPI    :: ServiceCaller PeerAPI UNIX
  , _sto        :: AnyStorage
  , _config     :: TVar Config
  , _configPoll :: TVar Int
  , _watchers   :: TVar (HashMap PolledRef Watcher)
  , _listeners  :: TVar (HashMap RRefLog (Async ()))
  , _result     :: TVar (HashMap Ref (Maybe HashRef, Maybe HashRef))
  , _runNum     :: TVar Int
  , _locals     :: TVar (HashMap Id (Syntax C))
  , _pipeline   :: TQueue (IO ())
  }

makeLenses ''FixerEnv


newtype FixerM m a = FixerM { runFixerM :: ReaderT FixerEnv m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader FixerEnv, MonadUnliftIO)

instance MonadIO m => HasConf (FixerM m) where
  getConf = asks _config >>= readTVarIO


debugPrefix  = toStdout . logPrefix "[debug] "

readConf :: MonadIO m => FilePath -> m [Syntax C]
readConf  fn = liftIO (readFile fn) <&> parseTop <&> fromRight mempty

withConfig :: MonadUnliftIO m => Maybe FilePath -> FixerM m () -> FixerM m ()
withConfig cfgPath m = do
  defConfDir <- liftIO $ getXdgDirectory XdgConfig "hbs2-fixer"

  let configPath = fromMaybe (defConfDir </> "config") cfgPath
  unless (isJust cfgPath) do
    debug $ pretty configPath
    touch configPath

  syn <- readConf configPath
  tsyn <- newTVarIO syn

  local (set config tsyn . set configFile (Just configPath)) (void m)

withApp :: Maybe FilePath -> FixerM IO () -> IO ()
withApp cfgPath action = do
  setLogging @DEBUG debugPrefix
  setLogging @INFO  defLog
  setLogging @ERROR errorPrefix
  setLogging @WARN  warnPrefix
  setLogging @NOTICE noticePrefix

  fix \next -> do

    flip runContT pure do

      soname' <- lift detectRPC

      soname <- ContT $ maybe1 soname' (pure ())

      client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                  >>= orThrowUser ("can't connect to" <+> pretty soname)

      mess <-  ContT $ withAsync $ runMessagingUnix client

      peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
      refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
      storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
      lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

      let endpoints = [ Endpoint @UNIX  peerAPI
                      , Endpoint @UNIX  refLogAPI
                      , Endpoint @UNIX  lwwAPI
                      , Endpoint @UNIX  storageAPI
                      ]

      mn <- ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

      let o = [MUDontRetry]
      clientN <- newMessagingUnixOpts o False 1.0 soname

      notif <- ContT $ withAsync (runMessagingUnix clientN)


      sink <- newNotifySink

      void $ ContT $ withAsync $ flip runReaderT clientN $ do
                debug $ red "notify restarted!"
                runNotifyWorkerClient sink

      p1 <- ContT $ withAsync $ flip runReaderT clientN $ do
          runProto @UNIX
            [ makeResponse (makeNotifyClient @(RefLogEvents L4Proto) sink)
            ]

      env <- FixerEnv Nothing
                      lwwAPI
                      refLogAPI
                      sink
                      peerAPI
                      (AnyStorage (StorageClient storageAPI))
               <$> newTVarIO mempty
               <*> newTVarIO 30
               <*> newTVarIO mempty
               <*> newTVarIO mempty
               <*> newTVarIO mempty
               <*> newTVarIO 0
               <*> newTVarIO mempty
               <*> newTQueueIO

      void $ ContT $ bracket (pure ()) $ \_ -> do
               readTVarIO (_listeners env) <&> HM.elems >>= mapM_ cancel

      p3 <- ContT $ withAsync $ runReaderT (runFixerM $ withConfig cfgPath action) env

      void $ waitAnyCatchCancel [mess,mn,notif,p1,p3]

    debug $ red "respawning..."
    pause @'Seconds 5
    next

  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

  where
    errorPrefix  = toStdout . logPrefix "[error] "
    warnPrefix   = toStdout . logPrefix "[warn] "
    noticePrefix = toStdout


data ConfWatch =
    ConfWatch
  | ConfRead
  | ConfUpdate [Syntax C]

mainLoop :: FixerM IO ()
mainLoop = do
  debug "hbs2-fixer. do stuff since 2024"
  conf <- getConf
  -- debug $ line <> vcat (fmap pretty conf)

  flip runContT pure do

    debug $ red "Reloading..."

    lift $ updateFromConfig conf

    p1 <- ContT $ withAsync $ do
            cfg <- asks _configFile `orDie` "config file not specified"

            flip fix ConfRead $ \next -> \case
              ConfRead -> do
                debug $ yellow "read config" <+> pretty cfg
                newConf <- readConf cfg
                oldConf <- getConf

                let a = hashObject @HbSync (LBS.pack $ show $ pretty newConf)
                let b = hashObject @HbSync (LBS.pack $ show $ pretty oldConf)

                let changed = a /= b

                if not changed then
                  next ConfWatch
                else
                  next (ConfUpdate newConf)

              ConfUpdate new -> do
                debug $ yellow "read config / update state"
                updateFromConfig new
                next ConfWatch

              ConfWatch{} -> do
                w <- asks _configPoll >>= readTVarIO
                pause (TimeoutSec (realToFrac w))
                next  ConfRead

    -- poll reflogs
    p2 <- ContT $ withAsync do

      let w = asks _watchers
               >>= readTVarIO
               <&> HM.toList
               <&> \wtf -> [ (ByFirst r wa, t) | (PolledRef (r,t), wa) <- wtf ]

      polling (Polling 1 1) w $ \case
        ByFirst ref wa -> do
          new <- getRefRpc ref
          re <- asks _result
          old <- readTVarIO re
                   <&> (snd <=< HM.lookup ref)

          when (new /= old) do
            atomically $ modifyTVar re (HM.insert ref (old, new))
            -- bindId
            forM_ new (runWatcher wa ref)

      pure ()

    jobs <- asks _pipeline
    p3 <-  ContT $ withAsync $ fix \next -> do
      r <- liftIO $ E.try @SomeException (join $ atomically $ readTQueue jobs)
      case r of
        Left e -> do
          err (viaShow e)
          let ee = fromException @AsyncCancelled e

          unless (isJust ee) do
            next

        _ -> next

    void $ waitAnyCatchCancel [p1,p2,p3]

oneSec :: MonadUnliftIO m => m b -> m (Either () b)
oneSec = race (pause @'Seconds 1)


fromStrLitMay :: forall s c . FromStringMaybe s => Syntax c -> Maybe s
fromStrLitMay = \case
  LitStrVal s -> fromStringMay (Text.unpack s)
  _ -> Nothing

pattern PTop :: forall {c}. Id -> [Syntax c] -> Syntax c
pattern PTop ctor rest <- ListVal (SymbolVal ctor : rest)

pattern PPolledRef :: forall {c}. Id -> AnyPolledRef  -> Syntax c
pattern PPolledRef t r <- ListVal [ SymbolVal t, fromStrLitMay @AnyPolledRef -> Just r ]

pattern PWatchRef :: forall {c}. Integer -> Id -> AnyPolledRef -> [Syntax c] -> [Syntax c]
pattern PWatchRef n t r w    <- (LitIntVal n : PPolledRef t r : w)

pattern PListenRef :: forall {c}. Id -> AnyPolledRef -> [Syntax c] -> [Syntax c]
pattern PListenRef t r w <- (PPolledRef t r : w)

-- pattern PDisplay :: Syntax c
pattern PDisplay :: forall {c}. Syntax c -> Syntax c
pattern PDisplay w  <- ListVal [ SymbolVal "display", w ]

pattern PApply :: Id -> [Syntax C] -> Syntax C
pattern PApply f a <- ListVal ( SymbolVal f : a )

fetchRef :: forall m . MonadIO m => Ref -> FixerM m ()
fetchRef r = do
    case r of
      RefRefLog ref -> do
        api <- asks _refLogAPI
        void $ liftIO $ oneSec $ void $ callService @RpcRefLogFetch api (fromRefLogKey ref)
      RefLWW ref -> do
        api <- asks _lwwAPI
        void $ liftIO $ oneSec $ void $ callService @RpcLWWRefFetch api ref


getRefRpc :: forall m . MonadIO m => Ref -> FixerM m (Maybe HashRef)
getRefRpc r = do
    case r of
      RefRefLog ref -> do
        api <- asks _refLogAPI
        liftIO (oneSec $ callService @RpcRefLogGet api (fromRefLogKey ref))
          >>= \case
                Right (Right x) -> pure x
                _               -> pure Nothing

      RefLWW ref -> do
        api <- asks _lwwAPI
        liftIO (oneSec $ callService @RpcLWWRefGet api ref) >>= \case
          Right (Right x) -> pure (lwwValue <$> x)
          _               -> pure Nothing

subscribeRef :: forall m . MonadIO m => Integer -> Ref -> FixerM m ()
subscribeRef n r = do
  debug $ "subscribeRef" <+> pretty n <+> pretty r
  let (puk,t) = case r of
                 RefRefLog k ->  (coerce k, "reflog")
                 RefLWW    k  -> (coerce k, "lwwref")

  let minutes = fromIntegral $ max 1 (n `div` 60)

  api <- asks _peerAPI
  void $ liftIO $ oneSec $ callService @RpcPollAdd api (puk, t, minutes)

asRef :: Id -> AnyPolledRef -> Maybe Ref
asRef t r = case t of
  "lwwref" -> Just $ RefLWW (coerce r)
  "reflog" -> Just $ RefRefLog (coerce r)
  _        -> Nothing


runWatcher :: forall m . MonadUnliftIO m => Watcher -> Ref -> HashRef -> FixerM m ()
runWatcher (Watcher code)  ref new = do
  debug $ yellow "CHANGED" <+> pretty ref <+> pretty new

  sto <- asks _sto

  newCode <- flip transformBiM code $ \case
               PApply "lwwref:get-hbs2-git-reflog" _ -> do
                 v <- case ref of
                        RefLWW k -> readLWWBlock sto k
                        _ -> pure Nothing

                 -- FIXME: wrappers-for-syntax-ctors
                 let vv = maybe1 v (List (noContext @C) mempty) $
                           \(_, LWWBlockData{..}) ->
                             List (noContext @C) [ Symbol (noContext @C) "reflog"
                                                 , Literal (noContext @C)
                                                           (mkLit @Text (fromString $ show $ pretty (AsBase58 lwwRefLogPubKey)))
                                                 ]
                 pure vv

               w -> pure w

  debug (pretty newCode)
  runConfig newCode



display :: forall m . MonadUnliftIO m => Syntax C -> FixerM m ()
display what = do
  case what of
    LitStrVal s -> notice (pretty s)
    ast         -> notice (pretty ast)

nil :: Syntax C
nil = List (noContext @C) []

list_ :: [Syntax C] -> Syntax C
list_ = List (noContext @C)

symbol_ :: Id -> Syntax C
symbol_ = Symbol (noContext @C)

str_ :: Text -> Syntax C
str_ s = Literal (noContext @C) (LitStr s)

int_ :: Integer -> Syntax C
int_ s = Literal (noContext @C) (LitInt s)

bool_ :: Bool -> Syntax C
bool_ s = Literal (noContext @C) (LitBool s)

-- FIXME: to-suckless-conf
class AsString s where
  asString :: s -> String

instance AsString Literal where
  asString (LitStr s) = Text.unpack s
  asString other = show $ pretty other

instance AsString (Syntax c) where
  asString (Literal _ x) = asString x
  asString x = show $ pretty x

data RunOpts =
  RunCWD FilePath

instance Pretty RunOpts where
  pretty = \case
    RunCWD f -> parens ("cwd" <+> pretty f)

eval :: forall m . MonadUnliftIO m => Syntax C -> FixerM m (Syntax C)
eval = eval'
  -- debug $ "EVAL" <+> pretty syn <+> pretty r
  -- pure r

eval' :: forall m . MonadUnliftIO m => Syntax C -> FixerM m (Syntax C)
eval' syn = do

  case syn  of

    x@(Literal{}) -> pure x

    (SymbolVal n) -> lookupLocal n

    w@(PApply "list" code') -> do
      code <- mapM unquote code'
      pure $ list_ (symbol_ "list" : code)

    PApply "local" [SymbolVal n, what] -> do
      bindLocal n =<< eval what
      pure nil

    PApply "eval" [e] -> do
      eval e >>= \case
        (ListVal ( SymbolVal "list" : es ) ) -> do
           lastDef nil <$> mapM eval es

        _ -> pure nil

    PApply "listen" (what' : code) -> do
      what <- eval  what'
      case what of
        PPolledRef "reflog" ref -> do
          setReflogListener (coerce ref) =<< mapM unquote code

        PPolledRef tp r -> do
          warn $ yellow "not supported listener type" <+> pretty tp

        _ -> pure ()

      pure nil

    PApply "watch" (p' : what' : watcher') -> do
      p <- eval p'
      what <- eval what'
      watcher <- mapM unquote watcher'

      case (p, what) of
        (LitIntVal n, PPolledRef tp ref) -> do

          let re = asRef tp ref

          forM_ re (subscribeRef n)
          void $ async (pause @'Seconds 5 >> forM_ re  fetchRef)

          void $ runMaybeT do

            -- FIXME: more-diagnostics
            pref <- toMPlus $ case tp of
                        "lwwref" -> Just $ PolledRef (RefLWW (coerce ref), fromIntegral n)
                        "reflog" -> Just $ PolledRef (RefRefLog (coerce ref), fromIntegral n)
                        _        -> Nothing

            debug $ blue "watch" <+> pretty n <+> pretty tp <+> pretty ref
            w <- asks _watchers
            atomically $ modifyTVar w (HM.insert pref (Watcher watcher))

        _ -> pure ()

      pure nil

    PApply "on-start" wtf -> do

      rn <- asks _runNum
      rnn <- atomically do
              x <- readTVar rn
              modifyTVar rn succ
              pure x

      when (rnn == 0) do
        mapM_ eval wtf

      pure nil

    PApply fn args' -> do
      args <- mapM eval args'
      case fn of

        "reflog" -> do
          pure $ list_ (symbol_ "reflog" : args)

        "lwwref" -> do
          pure $ list_ (symbol_ "lwwref" : args)

        "watch-config" -> do
          case headDef (int_ 30) args of
            LitIntVal n -> do
              debug $ "watch-config" <+> pretty n
              asks _configPoll >>= atomically . flip writeTVar (fromIntegral n)
            _ -> do
              pure ()

          pure nil

        "debug" -> do
          let onOff = headDef (bool_ False) args
          case onOff of
            LitBoolVal True -> do
              setLogging @DEBUG debugPrefix
            _ -> do
              setLoggingOff @DEBUG

          pure nil

        "string-append" -> do
          pieces <- for args $ \case
              LitStrVal s -> pure s
              other       -> pure (Text.pack $ show $ pretty other)

          pure $ str_ $ mconcat pieces

        "display" -> do
          first <- headDef nil <$> mapM eval args
          case first of
            LitStrVal s -> notice (pretty s)
            ast         -> notice (pretty ast)

          pure nil

        "getenv" -> do
          let name = asString $ headDef nil args
          liftIO $ Env.lookupEnv name
           >>= \case
                 Nothing -> pure nil
                 Just s  -> pure $ str_ (fromString s)

        "mkdir" -> do
          debug $ "mkdir" <+> pretty args
          mapM_ mkdir [ Text.unpack s | (LitStrVal s) <- args ]
          pure nil

        "exit" -> do
           case headDef (int_ 0) args of
            LitIntVal 0 -> liftIO Exit.exitSuccess
            LitIntVal w -> liftIO $ Exit.exitWith (ExitFailure $ fromIntegral w)
            _           -> liftIO Exit.exitFailure

           pure nil

        "run" -> do
          debug $ red "RUN-ARGS" <+> pretty args
          (o,cargs) <- case args of
                      (ListVal (SymbolVal "list" : SymbolVal "opts" : opts) : rest)  -> do
                        let pairs = [ (opt, e) | ListVal [SymbolVal opt, e] <- opts ]
                        oo <- for pairs $ \(o, e) -> (o,) <$> eval e
                        let cwd = lastMay [ RunCWD (Text.unpack f )
                                          | ("cwd", LitStrVal f) <- oo
                                          ]
                        pure (maybeToList cwd, rest)

                      rest -> do
                        pure (mempty, rest)

          let what = unwords $ [Text.unpack s | LitStrVal s <- cargs]

          let cwd = case headMay [ p | c@(RunCWD p) <- o ] of
                      Just c -> setWorkingDir c
                      _      -> id

          debug $ red "RUN" <+> pretty what <+> pretty o

          let job = void $ runProcess_ (shell what & cwd)
          pip  <- asks _pipeline
          atomically $ writeTQueue pip job

          pure nil

        _ -> pure nil


    _ -> pure nil


unquote :: forall code m . (code ~ Syntax C, MonadUnliftIO m) => code -> FixerM m code
unquote code = flip transformBiM code $ \case
  x@(ListVal [SymbolVal "unquoted", rest] :: Syntax C) -> do
    eval rest

  x -> pure x

setReflogListener :: forall m . MonadUnliftIO m => RRefLog -> [Syntax C] -> FixerM m ()
setReflogListener reflog code = do
  debug $ green "setReflogListener" <+> pretty reflog <> line <> pretty code

  dudes <- asks _listeners

  a <- atomically do
         x <- readTVar dudes <&> HM.lookup reflog
         modifyTVar dudes (HM.delete reflog)
         pure x

  maybe1 a none cancel

  sink <- asks _refLogSink

  debug $ "subscribe to" <+> pretty reflog

  new <- async do
          cache <- liftIO $ Cache.newCache (Just (toTimeSpec (TimeoutSec 10)))

          runNotifySink sink (RefLogNotifyKey reflog) $ \(RefLogUpdateNotifyData _ h) -> do
            debug $ "Got notification" <+> pretty reflog <+> pretty h
            here <- liftIO (Cache.lookup cache (reflog, h)) <&> isJust
            unless here do
              liftIO $ Cache.insert cache (reflog,h) ()
              runConfig code

  atomically $ modifyTVar dudes (HM.insert reflog new)

bindLocal :: forall m . MonadUnliftIO m => Id -> Syntax C -> FixerM m ()
bindLocal l e = do
  -- debug $ "bindLocal" <+> pretty l
  asks _locals >>= atomically . flip modifyTVar (HM.insert l e)

lookupLocal :: forall m . MonadUnliftIO m => Id ->FixerM m (Syntax C)
lookupLocal name = do
  -- debug $ "lookupLocal" <+> pretty name
  asks _locals >>= readTVarIO <&> fromMaybe nil . HM.lookup name

runConfig :: forall m . MonadUnliftIO m => Config -> FixerM m ()
runConfig conf = do
  debug $ green "runConfig"
  bindLocal "off" (bool_ False)
  bindLocal "on"  (bool_ True)

  mapM_ eval conf

updateFromConfig :: MonadUnliftIO m => Config -> FixerM m ()
updateFromConfig conf = do
  asks _config >>= atomically . flip writeTVar conf
  runConfig conf

main :: IO ()
main = do
  runMe =<< customExecParser (prefs showHelpOnError)
              ( info (helper <*> opts)
                ( fullDesc
                <> header "hbs2-fixer"
                <> progDesc "Intermediary between hbs2-peer and external applications. Listen events / do stuff"
                ))

  where
    opts = optional $ strOption (short 'c' <> long "config" <> metavar "FILE" <> help "Specify configuration file")

    runMe opt = withApp opt mainLoop

