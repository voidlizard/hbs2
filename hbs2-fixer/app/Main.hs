{-# LANGUAGE TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Schema
import HBS2.Polling
import HBS2.Misc.PrettyStuff
import HBS2.System.Dir
import HBS2.System.Logger.Simple.ANSI hiding (info)
import HBS2.Net.Messaging.Unix


import HBS2.Net.Proto.Service
import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.Peer.CLI.Detect
import HBS2.Peer.Proto.RefLog

import Data.Config.Suckless

import Data.Time.Clock
import Control.Monad.Reader
import Lens.Micro.Platform
import System.Directory
import System.FilePath
import UnliftIO
import Options.Applicative
import Data.Maybe
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Control.Monad.Trans.Cont
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text qualified as Text
import Codec.Serialise
import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}

type Config = [Syntax C]

data family Watcher r

type RLWW = LWWRefKey HBS2Basic
type RRefLog = RefLogKey HBS2Basic

newtype instance Watcher RRefLog =
  WatchRefLog [Syntax C]
  deriving newtype (Semigroup,Monoid)

newtype instance (Watcher (LWWRefKey HBS2Basic)) =
  WatchLWWRef [Syntax C]
  deriving newtype (Semigroup,Monoid)

data FixerEnv = FixerEnv
  { _configFile :: Maybe FilePath
  , _lwwAPI     :: ServiceCaller LWWRefAPI UNIX
  , _refLogAPI  :: ServiceCaller RefLogAPI UNIX
  , _peerAPI    :: ServiceCaller PeerAPI UNIX
  , _config     :: TVar Config
  , _onRefLog   :: TVar ( HashMap RRefLog (NominalDiffTime, Watcher RRefLog) )
  , _onLww      :: TVar ( HashMap RLWW    (NominalDiffTime, Watcher RLWW))
  , _refLogLast :: TVar ( HashMap RRefLog HashRef )
  , _lwwLast    :: TVar ( HashMap RLWW HashRef )
  }

makeLenses ''FixerEnv

newtype FixerM m a = FixerM { runFixerM :: ReaderT FixerEnv m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader FixerEnv, MonadUnliftIO)

instance MonadIO m => HasConf (FixerM m) where
  getConf = asks _config >>= readTVarIO

readConf :: MonadIO m => FilePath -> m [Syntax MegaParsec]
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

  soname <- detectRPC
              `orDie` "can't detect RPC"

  flip runContT pure do

    client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                >>= orThrowUser ("can't connect to" <+> pretty soname)

    void $ ContT $ withAsync $ runMessagingUnix client

    peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
    refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
    -- storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
    lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

    let endpoints = [ Endpoint @UNIX  peerAPI
                    , Endpoint @UNIX  refLogAPI
                    , Endpoint @UNIX  lwwAPI
                    -- , Endpoint @UNIX  storageAPI
                    ]

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client


    env <- FixerEnv Nothing
                    lwwAPI
                    refLogAPI
                    peerAPI
             <$> newTVarIO mempty
             <*> newTVarIO mempty
             <*> newTVarIO mempty
             <*> newTVarIO mempty
             <*> newTVarIO mempty

    lift $ runReaderT (runFixerM $ withConfig cfgPath action) env
      `finally` do
         setLoggingOff @DEBUG
         setLoggingOff @INFO
         setLoggingOff @ERROR
         setLoggingOff @WARN
         setLoggingOff @NOTICE

  where
    debugPrefix  = toStdout . logPrefix "[debug] "
    errorPrefix  = toStdout . logPrefix "[error] "
    warnPrefix   = toStdout . logPrefix "[warn] "
    noticePrefix = toStdout . logPrefix "[notice] "


data ConfWatch =
    ConfWatch
  | ConfRead
  | ConfUpdate [Syntax C]

mainLoop :: FixerM IO ()
mainLoop = forever $ do
  debug "hbs2-fixer. do stuff since 2024"
  conf <- getConf
  debug $ line <> vcat (fmap pretty conf)

  flip runContT pure do

    lift $ updateFromConfig conf

    void $ ContT $ withAsync $ do
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
                pause @'Seconds 30
                next  ConfRead

    -- poll reflogs
    void $ ContT $ withAsync do

      api <- asks _refLogAPI

      olds <- asks _refLogLast

      rlo <- pure $ asks _onRefLog >>= keysToListen

      polling (Polling 1 1) rlo  $ \ref -> do
        debug $ red "POLL REFLOG" <+> pretty ref
        liftIO $ oneSec $ void $ callService @RpcRefLogFetch api (fromRefLogKey ref)

        liftIO (oneSec $ callService @RpcRefLogGet api (fromRefLogKey ref)) >>= \case
          Right (Right (Just v)) -> do
            old <- readTVarIO olds <&> HM.lookup ref
            unless (old == Just v) do
              debug $ green "CHANGED" <+> pretty ref <+> pretty v
              atomically $ modifyTVar olds (HM.insert ref v)

          _ -> pure ()

        pure ()

      pure ()

    -- poll lww
    void $ ContT $ withAsync do

      api  <- asks _lwwAPI
      olds <- asks _lwwLast

      lww <- pure $ asks _onLww >>= keysToListen

      polling (Polling 1 1) lww  $ \ref -> do
        debug $ red "POLL LWWREF" <+> pretty ref
        liftIO $ oneSec $ void $ callService @RpcLWWRefFetch api ref

        liftIO (oneSec $ callService @RpcLWWRefGet api ref) >>= \case
          Right (Right (Just LWWRef{..})) -> do
            old <- readTVarIO olds <&> HM.lookup ref
            unless (old == Just lwwValue) do
              debug $ green "CHANGED" <+> pretty ref <+> pretty lwwValue
              atomically $ modifyTVar olds (HM.insert ref lwwValue)

          _       -> pure ()


    forever $ pause @'Seconds 60

  where
    keysToListen what =
      readTVarIO what
        <&> HM.toList
        <&> \x -> [ (a,b) | (a, (b,_)) <- x ]

oneSec :: MonadUnliftIO m => m b -> m (Either () b)
oneSec = race (pause @'Seconds 1)

updateFromConfig :: MonadIO m => Config -> FixerM m ()
updateFromConfig conf = do

  asks _config >>= atomically . flip writeTVar conf

  let w = [ (def,sec,actions)
          | ListVal ( SymbolVal "watch"
                    : LitIntVal sec
                    : ListVal def
                    : actions ) <- conf
          ]

  rlo     <- asks _onRefLog
  rloLast <- asks _refLogLast
  lww     <- asks _onLww
  lwLast  <- asks _lwwLast

  peerAPI <- asks _peerAPI


  updates <- S.toList_ $ for_ w $ \(who,sec,what) -> do

    case who of
      [SymbolVal rt, LitStrVal r] -> do
        case rt of
          "lwwref" -> do
              let k' = fromStringMay @RLWW (Text.unpack r)
              trace $ red "SET LWWREF WATCHER" <+> pretty sec <+> pretty k' <+> pretty what
              for_ k' $ \k -> do
                liftIO $ void $ oneSec $ callService @RpcPollAdd peerAPI (fromLwwRefKey k, "lwwref", 60 * fromIntegral sec)
                S.yield $ modifyTVar lww (HM.insert k (fromIntegral sec, mempty))
          "reflog" -> do
              let k' = fromStringMay @RRefLog (Text.unpack r)
              trace $ red "SET LWWREF WATCHER" <+> pretty sec <+> pretty k' <+> pretty what
              for_ k' $ \k -> do
                liftIO $ void $ oneSec $ callService @RpcPollAdd peerAPI (fromRefLogKey k, "reflog", 60 * fromIntegral sec)
                S.yield $ modifyTVar rlo (HM.insert k (fromIntegral sec, mempty))
          _ -> pure ()

      x -> debug $ red "WTF?" <+> pretty x

  liftIO $ print $ "W" <+> pretty (length updates)

  atomically do
    writeTVar rlo mempty
    writeTVar lww mempty
    writeTVar rloLast mempty
    writeTVar lwLast mempty
    sequence_ updates

  debug $ vcat (fmap pretty w)

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

