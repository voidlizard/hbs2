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

import HBS2.Peer.Proto.LWWRef
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
  WatchRefLog ( RRefLog -> [Syntax C] -> IO () )

newtype instance (Watcher (LWWRefKey HBS2Basic)) =
  WatchLWWRef ( LWWRefKey HBS2Basic -> [Syntax C] -> IO () )

data FixerEnv = FixerEnv
  { _configFile :: Maybe FilePath
  , _config     :: TVar Config
  , _onRefLog   :: TVar ( HashMap RRefLog (NominalDiffTime, [Watcher RRefLog]) )
  , _onLww      :: TVar ( HashMap RLWW    (NominalDiffTime, [Watcher RLWW]))
  , _refLogLast :: TVar ( HashMap RRefLog HashRef )
  , _lwwLast    :: TVar ( HashMap RRefLog HashRef )
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

  env <- FixerEnv Nothing
           <$> newTVarIO mempty
           <*> newTVarIO mempty
           <*> newTVarIO mempty
           <*> newTVarIO mempty
           <*> newTVarIO mempty

  runReaderT (runFixerM $ withConfig cfgPath action) env
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
                pause @'Seconds 10
                next  ConfRead

    -- poll reflogs
    void $ ContT $ withAsync do

      rlo <- pure $ asks _onRefLog
               >>= readTVarIO
               <&> HM.toList
               <&> \x -> [ (a,b) | (a, (b,_)) <- x ]

      polling (Polling 1 1) rlo  $ \ref -> do
        debug $ red "POLL REFLOG" <+> pretty ref
        pure ()

      pure ()

    -- poll lww
    void $ ContT $ withAsync do

      lww <- pure $ asks _onLww
               >>= readTVarIO
               <&> HM.toList
               <&> \x -> [ (a,b) | (a, (b,_)) <- x ]

      polling (Polling 1 1) lww  $ \ref -> do
        debug $ red "POLL LWWREF" <+> pretty ref
        pure ()

    forever $ pause @'Seconds 60


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

  updates <- S.toList_ $ for_ w $ \(who,sec,what) -> do

    case who of
      [SymbolVal rt, LitStrVal r] -> do
        case rt of
          "lwwref" -> do
              let k' = fromStringMay @RLWW (Text.unpack r)
              debug $ red $ "SET LWWREF WATCHER" <+> pretty sec <+> pretty k' <+> pretty what
              for_ k' $ \k -> do
                S.yield $ modifyTVar lww (HM.insert k (fromIntegral sec, mempty))
          "reflog" -> do
              let k' = fromStringMay @RRefLog (Text.unpack r)
              debug $ red $ "SET LWWREF WATCHER" <+> pretty sec <+> pretty k' <+> pretty what
              for_ k' $ \k -> do
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

