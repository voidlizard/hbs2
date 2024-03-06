module Main where

import HBS2.Prelude
import HBS2.Base58
import HBS2.OrDie
import HBS2.Net.Proto.Types
import HBS2.Actors.Peer
import HBS2.Peer.Proto
import HBS2.Net.Messaging.Unix
import HBS2.Net.Auth.Credentials()

import HBS2.System.Logger.Simple

import Control.Monad.Reader
import Data.List qualified as List
import Options.Applicative hiding (info)
import Options.Applicative qualified as O
import System.Directory
import UnliftIO

tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix "[notice] "


data Verdict = DoAccept | DoReject
              deriving (Eq,Ord,Show)

instance Pretty Verdict where
  pretty = viaShow

withLogging :: MonadIO m => m a -> m ()
withLogging m = do
  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix

  m

  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

data MyEnv =
  MyEnv
  { mySelf :: Peer UNIX
  , myFab  :: Fabriq UNIX
  , myChan :: RefChanId UNIX
  }


newtype App m a = App { fromApp :: ReaderT MyEnv m a }
                  deriving newtype ( Functor
                                   , Applicative
                                   , Monad
                                   , MonadIO
                                   , MonadReader MyEnv
                                   , MonadTrans
                                   )

runApp :: (MonadIO m, PeerMessaging UNIX) => MyEnv -> App m a -> m a
runApp env m = runReaderT (fromApp  m) env

instance Monad m => HasFabriq UNIX (App m) where
  getFabriq = asks myFab

instance Monad m => HasOwnPeer UNIX (App m) where
  ownPeer = asks mySelf


runMe :: String -> FilePath -> Verdict -> IO ()
runMe chan' sa verdict = withLogging do
  chan  <- pure (fromStringMay @(RefChanId UNIX) chan') `orDie` "invalid REFCHAN"

  info $ "I'm dummy refchan validator" <+> pretty (AsBase58 chan) <+> pretty sa <+> pretty verdict

  here <- doesFileExist sa

  when here do
    removeFile sa

  server <- newMessagingUnix True 1.0 sa

  abus <- async $ runMessagingUnix server

  let env = MyEnv (fromString sa) (Fabriq server) chan

  runApp env do
    debug "BOO"
    runProto $ List.singleton $ makeResponse (myProto chan)

  void $ waitAnyCatchCancel [abus]
  err "WTF?"

  where

    myProto :: forall e m . ( MonadIO m
                            , Request e (RefChanValidate e) m
                            , Response e (RefChanValidate e) m
                            , e ~ UNIX
                            )
                       => RefChanId e
                       -> RefChanValidate e
                       -> m ()

    myProto chan msg = do
      case rcvData msg of
        Poke{} -> debug "poked"
        Validate href -> do
          debug $ "validate request" <+> pretty (AsBase58 (rcvChan msg)) <+> pretty href

          case verdict of
            DoAccept -> do
              debug $ "sending accept for" <+> brackets (pretty (AsBase58 (rcvNonce msg))) <+> pretty href
              response (RefChanValidate (rcvNonce msg) chan (Accepted @UNIX href))

            DoReject -> do
              debug $ "sending reject for" <+> brackets (pretty (AsBase58 (rcvNonce msg))) <+> pretty href
              response (RefChanValidate (rcvNonce msg) chan (Rejected @UNIX href))

        _ -> pure ()


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  O.info (helper <*> parser)
  (  fullDesc
  <> header "refchan-dummy-validator"
  <> progDesc "for test and demo purposed"
  )
  where
    parser ::  Parser (IO ())
    parser = do
      rchan   <- strArgument ( metavar "REFCHAN" ) <&> fromString
      soname  <- strArgument ( metavar "UNIX-SOCKET" )

      verdict <- accept <|> reject <|> pure DoAccept

      pure $ runMe rchan soname verdict

    accept = do
      void $ flag' True ( long "accept" <> short 'y' )
      pure DoAccept

    reject = do
      void $ flag' True ( long "reject" <> short 'n' )
      pure DoReject



