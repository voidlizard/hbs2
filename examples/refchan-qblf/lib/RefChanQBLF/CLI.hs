module RefChanQBLF.CLI where

import HBS2.Actors.Peer
import HBS2.Actors.Peer.Types ()
import HBS2.Base58
import HBS2.Clock
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Defaults
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service
import HBS2.OrDie
import HBS2.Peer.Proto.AnyRef
import HBS2.Peer.Proto.RefChan
import HBS2.Prelude
import HBS2.Storage.Simple
import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Arrow hiding ((<+>))
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Config.Suckless
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions (cs)
import Lens.Micro.Platform hiding ((.=))
import Options.Applicative hiding (info)
import Options.Applicative qualified as O
import System.Directory
import System.Exit qualified as Exit
import UnliftIO

import RefChanQBLF.App
import RefChanQBLF.Common
import RefChanQBLF.Impl
import RefChanQBLF.RPCServer
import RefChanQBLF.Transactions


type Config = [Syntax C]

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  O.info (helper <*> globalOptions)
  (  fullDesc
  <> header "refchan-qblf-worker"
  <> progDesc "for test and demo purposed"
  )
  where

    globalOptions = applyConfig <$> commonOpts <*> cli

    applyConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
    applyConfig config m = do
      maybe1 config (m mempty) $ \conf -> do
        top <- readFile conf <&> parseTop <&> either (pure mempty) id
        m top

    commonOpts = optional $ strOption (long "config" <> short 'c' <> help "Config file")

    cli = hsubparser (  command "run" (O.info pRun (progDesc "run qblf servant" ) )
                     <> command "gen" (O.info pGen (progDesc "generate transcation") )
                     <> command "post" (O.info pPostTx (progDesc "post transaction") )
                     <> command "check" (O.info pCheckTx (progDesc "check transaction") )
                     <> command "balances" (O.info pBalances (progDesc "show balances") )
                     )

    pGen = hsubparser
           (   command "tx-emit" ( O.info pGenEmit (progDesc "generate emit") )
            <> command "tx-move" ( O.info pGenMove (progDesc "generate move") )
           )

    pGenEmit = do
      kr    <- strOption   ( long "keyring"  <> short 'k' <> help "keyring file" )
      amnt <- option @Amount auto ( long "amount" <> short 'n' <> help "amount" )
      dest <- strArgument ( metavar "ADDRESS" )
      pure $ const $ silently do
        sc <- BS.readFile kr
        creds <- pure (parseCredentials @'HBS2Basic (AsCredFile sc)) `orDie` "bad keyring file"
        let pk = view peerSignPk creds
        let sk = view peerSignSk creds
        acc <- pure (fromStringMay @(RefChanId L4Proto) dest) `orDie` "bad account address"
        tx <- makeEmitDemoToken @_ @L4Proto pk sk acc amnt
        LBS.putStr $ serialise tx

    pGenMove = do
      kr    <- strOption   ( long "wallet"  <> short 'w' <> help "wallet (keyring) file" )
      amnt <- option @Amount auto ( long "amount" <> short 'n' <> help "amount" )
      dest <- strArgument ( metavar "ADDRESS" )
      pure $ const $ silently do
        sc <- BS.readFile kr
        creds <- pure (parseCredentials @'HBS2Basic (AsCredFile sc)) `orDie` "bad keyring file"
        let pk = view peerSignPk creds
        let sk = view peerSignSk creds
        acc <- pure (fromStringMay @(RefChanId L4Proto) dest) `orDie` "bad account address"
        tx <- makeMoveDemoToken @_ @L4Proto pk sk acc amnt
        LBS.putStr $ serialise tx

    pCheckTx = do
      pure $ const do
        tx <- either (Exit.die . ("QBLFDemoToken deserialise error: " <>) . show) pure
              . deserialiseOrFail @(QBLFDemoToken 'HBS2Basic)
            =<< LBS.getContents

        case tx of
          Emit box ->
            BS8.hPutStrLn stderr . cs . show . pretty . first AsBase58
                =<< pure (unboxSignedBox0 box) `orDie` "bad emit tx"

          Move box ->
            BS8.hPutStrLn stderr . cs . show . pretty . first AsBase58
                =<< pure (unboxSignedBox0 box) `orDie` "bad move tx"

        pure ()

pBalances :: Parser (Config -> IO ())
pBalances = do
    mstateref <- optional do
        option (fromStringP @HashRef "qblf state hash")
            (long "state-hash" <> metavar "HASH-REF")
    pure \syn -> withLogging do
        bal <- flip runContT pure do
            sto <- ContT $ withSimpleAnyStorage defStorePath
            lift do
                stateHashRef :: HashRef
                    <- mstateref & flip maybe pure do
                      either Exit.die pure =<< runExceptT do
                        stref <- (flip runReader syn $ cfgValue @StateRefOpt @(Maybe String))
                            & orE "state-ref key not found in config"
                            <&> fromStringMay
                            & orEM "state-ref key parse error"
                        HashRef <$> do
                          liftIO (readStateHashMay sto stref)
                            & orEM "State is not created yed"
                flip runReaderT sto $ do
                    debug $ "calculating balances for" <+> pretty stateHashRef
                    balances stateHashRef

        forM_ (HashMap.toList bal) $ \(acc, qty) -> do
            liftIO $ print $ pretty (AsBase58 acc) <+> pretty qty

fromStringP :: (FromStringMaybe a) => String -> ReadM a
fromStringP msg = eitherReader $
      maybe (Left ("Can not parse " <> msg)) Right . fromStringMay . cs

refchanP :: ReadM (RefChanId L4Proto)
refchanP = fromStringP "refchan id"

pPostTx :: Parser (Config -> IO ())
pPostTx = do
    pure \syn -> withLogging do
        debug $ "runQBLFApp" <+> pretty syn

        appsopath <- maybe (Exit.die "app-socket path not found in config") pure do
            flip runReader syn do
                cfgValue @AppSocketOpt @(Maybe String)

        tx <- either (Exit.die . ("QBLFDemoToken deserialise error: " <>) . show) pure
              . deserialiseOrFail @(QBLFDemoToken 'HBS2Basic)
            =<< LBS.getContents

        messagingUnix :: MessagingUnix <- newMessagingUnix False 1.0 appsopath
        ep <- makeServiceCaller @QBLFAppRPC @UNIX (msgUnixSelf messagingUnix)
        flip runContT pure do
          contAsync $ runMessagingUnix messagingUnix
          contAsync $ runReaderT (runServiceClient ep) messagingUnix
          lift do

            maybe (Exit.die "RPC server is not available") pure
                =<< callRpcWaitMay @PingRPC (TimeoutSec 0.42) ep ()

            r :: Text
                <- callRpcWaitMay @PostTxRPC (TimeoutSec 3) ep tx
                & peelMWith Exit.die do
                      orE "RPC server timeout" >>> leftEM show >>> leftEM show

            LBS.putStr . cs $ r

pRun :: Parser (Config -> IO ())
pRun = pure \conf -> withLogging do
    debug $ "runQBLFApp" <+> pretty conf
    runQBLFApp =<< (either Exit.die pure . parseQBLFAppConf) conf

parseQBLFAppConf :: Config -> Either String QBLFAppConf
parseQBLFAppConf = runReaderT do
    qapActorKeyring <- cfgValue @ActorOpt @(Maybe String)
        & orEM "actor's key not set"

    qapRefchanID <- cfgValue @RefChanOpt @(Maybe String)
        & orEM "refchan not set"
        <&> fromStringMay @(RefChanId L4Proto)
        & orEM "invalid REFCHAN value in config"

    qapSocket <- cfgValue @SocketOpt @(Maybe String)
        & orEM "socket not set"

    qapAppSocket <- cfgValue @AppSocketOpt @(Maybe String)
        & orEM "app socket not set"

    qapDefState <- cfgValue @DefStateOpt @(Maybe String)
        <&> (>>= fromStringMay)

    qapStateRef <- cfgValue @StateRefOpt @(Maybe String)
        & orEM "state-ref key not found in config"
        <&> fromStringMay
        & orEM "state-ref key parse error"

    pure QBLFAppConf {..}
