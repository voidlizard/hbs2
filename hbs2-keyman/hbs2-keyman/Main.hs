module Main where

import HBS2.KeyMan.Prelude
import HBS2.KeyMan.App.Types
import HBS2.KeyMan.Config
import HBS2.KeyMan.State

import HBS2.Net.Auth.Credentials

import HBS2.Data.KeyRing qualified as KeyRing

import HBS2.System.Dir

import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.Client.RefChan
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient

import Data.Config.Suckless.KeyValue
import Data.Config.Suckless

import Data.List qualified as List
import Options.Applicative qualified as O
import Data.Text qualified as Text
import Options.Applicative hiding (info,action)
import Data.Set qualified as Set
import Data.HashSet qualified as HS
import Data.ByteString qualified as BS
import Data.ByteString qualified as LBS
import Data.Maybe
import Data.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Except
import Codec.Serialise
import Data.Coerce

import Streaming.Prelude qualified as S

data GlobalOptions = GlobalOptions
  {
  }

type Command m = m ()

-- Парсер для глобальных опций
globalOptions :: Parser GlobalOptions
globalOptions = pure GlobalOptions

type AppPerks m = (MonadIO m, MonadUnliftIO m, MonadReader AppEnv m, HasConf m, SerialisedCredentials 'HBS2Basic)

-- TODO: key-mamagement-command-about-to-move-here

commands :: (AppPerks m) => Parser (Command m)
commands = hsubparser
  (  command "update"     (O.info (updateKeys <**> helper) (progDesc "update keys" ))
  <> command "list"       (O.info (listKeysCmd <**> helper) (progDesc "list keys" ))
  <> command "disclose"   (O.info (discloseKeyCmd <**> helper) (progDesc "disclose credentials" ))
  <> command "set-weight" (O.info (setWeightCmd <**> helper) (progDesc "set weight for a key"))
  <> command "add-mask"   (O.info (addPath <**> helper) (progDesc "add path/mask to search keys, ex. '/home/user/keys/*.key'"))
  <> command "config"     (O.info (showConfig <**> helper) (progDesc "show hbs2-keyman config"))
  )

opts :: (AppPerks m) => ParserInfo (GlobalOptions, Command m)
opts = O.info (liftA2 (,) globalOptions commands <**> helper)
  ( fullDesc
 <> header "hbs2-keyman" )


showConfig :: (AppPerks m) => Parser (Command m)
showConfig = do
  pure do
    readConfig >>=  liftIO . print . vcat . fmap pretty

addPath :: (AppPerks m) => Parser (Command m)
addPath = do
  masks <- many $ strArgument (metavar "KEYFILE-MASK")
  pure do
    cfg <- getConfigPath <&> takeDirectory
    mkdir cfg
    for_ masks $ \m -> do
      liftIO $ appendFile (cfg </> "config") (show $ "key-files" <+> dquotes (pretty m) <> line)

listKeysCmd :: (AppPerks m) => Parser (Command m)
listKeysCmd = pure do
  kw <- withState listKeys
  liftIO $ print $ vcat (fmap pretty kw)


data RChanScanEnv =
  RChanScanEnv
  { storage     :: AnyStorage
  , refchanAPI  ::  ServiceCaller RefChanAPI UNIX
  }

newtype ScanRefChansM m a = ScanRefChansM { fromScanRefChansM :: ReaderT RChanScanEnv m a }
                            deriving newtype ( Applicative
                                             , Functor
                                             , Monad
                                             , MonadIO
                                             , MonadUnliftIO
                                             , MonadReader RChanScanEnv
                                             , MonadTrans
                                             )

runScan :: Monad m => RChanScanEnv -> ScanRefChansM m a -> m a
runScan env action = runReaderT ( fromScanRefChansM action ) env


instance Monad m => HasClientAPI RefChanAPI UNIX (ScanRefChansM m) where
  getClientAPI = asks refchanAPI

instance Monad m => HasStorage (ScanRefChansM m) where
  getStorage = asks storage


updateKeys :: forall proto m . (AppPerks m, proto ~ UNIX) => Parser (Command m)
updateKeys = do
  prune <- flag False True ( long "prune" <> short 'p' <> help "prune keys for missed files")
  pure do
    updateLocalKeys prune
    updateGroupKeys

  where

    updateGroupKeys = do
      -- scanning refchans for group keys
      conf <- getConf
      let rchans = [ r | ListVal [SymbolVal "refchan", SignPubKeyLike r] <- conf ]

      -- FIXME: assume-huge-list
      seen <- withState selectAllSeenGKTx

      debug $ "SEEN" <+> pretty (List.length seen)

      flip runContT pure $ callCC \exit -> do
        when (List.null rchans) $ exit ()
        so' <- detectRPC
        so <- ContT $ maybe1 so' (warn $ yellow "peer is down")

        rpc <- ContT $ withRPC2 @RefChanAPI so
        sto <- ContT (withRPC2 @StorageAPI so) <&> AnyStorage . StorageClient


        txs <- S.toList_ do
          runScan (RChanScanEnv sto rpc) do

            for_ rchans $ \r -> do

              notice $ "scan refchan" <+> pretty (AsBase58  r)

              walkRefChanTx @proto (pure . not . flip HS.member seen) r $ \tx0 -> \case
                P _  (ProposeTran _ box) -> do

                  trace $ "got the fucking tx" <+> pretty tx0

                  void $ runMaybeT do
                    (_,bs) <- unboxSignedBox0 box & toMPlus

                    AnnotatedHashRef _ gkh <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict bs)
                                                & toMPlus . either (const Nothing) Just

                    -- FIXME: request-download-for-missed-groupkeys
                    -- FIXME: implement-download-with-timeout
                    gkbs <- runExceptT (readFromMerkle sto (SimpleKey (coerce gkh)))
                              >>= toMPlus

                    -- FIXME: do-it-right
                    --   если смогли скачать -- то уже потом не будем обрабатывать
                    --   потенциальная проблема -- мусорная транзакция, которая так и
                    --   будет болтаться, если она не AnnotatedHashRef
                    lift $ lift $ S.yield (Left tx0)

                    let gkz1 = deserialiseOrFail @(GroupKey 'Symm HBS2Basic) gkbs
                                  & either mempty List.singleton

                    let gkz2 = deserialiseOrFail @[GroupKey 'Symm HBS2Basic] gkbs
                                 & fromRight mempty

                    for_ (gkz1 <> gkz2) $ \gk -> do

                      gkId <- getGroupKeyId gk & toMPlus

                      --TODO: verify-group-key-id-if-possible

                      notice $ green "found new gk0" <+> pretty gkId <+> pretty gkh

                      lift $ lift $ S.yield (Right (gkId, gkh, gk) )
                      lift $ lift $ S.yield ( Left tx0 )

                _ -> do
                  lift $ S.yield (Left tx0)

        lift $ withState $ transactional do
          for_ (lefts txs) insertSeenGKTx
          for_ (rights txs)  $ \(gkId, h, gh) -> do
            insertGKTrack gkId h
            insertGKAccess h gh

        pure ()

    updateLocalKeys prune = do

      masks <- cfgValue @KeyFilesOpt @(Set String) <&> Set.toList
      files <- KeyRing.findFilesBy masks

      when prune do
        -- here <- doesPathExist fn
        --
        keys <- withState listKeys
        for_ keys $ \k -> void $ runMaybeT do
          fn <- keyFile k & toMPlus <&> Text.unpack
          here <- doesPathExist fn
          unless here do
            info $ "prune" <+> pretty fn
            lift $ withState $ deleteKey (keyId k)

      for_ files $ \fn -> runMaybeT do

        bs <- liftIO $ BS.readFile fn

        krf <- parseCredentials @'HBS2Basic (AsCredFile bs) & toMPlus

        let skp = view peerSignPk krf

        withState do
          -- info $ pretty (AsBase58 skp) <+> pretty "sign" <+> pretty fn
          updateKeyFile (SomePubKey @'Sign skp) fn
          updateKeyType (SomePubKey @'Sign skp)

          for_ (view peerKeyring  krf) $ \(KeyringEntry pk _ _) -> do
            -- info $ pretty (AsBase58 pk) <+> pretty "encrypt" <+> pretty fn
            updateKeyFile (SomePubKey @'Encrypt pk) fn
            updateKeyType (SomePubKey @'Encrypt pk)

          commitAll



setWeightCmd :: (AppPerks m) => Parser (Command m)
setWeightCmd = do
  k <- argument str (metavar "KEY" <> help "Key identifier")
  v <- argument auto (metavar "WEIGHT" <> help "Weight value")
  pure do
    withState $ updateKeyWeight k v

discloseKeyCmd :: (AppPerks m) => Parser (Command m)
discloseKeyCmd  = do
  -- k <- argument str (metavar "KEY" <> help "Key identifier")
  -- v <- argument auto (metavar "WEIGHT" <> help "Weight value")
  pure do
    notice "WIP"

main :: IO ()
main = do
  (_, action) <- execParser opts
  runApp action


