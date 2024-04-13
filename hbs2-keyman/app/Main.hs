module Main where

import HBS2.KeyMan.Prelude
import HBS2.KeyMan.App.Types
import HBS2.KeyMan.Config
import HBS2.KeyMan.State

import HBS2.Net.Auth.Credentials

import HBS2.Data.KeyRing qualified as KeyRing

import HBS2.System.Dir
import HBS2.System.Logger.Simple

import Data.Config.Suckless.KeyValue

import Options.Applicative qualified as O
import Data.Text qualified as Text
import Options.Applicative hiding (info)
import Data.Set qualified as Set
import Data.ByteString qualified as BS
import Control.Monad.Trans.Maybe
import Control.Monad.Reader


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

updateKeys :: (AppPerks m) => Parser (Command m)
updateKeys = do
  prune <- flag False True ( long "prune" <> short 'p' <> help "prune keys for missed files")
  pure do

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


