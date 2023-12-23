module Main where

import HBS2.Prelude
import HBS2.Net.Auth.Credentials

import HBS2.KeyMan.App.Types
import HBS2.KeyMan.Config
import HBS2.KeyMan.State
import HBS2.Data.KeyRing qualified as KeyRing

import HBS2.System.Logger.Simple

import Data.Config.Suckless.KeyValue


import Options.Applicative qualified as O
import Options.Applicative hiding (info)
import Data.Set qualified as Set
import Data.ByteString qualified as BS
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import UnliftIO


data GlobalOptions = GlobalOptions
  {
  }

type Command m = m ()

-- Парсер для глобальных опций
globalOptions :: Parser GlobalOptions
globalOptions = pure GlobalOptions

type AppPerks m = (MonadIO m, MonadReader AppEnv m, HasConf m)

-- Парсер для команд
commands :: (AppPerks m) => Parser (Command m)
commands = hsubparser
  (  command "update" (O.info (updateKeys <**> helper) (progDesc "update keys" ))
  <> command "list" (O.info (listKeysCmd <**> helper) (progDesc "list keys" ))
  )

opts :: (AppPerks m) => ParserInfo (GlobalOptions, Command m)
opts = O.info (liftA2 (,) globalOptions commands <**> helper)
  ( fullDesc
 -- <> progDesc "An application with global options and subcommands"
 <> header "hbs2-keyman" )

listKeysCmd :: (AppPerks m) => Parser (Command m)
listKeysCmd = pure do
  kw <- withState listKeys
  liftIO $ print $ vcat (fmap pretty kw)

updateKeys :: (AppPerks m) => Parser (Command m)
updateKeys = do
  pure do
    masks <- cfgValue @KeyFilesOpt @(Set String) <&> Set.toList
    files <- KeyRing.findFilesBy masks

    for_ files $ \fn -> runMaybeT do
      bs <- liftIO $ BS.readFile fn

      krf <- parseCredentials @HBS2Basic (AsCredFile bs) & toMPlus

      let skp = view peerSignPk krf

      withState do
        info $ pretty (AsBase58 skp) <+> pretty "sign" <+> pretty fn
        updateKeyFile (SomePubKey @'Sign skp) fn
        updateKeyType (SomePubKey @'Sign skp)

        for_ (view peerKeyring  krf) $ \(KeyringEntry pk _ _) -> do
          info $ pretty (AsBase58 pk) <+> pretty "encrypt" <+> pretty fn
          updateKeyFile (SomePubKey @'Encrypt pk) fn
          updateKeyType (SomePubKey @'Encrypt pk)

        commitAll

main :: IO ()
main = do
  (_, action) <- execParser opts
  runApp action


