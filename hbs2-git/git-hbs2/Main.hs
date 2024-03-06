module Main where

import HBS2.Prelude
import HBS2.OrDie

import HBS2Git.App
import HBS2Git.Export
import HBS2Git.Tools
import HBS2Git.KeysCommand
import HBS2.Version

import RunShow

import Options.Applicative as O
import Control.Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS

import Paths_hbs2_git qualified as Pkg

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "git-hbs2"
  <> progDesc "helper tool for hbs2-git"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "init"      (info pInit (progDesc "init new hbs2 repo"))
                        <> command "list-refs" (info pListRefs (progDesc "list refs"))
                        <> command "show"      (info pShow (progDesc "show various types of objects"))
                        <> command "tools"     (info pTools (progDesc "misc tools"))
                        <> command "key"       (info pKeys (progDesc "manage keys"))
                        <> command "version"   (info pVersion (progDesc "show program version"))
                        )

    pVersion = pure do
        LBS.putStr $ Aeson.encode $(inlineBuildVersion Pkg.version)

    pExport = do
      keyfile   <- strArgument (metavar "KEIRING-FILE")
      pure $ runApp WithLog do
          runExport' keyfile

    pListRefs = do
      pure $ runApp NoLog runListRefs

    showReader s = if s == "config"
      then Just ShowConfig
      else ShowRef <$> fromStringMay s

    pShow = do
      object <- optional $
        argument (maybeReader showReader) (metavar "object" <> help "<HASH-REF> | config")

      pure $ runApp NoLog (runShow object)

    pTools = hsubparser (    command "scan" (info pToolsScan (progDesc "scan reference"))
                          <> command "export"    (info pExport (progDesc "export repo"))
                          <> command "refs" (info pToolsGetRefs (progDesc "list references"))

                        )

    pToolsScan = do
      ref  <- strArgument (metavar "HASH-REF")
      pure $ runApp WithLog (runToolsScan ref)

    pToolsGetRefs = do
      ref  <- strArgument (metavar "HASH-REF")
      pure $ runApp WithLog (runToolsGetRefs ref)


    pKeys = hsubparser (  command "list"   (info pKeysList    (progDesc "list keys for refs"))
                       <> command "refs"   (info pKeyRefsList (progDesc "list encrypted refs"))
                       <> command "update" (info pKeyUpdate   (progDesc "update key for the ref"))
                       )


    pKeyUpdate = do
      ref <- strArgument (metavar "REF-KEY")
      pure $ do
        rk <- pure (fromStringMay ref) `orDie` "invalid REF-KEY"
        runApp WithLog (runKeysUpdate rk)

    pKeyRefsList = do
      pure $ do
        runApp WithLog runKeyRefsList

    pKeysList = do
      ref <- strArgument (metavar "REF-KEY")
      pure $ do
        rk <- pure (fromStringMay ref) `orDie` "invalid REF-KEY"
        runApp WithLog (runKeysList rk)

    pInit = do
      opts <- pOpts
      pure do
        runInit (runInitRepo opts)

      where
        pOpts = pInteractive

        pInteractive = NewRepoOpts <$> optional pKeyring
                                   <*> pEncryption


        pEncryption = pEncryptionHere <|> pure Nothing

        pEncryptionHere = do
          puk <- option pEncPk ( short 'p' <> long "encryption-pk" <> help "public key for encryption")
          fn  <- strOption ( short 'e' <> long "keyring-enc" <> help "keyring for encryption" )
          pure $ Just (puk, fn)


        pEncPk :: ReadM (PubKey 'Encrypt (Encryption L4Proto))
        pEncPk  = eitherReader $
            maybe (Left "invalid encryption public key") pure . fromStringMay

        pKeyring = do
          strOption (short 'k' <> long "keyring" <> help "reference keyring file")

