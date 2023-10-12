module Main where

import HBS2.Prelude
import HBS2.OrDie

import HBS2Git.App
import HBS2Git.Export
import HBS2Git.ListRefs
import HBS2Git.KeysCommand
import HBS2.Net.Proto.Definition()

import RunShow

import Data.Functor
import Options.Applicative as O
import Control.Monad

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "git-hbs2"
  <> progDesc "helper tool for hbs2-git"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "export"    (info pExport (progDesc "export repo"))
                        <> command "list-refs" (info pListRefs (progDesc "list refs"))
                        <> command "show"      (info pShow (progDesc "show various types of objects"))
                        <> command "tools"     (info pTools (progDesc "misc tools"))
                        <> command "key"       (info pKeys (progDesc "manage keys"))
                        )

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


