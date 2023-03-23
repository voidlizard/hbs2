module Main where

import HBS2.Prelude
import HBS2.System.Logger.Simple hiding (info)

import HBS2Git.App
import HBS2Git.Export
import HBS2Git.ListRefs

import RunShow

import Options.Applicative as O
import Control.Monad

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "hbsync block fetch"
  <> progDesc "fetches blocks from hbsync peers"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "export"    (info pExport (progDesc "export repo"))
                        <> command "list-refs" (info pListRefs (progDesc "list refs"))
                        <> command "show"      (info pShow  (progDesc "show current state"))
                        )

    pExport = do
      ref  <- strArgument (metavar "HASH-REF")
      kr   <- optional $ strOption (short 'k' <> long "keyring" <> metavar "KEYRING-FILE")
      pure $ runApp WithLog (runExport kr ref)

    pListRefs = do
      pure $ runApp NoLog runListRefs

    pShow = do
      ref <- strArgument (metavar "HASH-REF")
      pure $ runApp NoLog (runShow ref)

