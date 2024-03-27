module Main where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.Facts
import HBS2.Git.Oracle.App
import HBS2.Git.Oracle.Run

import Options.Applicative as O



data RunMode =
    RunIndex PKS
  | RunDump PKS
  | RunPipe

main :: IO ()
main = do
  let parser = hsubparser $ pRunIndexCmd <>
                            pRunDumpCmd <>
                            pRunPipeCmd

  join $ execParser (O.info (parser <**> helper)
              ( fullDesc
             <> progDesc "hbs2-git oracle / distributed index builder"
             <> header "hbs2-git-oracle"))

  where
    pkey = maybeReader (fromStringMay @(PubKey 'Sign HBS2Basic))

    pRunIndexCmd = command "index" ( O.info pRunIndex (progDesc "run index")  )

    pRunIndex = do
      chan   <- option pkey ( long "refchan" <> short 'r' <> help "refchan to post" )
      author <- option pkey ( long "author"  <> short 'a' <> help "author" )
      pure $ runApp chan (RunIndex author)

    pRunDumpCmd = command "dump" ( O.info pRunDump (progDesc "run index")  )
    pRunDump = do
      chan   <- option pkey ( long "refchan" <> short 'r' <> help "refchan to post" )
      pure $ runApp chan (RunDump chan)

    pRunPipeCmd = command "pipe" ( O.info pRunPipe (progDesc "run pipe mode")  )
    pRunPipe = do
      chan   <- option pkey ( long "refchan" <> short 'r' <> help "refchan for queries" )
      pure $ runApp chan RunPipe


runApp :: MonadUnliftIO m
       => RefChanId L4Proto
       -> RunMode
       -> m ()
runApp chan mode = do

  setLogging @DEBUG  (toStderr . logPrefix "[debug] ")
  setLogging @WARN   (toStderr . logPrefix "[warn]  ")
  setLogging @ERROR  (toStderr . logPrefix "[error] ")
  setLogging @NOTICE (toStderr . logPrefix "[debug] ")


  case mode of
    RunIndex a  -> runWithOracleEnv chan $ runOracleIndex a
    RunPipe{}   -> runWithOracleEnv chan $ runPipe
    RunDump pks -> runDump pks

  `finally` do
      setLoggingOff @DEBUG
      setLoggingOff @WARN
      setLoggingOff @ERROR
      setLoggingOff @NOTICE


