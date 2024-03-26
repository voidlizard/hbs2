module Main where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App
import HBS2.Git.Oracle.Run

import Options.Applicative as O


type PKS = PubKey 'Sign HBS2Basic

data RunMode =
    RunIndex PKS
  | RunDump

main :: IO ()
main = do
  let parser = hsubparser ( pRunIndexCmd <> pRunDumpCmd )

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
      pure $ runApp chan RunDump



runApp :: MonadUnliftIO m
       => RefChanId L4Proto
       -> RunMode
       -> m ()
runApp chan mode = do

  setLogging @DEBUG  (toStderr . logPrefix "[debug] ")
  setLogging @WARN   (toStderr . logPrefix "[warn]  ")
  setLogging @ERROR  (toStderr . logPrefix "[error] ")
  setLogging @NOTICE (toStderr . logPrefix "[debug] ")

  runWithOracleEnv chan $ case mode of
    RunIndex a  -> runOracleIndex a
    RunDump{}   -> runDump

  `finally` do
      setLoggingOff @DEBUG
      setLoggingOff @WARN
      setLoggingOff @ERROR
      setLoggingOff @NOTICE



