module Main where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App
import HBS2.Git.Oracle.Run

import Options.Applicative as O

main :: IO ()
main = do
  let parser = runApp
                <$>  flag False True (    long "serve"
                        <> short 's'
                        <> help "serve"
                     )

  join $ execParser (O.info (parser <**> helper)
              ( fullDesc
             <> progDesc "hbs2-git oracle / distributed index builder"
             <> header "hbs2-git-oracle"))

runApp :: MonadUnliftIO m => Bool -> m ()
runApp _ = do

  setLogging @DEBUG  (toStderr . logPrefix "[debug] ")
  setLogging @WARN   (toStderr . logPrefix "[warn]  ")
  setLogging @ERROR  (toStderr . logPrefix "[error] ")
  setLogging @NOTICE (toStderr . logPrefix "[debug] ")

  runWithOracleEnv runOracle

  `finally` do
      setLoggingOff @DEBUG
      setLoggingOff @WARN
      setLoggingOff @ERROR
      setLoggingOff @NOTICE



