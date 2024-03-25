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
                <*> option pkey ( long "refchan" <> short 'r' <> help "refchan to post" )
                <*> option pkey ( long "author"  <> short 'a' <> help "author" )

  join $ execParser (O.info (parser <**> helper)
              ( fullDesc
             <> progDesc "hbs2-git oracle / distributed index builder"
             <> header "hbs2-git-oracle"))

  where
    pkey = maybeReader fromStringMay

runApp :: MonadUnliftIO m
       => Bool
       -> RefChanId L4Proto
       -> RefChanAuthor L4Proto
       -> m ()
runApp _ rchan author = do

  setLogging @DEBUG  (toStderr . logPrefix "[debug] ")
  setLogging @WARN   (toStderr . logPrefix "[warn]  ")
  setLogging @ERROR  (toStderr . logPrefix "[error] ")
  setLogging @NOTICE (toStderr . logPrefix "[debug] ")

  runWithOracleEnv rchan author runOracle

  `finally` do
      setLoggingOff @DEBUG
      setLoggingOff @WARN
      setLoggingOff @ERROR
      setLoggingOff @NOTICE



