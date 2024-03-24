module Main where

import HBS2.Git.Oracle.Prelude

import Options.Applicative

main :: IO ()
main = do
  let parser = runApp
                <$>  flag False True (    long "serve"
                        <> short 's'
                        <> help "serve"
                     )

  join $ execParser (info (parser <**> helper)
              ( fullDesc
             <> progDesc "hbs2-git oracle / distributed index builder"
             <> header "hbs2-git-oracle"))

runApp :: MonadUnliftIO m => Bool -> m ()
runApp _ = do
  pure ()


  -- where
  --   pLww :: ReadM (LWWRefKey HBS2Basic)
  --   pLww = maybeReader fromStringMay



