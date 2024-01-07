module Main where

import HBS2.Share.App

import Options.Applicative as O

-- Парсер для глобальных опций
globalOptions :: Parser [AppOption]
globalOptions = do
  dry <- optional (flag' True (long "dry" <> short 'n' <> help "dont post anything"))
           <&> maybe mempty (const [AppDontPostOpt])

  debug <- optional (flag' True (long "debug" <> short 'v' <> help "allow debug output"))
             <&> maybe mempty (const [AppDebugOpt])

  trace <- optional (flag' True (long "trace" <> help "allow more debug output"))
             <&> maybe mempty (const [AppDebugOpt])


  replica <- optional (flag' True (long "replica" <> help "replica (slave) mode"))
             <&> maybe mempty (const [AppReplicaOpt])

  pure (replica <> debug <> dry <> trace )

-- Парсер для команд
commands :: AppPerks m => Parser (ShareCLI m ())
commands = defCmd

defCmd :: AppPerks m => Parser (ShareCLI m ())
defCmd = pure $ runSync

opts :: AppPerks m => ParserInfo ([AppOption], ShareCLI m ())
opts = O.info (liftA2 (,) globalOptions commands <**> helper)
  ( fullDesc
 -- <> progDesc "An application with global options and subcommands"
 <> header "hbs2-share" )

main :: IO ()
main = do
  (o, action) <- execParser opts
  runApp o action


