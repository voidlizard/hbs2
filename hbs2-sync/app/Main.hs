{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
module Main where

import HBS2.Sync.Prelude

import System.Environment
import System.Exit qualified as Exit
import UnliftIO
import Control.Monad.Identity

quit :: forall m . MonadUnliftIO m => m ()
quit = liftIO Exit.exitSuccess

die :: forall a m . (MonadUnliftIO m, Pretty a) => a -> m ()
die what = liftIO do
  hPutDoc stderr (pretty what)
  Exit.exitFailure

helpEntries :: forall c m . (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
helpEntries = do

  entry $ bindMatch "help" $ nil_ $ \syn -> do

      display_ $ "hbs2-sync tool" <> line

      case syn of

        (StringLike p : _) -> do
          helpList False (Just p)

        HelpEntryBound what -> helpEntry what

        _ -> helpList False Nothing

      quit

  entry $ bindMatch "--help" $ nil_ \case
    HelpEntryBound what -> helpBanner >> helpEntry what >> quit
    [StringLike s]      -> helpBanner >> helpList False (Just s) >> quit
    _                   -> helpBanner >> helpList False Nothing >> quit

helpBanner :: MonadUnliftIO m => m ()
helpBanner = liftIO do
  print $
    "hbs2-sync tool" <> line

main :: IO ()
main = do

  cli <- liftIO getArgs <&> unlines . fmap unwords . splitForms
           >>= either (error.show) pure . parseTop
           <&> \case
            [] -> [mkList [mkSym "run", mkSym "."]]
            xs -> xs

  let dict = makeDict do
        helpEntries

        entry $ bindMatch "init"  $ nil_ $ const do
          pure ()

        entry $ bindMatch "run" $ nil_  \case
          [StringLike what] -> do
            runDirectory what

          _ -> do
            die "command not specified; run hbs2-sync help for details"

  void $ runSyncApp $ run dict cli

