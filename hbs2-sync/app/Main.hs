{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
module Main where

import HBS2.Sync.Prelude

import System.Environment
import UnliftIO
import Control.Monad.Identity


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
            [] -> [mkList [mkSym "run"]]
            xs -> xs

  let dict = makeDict do
        helpEntries
        syncEntries

        entry $ bindMatch "debug:cli:show" $ nil_ \case
          _ -> display cli


  void $ runSyncApp $ recover $ run dict cli

