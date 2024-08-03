module HBS2.CLI.Run.Help where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as Text

helpEntries :: (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
helpEntries = do

  entry $ bindMatch "help" $ nil_ $ \syn -> do

      display_ $ "hbs2-cli tool" <> line

      case syn of

        [StringLike "--documented"] -> do
          helpList True Nothing

        (StringLike p : _) -> do
          helpList False (Just p)

        HelpEntryBound what -> helpEntry what

        _ -> helpList False Nothing

