module HBS2.CLI.Run.Help where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as Text

{- HLINT ignore "Functor law" -}

helpList :: MonadUnliftIO m => Maybe String -> RunM c m ()
helpList p = do

  let match = maybe (const True) (Text.isPrefixOf . Text.pack) p

  d <- ask >>= readTVarIO
  let ks = [k | Id k <- List.sort (HM.keys d)
           , match k
           ]

  display_ $ vcat (fmap pretty ks)

helpEntries :: (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
helpEntries = do

  entry $ bindMatch "help" $ nil_ $ \syn -> do

      display_ $ "hbs2-cli tool" <> line

      case syn of
        (StringLike p : _) -> do
          helpList (Just p)

        [ListVal (SymbolVal "builtin:lambda" : SymbolVal what : _ )] -> do
          man <- ask >>= readTVarIO
                   <&> HM.lookup what
                   <&> maybe mzero bindMan

          liftIO $ hPutDoc stdout (pretty man)

        _ -> helpList Nothing

