module HBS2.CLI.Run.Help where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as Text

{- HLINT ignore "Functor law" -}

helpList :: MonadUnliftIO m => Bool -> Maybe String -> RunM c m ()
helpList hasDoc p = do

  let match = maybe (const True) (Text.isPrefixOf . Text.pack) p

  d <- ask >>= readTVarIO
  let ks = [k | Id k <- List.sort (HM.keys d)
           , match k
           , not hasDoc || docDefined (HM.lookup (Id k) d)
           ]

  display_ $ vcat (fmap pretty ks)

  where
    docDefined (Just (Bind (Just w) _)) = True
    docDefined _ = False

helpEntries :: (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
helpEntries = do

  entry $ bindMatch "help" $ nil_ $ \syn -> do

      display_ $ "hbs2-cli tool" <> line

      case syn of

        [StringLike "--documented"] -> do
          helpList True Nothing

        (StringLike p : _) -> do
          helpList False (Just p)

        [ListVal (SymbolVal "builtin:lambda" : SymbolVal what : _ )] -> do
          man <- ask >>= readTVarIO
                   <&> HM.lookup what
                   <&> maybe mzero bindMan

          liftIO $ hPutDoc stdout (pretty man)

        _ -> helpList False Nothing

