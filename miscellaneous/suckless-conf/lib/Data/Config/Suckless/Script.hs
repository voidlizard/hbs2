{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
module Data.Config.Suckless.Script
  ( module Exported
  , module Data.Config.Suckless.Script
  ) where

import Data.Config.Suckless as Exported
import Data.Config.Suckless.Script.Internal as Exported

import Control.Monad
import Control.Monad.Reader
import Data.HashMap.Strict qualified as HM
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.List qualified as List
import Data.Text qualified as Text
import Data.String
import UnliftIO


{- HLINT ignore "Functor law" -}

helpList :: MonadUnliftIO m => Bool -> Maybe String -> RunM c m ()
helpList hasDoc p = do

  let match = maybe (const True) (Text.isPrefixOf . Text.pack) p

  d <- ask >>= readTVarIO
  let ks = [k | Id k <- List.sort (HM.keys d)
           , match k
           , docDefined (HM.lookup (Id k) d) || not hasDoc
           ]

  display_ $ vcat (fmap pretty ks)

  where
    docDefined (Just (Bind (Just Man{..}) _)) | not manHidden  = True
    docDefined _ = False

helpEntry :: MonadUnliftIO m => Id -> RunM c m ()
helpEntry what = do
  man <- ask >>= readTVarIO
           <&> HM.lookup what
           <&> maybe mzero bindMan

  liftIO $ hPutDoc stdout (pretty man)

pattern HelpEntryBound :: forall {c}. Id -> [Syntax c]
pattern HelpEntryBound what <- [ListVal (SymbolVal "builtin:lambda" : SymbolVal what : _ )]


splitOpts :: [(Id,Int)]
          -> [Syntax C]
          -> ([Syntax C], [Syntax C])

splitOpts def opts' = flip fix (mempty, opts) $ \go -> \case
  (acc, []) -> acc
  ( (o,a), r@(StringLike x) : rs ) -> do
    case HM.lookup (fromString x) omap of
      Nothing -> go ((o, a <> [r]), rs)
      Just n  -> do
        let (w, rest) = List.splitAt n rs
        let result = mkList @C ( r : w )
        go ( (o <> [result], a), rest )
  ( (o,a), r : rs ) -> do
      go ((o, a <> [r]), rs)

  where
    omap = HM.fromList [ (p, x) | (p,x) <- def ]
    opts = opts'


