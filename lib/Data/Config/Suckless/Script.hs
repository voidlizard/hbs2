{-# Language UndecidableInstances #-}
{-# Language PatternSynonyms #-}
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
import UnliftIO


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

helpEntry :: MonadUnliftIO m => Id -> RunM c m ()
helpEntry what = do
  man <- ask >>= readTVarIO
           <&> HM.lookup what
           <&> maybe mzero bindMan

  liftIO $ hPutDoc stdout (pretty man)

pattern HelpEntryBound :: forall {c}. Id -> [Syntax c]
pattern HelpEntryBound what <- [ListVal (SymbolVal "builtin:lambda" : SymbolVal what : _ )]

