{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module HBS2.Git.DashBoard.Manifest where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.Data.RepoHead

import Data.Text qualified as Text
import Data.Either
import Streaming.Prelude qualified as S


pattern FixmeRefChanP :: forall {c} . PubKey Sign HBS2Basic -> Syntax c
pattern FixmeRefChanP x <- ListVal [ SymbolVal "fixme:"
                                   , ListVal [ SymbolVal "refchan", SignPubKeyLike x
                                   ]]


pattern PinnedRefBlob :: forall {c}. Text -> Text -> GitHash -> Syntax c
pattern PinnedRefBlob syn name hash <- ListVal [ SymbolVal "blob"
                                               , SymbolVal (Id syn)
                                               , LitStrVal name
                                               , asGitHash -> Just hash
                                               ]
{-# COMPLETE PinnedRefBlob #-}

asGitHash :: forall c . Syntax c -> Maybe GitHash
asGitHash  = \case
  LitStrVal s -> fromStringMay (Text.unpack s)
  _ -> Nothing



parseManifest :: Monad m => RepoHead -> m ([Syntax C], Text)
parseManifest mhead = do

  let rawManifest  = maybe mempty Text.lines (_repoManifest mhead)

  w <- S.toList_ do
         flip fix rawManifest $ \next ss -> do
          case ss of
            ( "" : rest )  -> S.yield (Right (Text.stripStart (Text.unlines rest)))
            ( a : rest )   -> S.yield (Left a ) >> next rest
            [] -> pure ()

  let meta = Text.unlines (lefts w)
                & Text.unpack
                & parseTop
                & fromRight mempty

  let manifest = mconcat $ rights  w

  pure (meta, manifest)


