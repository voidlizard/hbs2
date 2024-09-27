module HBS2.Git.DashBoard.Manifest where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.Data.RepoHead

import Data.Text qualified as Text
import Data.Either
import Streaming.Prelude qualified as S

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


