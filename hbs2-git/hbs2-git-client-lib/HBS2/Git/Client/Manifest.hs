module HBS2.Git.Client.Manifest (getLastManifestFromStorage, addManifestBriefAndName) where

import Data.Coerce
import Data.Either
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as Text
import HBS2.Git.Client.App.Types
import HBS2.Git.Client.Config
import HBS2.Git.Client.Prelude
import HBS2.Git.Client.State
import HBS2.Git.Data.RepoHead
import HBS2.Storage.Operations.ByteString
import HBS2.System.Dir

addManifestBriefAndName :: (GitPerks m) => Maybe Text -> m (Text, Text, Maybe Text)
addManifestBriefAndName manifest = do
  dir <- getConfigDir
  let defBrief = "n/a"
      defName = takeFileName (takeDirectory dir) & Text.pack
      -- FIXME: size-hardcode
      header =
        lines (take 1024 (Text.unpack $ fromMaybe "" manifest))
          & takeWhile (not . L.null)
          & unlines
          & parseTop
          & fromRight mempty
      name = lastDef defName [n | ListVal [SymbolVal "name:", LitStrVal n] <- header]
      brief = lastDef defBrief [n | ListVal [SymbolVal "brief:", LitStrVal n] <- header]
  pure (name, brief, manifest)

getLastManifestFromStorage ::
  ( MonadReader GitEnv m,
    GitPerks m
  ) =>
  LWWRefKey 'HBS2Basic ->
  m (Text, Text, Maybe Text)
getLastManifestFromStorage lwwref = do
  manifest <- runMaybeT do
    sto <- asks _storage
    headRef <- MaybeT $ withState $ selectLastRepoHeadFor lwwref
    rhead <-
      runExceptT (readFromMerkle sto (SimpleKey (coerce headRef)))
        >>= toMPlus
          <&> deserialiseOrFail @RepoHead
        >>= toMPlus
    MaybeT $ pure $ _repoManifest rhead
  addManifestBriefAndName manifest
