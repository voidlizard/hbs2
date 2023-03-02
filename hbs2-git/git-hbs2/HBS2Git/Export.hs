{-# Language TemplateHaskell #-}
module HBS2Git.Export where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.System.Logger.Simple

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2Git.App
import HBS2Git.State

import Data.Foldable (for_)
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Cache as Cache
import System.FilePath
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Codec.Serialise

newtype AsGitRefsFile a = AsGitRefsFile a

newtype RepoHead =
  RepoHead
  { _repoHeads :: HashMap GitRef GitHash
  }
  deriving stock (Generic)

makeLenses 'RepoHead

instance Pretty (AsGitRefsFile RepoHead) where
  pretty (AsGitRefsFile h) = vcat (fmap fmt els)
    where
      els = HashMap.toList (view repoHeads h)
      fmt (r,hx) = pretty hx <+> pretty r

instance Serialise RepoHead

data HashCache =
  HashCache
  { hCache :: Cache GitHash (Set GitHash)
  , hDb    :: DBEnv
  }

instance Hashable GitHash => HasCache HashCache GitHash (Set GitHash) IO where
  cacheInsert (HashCache cache _) = Cache.insert cache

  cacheLookup (HashCache cache db) k = do
    refs <- withDB db (stateGetDeps k)
    case refs of
      [] -> Cache.lookup' cache k
      xs -> pure $ Just $ Set.fromList xs

newHashCache :: MonadIO m => DBEnv -> m HashCache
newHashCache db = do
  ca <- liftIO $ Cache.newCache Nothing
  pure $ HashCache ca db

runExport :: MonadIO m => HashRef -> App m ()
runExport h = do
  trace $ "Export" <+> pretty h

  -- TODO: read-repo-head
  trace "read repository head"

  -- TODO: create-repo-head-block
  trace "create-repo-head-block"


  git <- asks (view appGitDir)

  trace $ "git directory is" <+> pretty git

  env <- ask

  let branches = cfgValue @ConfBranch env

  refs <- gitReadRefs git branches

  -- TODO: build-transitive-closure
  trace "build-transitive-closure"

  dbPath <- asks (view appStateDir) <&> (</> (show $ pretty h))

  trace $ "dbPath" <+> pretty dbPath

  db <- dbEnv dbPath

  cache <- newHashCache db

  for_ refs $ \(_, h) -> do
    liftIO $ gitGetTransitiveClosure cache mempty h <&> Set.toList

  withDB db $ transactional do
    els <- liftIO $ Cache.toList (hCache cache)
    for_ els $ \(k,vs,_) -> do
      for_ (Set.toList vs) $ \h -> do
        stateAddDep k h

  let repoHead = RepoHead (HashMap.fromList refs)

  shutUp

  liftIO $ print $ pretty (AsGitRefsFile repoHead)

  let hd = serialise repoHead

  liftIO $ LBS.putStr hd




