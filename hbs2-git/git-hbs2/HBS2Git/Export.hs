module HBS2Git.Export where

import HBS2.Prelude
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

runExport :: MonadIO m => App m ()
runExport = do
  trace "Export"

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

  cache <- newHashCache (view appStateEnv env)

  for_ refs $ \(_, h) -> do
    liftIO $ gitGetTransitiveClosure cache mempty h <&> Set.toList

  db <- asks (view appStateEnv)

  -- FIXME: test-fixme

  withDB db $ transactional do
    els <- liftIO $ Cache.toList (hCache cache)
    for_ els $ \(k,vs,_) -> do
      for_ (Set.toList vs) $ \h -> do
        stateAddDep k h


