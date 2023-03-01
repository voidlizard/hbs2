module HBS2Git.Export where

import HBS2.Prelude
import HBS2.System.Logger.Simple

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2Git.App

import Data.Foldable (for_)
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Cache as Cache

type HashCache = Cache GitHash (Set GitHash)

instance Hashable GitHash => HasCache (Cache GitHash (Set GitHash)) GitHash (Set GitHash) IO where
  cacheInsert = Cache.insert
  cacheLookup = Cache.lookup'


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

  cache <- liftIO (newCache Nothing :: IO HashCache)

  for_ refs $ \(_, h) -> do
    clo <- liftIO $ gitGetTransitiveClosure cache mempty h <&> Set.toList
    debug $ "closure:" <+> pretty (length clo)


