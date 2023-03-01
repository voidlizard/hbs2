module HBS2Git.Export where

import HBS2.Prelude
import HBS2.System.Logger.Simple

import HBS2.Git.Local

import HBS2Git.App

import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Set (Set)
import Data.Set qualified as Set


runExport :: MonadIO m => App m ()
runExport = do
  trace "Export"

  -- TODO: read-repo-head
  trace "read repository head"

  -- TODO: create-repo-head-block
  trace "create-repo-head-block"

  -- TODO: build-transitive-closure
  trace "build-transitive-closure"

  git <- asks (view appGitDir)

  trace $ "git directory is" <+> pretty git

  env <- ask

  let branches = cfgValue @ConfBranch env

  refs <- gitReadRefs git branches

  notice $ vcat (fmap pretty refs)

  pure ()


