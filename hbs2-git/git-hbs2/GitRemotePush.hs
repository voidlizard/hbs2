module GitRemotePush where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
-- import HBS2.OrDie
import HBS2.System.Logger.Simple
-- import HBS2.Merkle
-- import HBS2.Hash

import HBS2.Git.Local
-- import HBS2.Git.Local.CLI

import HBS2Git.Config as Config
import HBS2Git.App

import GitRemoteTypes

import Data.Set (Set)

-- import HBS2Git.State
--


push :: forall m . (MonadIO m) => HashRef -> [Maybe GitRef] -> GitRemoteApp m ()

push remote [Nothing, br] = do
  (pwd, syn) <- Config.configInit

  -- brCfg <- cfgValue @ConfBranch @(Set GitRef) @m

  trace $ pretty pwd
  warn $ "about to delete branch" <+> pretty br <+> pretty "in" <+> pretty remote

  pure ()

push remote fromTo = do
  trace $ "push" <+> pretty remote <+> pretty fromTo

  -- TODO: 1. get-current-head
  -- TODO: 2. update-branch
  -- TODO: 3. make-new-head
  -- TODO: 4. update-state
  -- TODO: 5. update-ref
  -- TODO: 6. notify-ref-updated
  pure ()




