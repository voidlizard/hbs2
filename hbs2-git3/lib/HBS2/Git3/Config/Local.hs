module HBS2.Git3.Config.Local where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.System.Dir

import HBS2.Git.Local.CLI

import Control.Monad.Trans.Maybe

{- HLINT ignore "Functor law"-}

getConfigPath :: MonadIO m => m (Maybe FilePath)
getConfigPath = do
  let name = ".hbs2-git3"
  runMaybeT do
    gitDir
      >>= toMPlus <&> (</> name) . takeDirectory


