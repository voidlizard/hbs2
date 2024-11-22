module HBS2.Git3.Config.Local where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.System.Dir

import HBS2.Git.Local.CLI

import System.Directory

import Data.Config.Suckless.Script

import Data.Text.IO qualified as IO

readLocalConf :: MonadIO m => m [Syntax C]
readLocalConf = do

  let name = ".hbs2-git3/config"

  g <- findGitDir
          >>= orThrowUser ".git not found"
          <&> (</> name) . takeDirectory

  touch g

  liftIO (IO.readFile g)
    <&> parseTop
    >>= either (error.show) pure


