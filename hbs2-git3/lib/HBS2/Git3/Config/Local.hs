module HBS2.Git3.Config.Local where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.System.Dir

import HBS2.Git.Local.CLI

import System.Directory

import Data.Config.Suckless.Script

import Data.Text.IO qualified as IO

{- HLINT ignore "Functor law"-}

getConfigPath :: MonadIO m => m FilePath
getConfigPath = do

  let name = ".hbs2-git3"

  findGitDir
    >>= orThrowUser ".git not found"
    <&> (</> name) . takeDirectory


getConfigRootFile :: MonadIO m => m FilePath
getConfigRootFile = do

  let name = ".hbs2-git3"

  findGitDir
    >>= orThrowUser ".git not found"
    <&> (</> name) . takeDirectory
    <&> (</> "config")

readLocalConf :: MonadIO m => m [Syntax C]
readLocalConf = do

  conf <- getConfigPath <&> (</> "config")

  touch conf

  liftIO (IO.readFile conf)
    <&> parseTop
    >>= either (error.show) pure


