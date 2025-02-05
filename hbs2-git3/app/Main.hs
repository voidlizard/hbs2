{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import HBS2.Git3.Prelude
import HBS2.Git3.Run
import HBS2.Git3.State
import HBS2.Git3.Logger

import Data.Config.Suckless.Script
import System.Environment qualified as E

main :: IO ()
main = flip runContT pure do

  setupLogger

  ContT $ bracket none $ const do
    silence

  argz <- liftIO $ E.getArgs
  cli <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  tvd <- newTVarIO theDict
  env' <- nullGit3Env
  let env = env' { gitRuntimeDict = Just (RuntimeDict tvd) }

  void $ lift $ withGit3Env env do
    conf <- readLocalConf
    recover $ setupLogger >> runEval tvd (conf <> cli)
      `finally` silence

