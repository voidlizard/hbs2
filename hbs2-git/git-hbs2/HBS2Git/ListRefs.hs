module HBS2Git.ListRefs where

import HBS2Git.Types
import HBS2Git.App

import HBS2.Git.Local.CLI

import Prettyprinter

runListRefs :: MonadIO m => App m ()
runListRefs = do
  shutUp
  refs <- gitGetRemotes
  liftIO $ print $ vcat (fmap pretty refs)

