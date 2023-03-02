module HBS2Git.ListRefs where

import HBS2Git.Types
import HBS2Git.App

import HBS2.Git.Local.CLI

import Data.Text (Text)
import Prettyprinter

newtype AsRemoteEntry = AsRemoteEntry (Text,Text)

instance Pretty AsRemoteEntry where
  pretty (AsRemoteEntry (x,y)) = fill 16 (pretty x) <+> pretty y

runListRefs :: MonadIO m => App m ()
runListRefs = do
  refs <- gitGetRemotes
  liftIO $ print $ vcat (fmap (pretty.AsRemoteEntry) refs)

