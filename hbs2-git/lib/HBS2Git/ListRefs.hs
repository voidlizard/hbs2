module HBS2Git.ListRefs where

import HBS2Git.Types
import HBS2Git.App

import HBS2.Git.Local.CLI

import Data.Functor
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter

newtype AsRemoteEntry = AsRemoteEntry (Text,Text)

instance Pretty AsRemoteEntry where
  pretty (AsRemoteEntry (x,y)) = fill 16 (pretty x) <+> pretty y

-- TODO: backlog-list-refs-all-option
--  сделать опцию --all которая выведет
--  все известные ref-ы из стейта.
--  Сейчас выводятся только локальные

runListRefs :: MonadIO m => App m ()
runListRefs = do
  refs <- gitGetRemotes <&> filter isHbs2
  liftIO $ print $ vcat (fmap (pretty.AsRemoteEntry) refs)

  where
    isHbs2 (_,b) = Text.isPrefixOf "hbs2://" b

