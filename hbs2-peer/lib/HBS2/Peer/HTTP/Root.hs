module HBS2.Peer.HTTP.Root where

import HBS2.Prelude.Plated

import System.FilePath
import Data.Text qualified as Text

path :: [String] -> Text
path = Text.pack . joinPath
