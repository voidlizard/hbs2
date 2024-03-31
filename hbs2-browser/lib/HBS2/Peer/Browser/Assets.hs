{-# Language TemplateHaskell #-}
module HBS2.Peer.Browser.Assets where

import Data.FileEmbed

import Data.ByteString

version :: Int
version = 6

cssDir :: [(FilePath, ByteString)]
cssDir = $(embedDir "assets")

templates :: [(FilePath, ByteString)]
templates = $(embedDir "templates")

