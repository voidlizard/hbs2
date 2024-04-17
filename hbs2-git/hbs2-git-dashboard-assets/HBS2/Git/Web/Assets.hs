{-# Language TemplateHaskell #-}
module HBS2.Git.Web.Assets where

import Data.FileEmbed

import Data.ByteString

version :: Int
version = 3

assetsDir :: [(FilePath, ByteString)]
assetsDir = $(embedDir "hbs2-git-dashboard-assets/assets")


