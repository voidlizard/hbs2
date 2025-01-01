module HBS2.Git3.Types
  ( module HBS2.Git3.Types
  , module Exported
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.Git.Local as Exported

type GitRemoteKey = PubKey 'Sign 'HBS2Basic

newtype Short x = Short x

instance Pretty (Short GitObjectType) where
  pretty = \case
    (Short Tree)   -> "T"
    (Short Blob)   -> "B"
    (Short Commit) -> "C"


instance FromStringMaybe (Short GitObjectType) where
  fromStringMay = \case
    "T" -> Just (Short Tree)
    "B" -> Just (Short Blob)
    "C" -> Just (Short Commit)
    _   -> Just (Short Blob)


instance FromStringMaybe (Short SegmentObjectType) where
  fromStringMay = \case
    "T" -> Just (Short (GitObject Tree))
    "B" -> Just (Short (GitObject Blob))
    "C" -> Just (Short (GitObject Commit))
    "R" -> Just (Short RefObject)
    _   -> Just (Short (GitObject Blob))

data SegmentObjectType =
     GitObject GitObjectType
   | RefObject


