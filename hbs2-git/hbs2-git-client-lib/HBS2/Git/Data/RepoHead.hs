module HBS2.Git.Data.RepoHead where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs

import HBS2.Git.Local

import Data.Word
import Codec.Serialise

data RepoHeadType = RepoHeadType1
                    deriving stock (Enum,Generic)

data RepoHeadExt = RepoHeadExt
                   deriving stock Generic

data RepoHead =
  RepoHeadSimple
  { _repoHeadType   :: RepoHeadType
  , _repoHeadTime   :: Word64
  , _repoHeadGK0    :: Maybe HashRef
  , _repoHeadName   :: Text
  , _repoHeadBrief  :: Text
  , _repoManifest   :: Maybe Text
  , _repoHeadRefs   :: [(GitRef, GitHash)]
  , _repoHeadExt    :: [RepoHeadExt]
  }
  deriving stock (Generic)


instance Serialise RepoHeadType
instance Serialise RepoHeadExt
instance Serialise RepoHead
