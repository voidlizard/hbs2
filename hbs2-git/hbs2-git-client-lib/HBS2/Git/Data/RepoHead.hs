{-# Language TemplateHaskell #-}
module HBS2.Git.Data.RepoHead where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs

import HBS2.Git.Local

import Data.Text qualified as Text
import Data.ByteString.Char8 qualified as B8
import Data.Word
import Codec.Serialise
import Lens.Micro.Platform
import Data.Coerce
import Safe
import Data.Maybe
import Data.Set qualified as Set

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
  ,  repoHeadRefs'  :: [(GitRef, GitHash)]
  , _repoHeadExt    :: [RepoHeadExt]
  }
  deriving stock (Generic)

makeLenses ''RepoHead

repoHeadTags :: SimpleGetter RepoHead [Text]
repoHeadTags =
  to \h@RepoHeadSimple{} -> do
    catMaybes [ lastMay (B8.split '/' s) <&> (Text.pack . B8.unpack)
                      | (GitRef s, _)  <- view repoHeadRefs h, B8.isPrefixOf "refs/tags" s
                      ] & Set.fromList & Set.toList


repoHeadHeads :: SimpleGetter RepoHead [Text]
repoHeadHeads =
  to \h@RepoHeadSimple{} -> do
    catMaybes [ lastMay (B8.split '/' s) <&> (Text.pack . B8.unpack)
              | (GitRef s, v)  <- view repoHeadRefs h, B8.isPrefixOf "refs/heads" s
              ] & Set.fromList & Set.toList


repoHeadRefs :: Lens RepoHead
                     RepoHead
                     [(GitRef, GitHash)]
                     [(GitRef, GitHash)]

repoHeadRefs = lens g s
  where
   s rh r = rh { repoHeadRefs' = r }
   g rh = [ (r,v) | (r,v) <- repoHeadRefs' rh, v /= gitHashTomb ]

instance Serialise RepoHeadType
instance Serialise RepoHeadExt
instance Serialise RepoHead
