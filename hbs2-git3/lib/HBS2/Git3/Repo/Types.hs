module HBS2.Git3.Repo.Types where

import HBS2.Git3.Prelude

import Data.Config.Suckless.Script

import Data.HashSet qualified as HS
import Data.Text qualified as Text

pattern RepoURL :: GitRemoteKey -> Syntax C
pattern RepoURL x <- (isRepoURL -> Just x)

isRepoURL :: Syntax C -> Maybe GitRemoteKey
isRepoURL = \case
  TextLike xs -> case mkList @C (fmap mkStr (Text.splitOn "://" xs)) of
    ListVal [TextLike pref, SignPubKeyLike puk] | pref `HS.member` prefixes -> Just puk
    _ -> Nothing

  _ -> Nothing

  where
    prefixes = HS.fromList [ "hbs2", "hbs23" ]


