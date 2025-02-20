module HBS2.Git3.Repo.Types where

import HBS2.Git3.Prelude

import Data.Config.Suckless.Script

import Data.HashSet qualified as HS
import Data.Text qualified as Text

pattern RepoURL :: forall {c}  . IsContext c => GitRemoteKey -> Syntax c
pattern RepoURL x <- (isRepoURL [ "hbs2", "hbs23" ] -> Just x)

pattern RepoURL3 :: forall {c} . IsContext c => GitRemoteKey -> Syntax c
pattern RepoURL3 x <- (isRepoURL [ "hbs23" ] -> Just x)

remoteRepoURL :: GitRemoteKey -> Text
remoteRepoURL k = Text.pack $ show $ "hbs23://" <> pretty (AsBase58 k)

isRepoURL :: forall c . IsContext c => [Text] -> Syntax c -> Maybe GitRemoteKey
isRepoURL pref = \case
  TextLike xs -> case mkList @C (fmap mkStr (Text.splitOn "://" xs)) of
    ListVal [TextLike pref, SignPubKeyLike puk] | pref `HS.member` prefixes -> Just puk
    _ -> Nothing

  _ -> Nothing

  where
    prefixes = HS.fromList pref


