{-# Language MultiWayIf #-}
{-# Language ImplicitParams #-}
module HBS2.Git.Web.Html.Types where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.Fixme as Fixme

import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Word
import Lucid.Base
import Network.URI.Encode
import System.FilePath
import Web.Scotty.Trans as Scotty

import Network.HTTP.Types.Status

newtype H a = H a

raiseStatus :: forall m . MonadIO m => Status -> Text -> m ()
raiseStatus s t = throwIO (StatusError s t)

itemNotFound s = StatusError status404 (Text.pack $ show $ pretty s)

rootPath :: [String] -> [String]
rootPath = ("/":)

data Domain = FixmeDomain

newtype FromParams (e :: Domain) a = FromParams a

class Path a where
  path :: [a] -> Text

instance Path String where
  path = Text.pack . joinPath . rootPath


class ToRoutePattern a where
  routePattern :: a -> RoutePattern

type WithBaseUrl = ?dashBoardBaseUrl :: Text

getBaseUrl :: WithBaseUrl => Text
getBaseUrl = ?dashBoardBaseUrl

withBaseUrl :: (WithBaseUrl => r) -> Text -> r
withBaseUrl thingInside baseUrl =
  let ?dashBoardBaseUrl = baseUrl in thingInside

toBaseURL :: (WithBaseUrl, ToURL a) => a -> Text
toBaseURL x = getBaseUrl <> toURL x

class ToURL a where
  toURL :: a -> Text

data family Tabs a :: Type

data RepoListPage = RepoListPage

data RepoPageTabs =  CommitsTab (Maybe GitHash)
                   | ManifestTab
                   | TreeTab (Maybe GitHash)
                   | IssuesTab
                   | ForksTab
                   | PinnedTab (Maybe (Text, Text, GitHash))
                    deriving stock (Eq,Ord,Show)

data RepoPage s a = RepoPage s a

data RepoRefs repo = RepoRefs repo

data RepoTree repo commit tree = RepoTree repo commit tree

data RepoTreeEmbedded repo commit tree = RepoTreeEmbedded repo commit tree

data RepoBlob repo commit tree blob = RepoBlob repo commit tree blob

data RepoSomeBlob repo blob tp = RepoSomeBlob repo blob tp

data RepoForksHtmx repo = RepoForksHtmx repo

newtype RepoManifest repo = RepoManifest repo

newtype RepoCommits repo = RepoCommits repo

data Paged q = Paged QueryOffset q

data RepoFixmeHtmx repo = RepoFixmeHtmx (Map Text Text) repo

data RepoCommitsQ repo off lim = RepoCommitsQ repo off lim

data RepoCommitDefault repo commit = RepoCommitDefault repo commit

data RepoCommitSummaryQ repo commit = RepoCommitSummaryQ repo commit

data RepoCommitPatchQ repo commit = RepoCommitPatchQ repo commit

data IssuePage repo issue = IssuePage repo issue


newtype ShortRef a = ShortRef a

shortRef :: Int -> Int -> String -> String
shortRef n k a = if k > 0 then  [qc|{b}..{r}|] else [qc|{b}|]
  where
    b = take n a
    r = reverse $ take k (reverse a)

instance ToHtml (ShortRef GitHash) where
  toHtml (ShortRef a) = toHtml (shortRef 10 0 (show $ pretty a))
  toHtmlRaw (ShortRef a) = toHtml (shortRef 10 0 (show $ pretty a))

instance ToHtml (ShortRef (LWWRefKey 'HBS2Basic)) where
  toHtml (ShortRef a) = toHtml (shortRef 14 3 (show $ pretty a))
  toHtmlRaw (ShortRef a) = toHtml (shortRef 14 3 (show $ pretty a))


toArg :: (Semigroup a, IsString a) => a -> a
toArg s = ":" <> s

toPattern :: Text -> RoutePattern
toPattern = fromString . Text.unpack


instance Pretty RepoPageTabs where
  pretty = \case
    CommitsTab{}   -> "commits"
    ManifestTab{}  -> "manifest"
    TreeTab{}      -> "tree"
    ForksTab{}     -> "forks"
    IssuesTab{}    -> "issues"
    PinnedTab{}    -> "pinned"

instance FromStringMaybe RepoPageTabs where
  fromStringMay = \case
    "commits"  -> pure (CommitsTab Nothing)
    "manifest" -> pure ManifestTab
    "tree"     -> pure (TreeTab Nothing)
    "forks"    -> pure ForksTab
    "issues"   -> pure IssuesTab
    "pinned"   -> pure $ PinnedTab Nothing
    _          -> pure (CommitsTab Nothing)


instance ToRoutePattern RepoListPage where
  routePattern = \case
    RepoListPage -> "/"

instance ToURL String where
  toURL str = path [str]

instance ToURL (RepoPage RepoPageTabs (LWWRefKey 'HBS2Basic)) where
  toURL (RepoPage s w)  = path @String [ "/", show (pretty s), show (pretty w)]
                            <> pred_
    where
     -- FIXME: use-uri-encode
     pred_ = case s of
      CommitsTab (Just p)       -> Text.pack $ "?ref=" <> show (pretty p)
      TreeTab (Just p)          -> Text.pack $ "?tree=" <> show (pretty p)
      PinnedTab (Just (s,n,h))  -> Text.pack $ "?ref=" <> show (pretty h)
      _ -> mempty

instance ToRoutePattern (RepoPage String String) where
  routePattern (RepoPage s w) = path ["/", toArg s, toArg w] & toPattern

instance ToURL RepoListPage where
  toURL _ =  "/"

instance ToURL (RepoRefs (LWWRefKey 'HBS2Basic))  where
  toURL (RepoRefs repo') = path ["/", "htmx", "refs", repo]
    where
      repo = show $ pretty repo'

instance ToRoutePattern (RepoRefs String) where
  routePattern (RepoRefs s) = path ["/", "htmx", "refs", toArg s] & toPattern


instance ToURL (RepoTree (LWWRefKey 'HBS2Basic) GitHash GitHash)  where
  toURL (RepoTree k co tree') = path ["/", "htmx", "tree", repo, commit, tree]
    where
      repo = show $ pretty k
      commit = show $ pretty co
      tree = show $ pretty tree'

instance ToRoutePattern (RepoTree String String String) where
  routePattern (RepoTree r co tree) =
    path ["/", "htmx", "tree", toArg r, toArg co, toArg tree] & toPattern

instance ToURL (RepoBlob (LWWRefKey 'HBS2Basic) GitHash GitHash GitHash)  where
  toURL (RepoBlob k co t bo) = path ["/", "htmx", "blob", repo, commit, tree, blob]
    where
      repo = show $ pretty k
      commit = show $ pretty co
      tree   = show $ pretty t
      blob = show $ pretty bo

instance ToRoutePattern (RepoBlob String String String String) where
  routePattern (RepoBlob r c t b) =
    path ["/", "htmx", "blob", toArg r, toArg c, toArg t, toArg b] & toPattern


instance ToURL (RepoSomeBlob (LWWRefKey 'HBS2Basic) Text GitHash)  where
  toURL (RepoSomeBlob k tp' blo) = path ["/", "htmx", "some-blob", repo, tp, blob]
    where
      repo = show $ pretty k
      tp = Text.unpack tp'
      blob = show $ pretty blo

instance ToRoutePattern (RepoSomeBlob String String String) where
  routePattern (RepoSomeBlob r t b) =
    path ["/", "htmx", "some-blob", toArg r, toArg t, toArg b] & toPattern

instance ToURL (RepoManifest (LWWRefKey 'HBS2Basic))  where
  toURL (RepoManifest repo') = path ["/", "htmx", "manifest", repo]
    where
      repo = show $ pretty repo'

instance ToRoutePattern (RepoManifest String) where
  routePattern (RepoManifest s) = path ["/", "htmx", "manifest", toArg s] & toPattern

instance ToURL (RepoCommits (LWWRefKey 'HBS2Basic))  where
  toURL (RepoCommits repo') = path ["/", "htmx", "commits", repo]
    where
      repo = show $ pretty repo'

instance ToRoutePattern (RepoCommits String) where
  routePattern (RepoCommits s) = path ["/", "htmx", "commits", toArg s] & toPattern

instance ToURL (RepoCommitsQ (LWWRefKey 'HBS2Basic) Int Int)  where
  toURL (RepoCommitsQ repo' off lim) = path ["/", "htmx", "commits", repo, show off, show lim]
    where
      repo = show $ pretty repo'

instance ToRoutePattern (RepoCommitsQ String String String) where
  routePattern (RepoCommitsQ r o l) =
    path ["/", "htmx", "commits", toArg r, toArg o, toArg l] & toPattern

instance ToURL (RepoCommitDefault (LWWRefKey 'HBS2Basic) GitHash)  where
  toURL (RepoCommitDefault repo' h) = toURL (RepoCommitSummaryQ repo' h)

instance ToRoutePattern (RepoCommitDefault String String) where
  routePattern (RepoCommitDefault r h) = routePattern (RepoCommitSummaryQ r h)

instance ToURL (RepoCommitSummaryQ (LWWRefKey 'HBS2Basic) GitHash)  where
  toURL (RepoCommitSummaryQ repo' h) = path ["/", "htmx", "commit", "summary", repo, ha]
    where
      repo = show $ pretty repo'
      ha = show $ pretty h

instance ToRoutePattern (RepoCommitSummaryQ String String) where
  routePattern (RepoCommitSummaryQ r h) =
    path ["/", "htmx", "commit", "summary", toArg r, toArg h] & toPattern

instance ToURL (RepoCommitPatchQ (LWWRefKey 'HBS2Basic) GitHash)  where
  toURL (RepoCommitPatchQ repo' h) = path ["/", "htmx", "commit", "patch", repo, ha]
    where
      repo = show $ pretty repo'
      ha = show $ pretty h

instance ToRoutePattern (RepoCommitPatchQ String String) where
  routePattern (RepoCommitPatchQ r h) =
    path ["/", "htmx", "commit", "patch", toArg r, toArg h] & toPattern


instance ToURL (RepoTreeEmbedded (LWWRefKey 'HBS2Basic) GitHash GitHash)  where
  toURL (RepoTreeEmbedded k co tree') = path ["/", "htmx", "tree", "embedded", repo, commit, tree]
    where
      repo = show $ pretty k
      commit = show $ pretty co
      tree = show $ pretty tree'

instance ToRoutePattern (RepoTreeEmbedded String String String) where
  routePattern (RepoTreeEmbedded r co tree) =
    path ["/", "htmx", "tree", "embedded", toArg r, toArg co, toArg tree] & toPattern


instance ToURL (RepoForksHtmx (LWWRefKey 'HBS2Basic))  where
  toURL (RepoForksHtmx k) = path ["/", "htmx", "forks", repo]
    where
      repo = show $ pretty k

instance ToRoutePattern (RepoFixmeHtmx  String) where
  routePattern (RepoFixmeHtmx _ r) =
    path ["/", "htmx", "fixme", toArg r] & toPattern

instance ToURL (RepoFixmeHtmx RepoLww)  where
  toURL (RepoFixmeHtmx argz' k) = path ["/", "htmx", "fixme", repo] <> "?" <> filtPart
    where
      repo = show $ pretty k
      filtPart = Text.intercalate "&" [ [qc|{encodeText k}={encodeText v}|] | (k,v) <- argz ]
      argz = Map.toList argz'

instance ToURL (Paged (RepoFixmeHtmx RepoLww))  where
  toURL (Paged p (RepoFixmeHtmx a k)) = toURL (RepoFixmeHtmx paged k)
    where paged = Map.insert "$page" (Text.pack (show p)) a

instance ToRoutePattern (RepoForksHtmx String) where
  routePattern (RepoForksHtmx r) =
    path ["/", "htmx", "forks", toArg r] & toPattern


instance ToRoutePattern (IssuePage String String) where
  routePattern (IssuePage s w) = path ["/", "issues", toArg s, toArg w] & toPattern

instance ToURL (IssuePage RepoLww FixmeKey)  where
  toURL (IssuePage r i) = path ["/", "issues", repo, issue]
    where
      repo = show $ pretty r
      issue = show $ pretty i


agePure :: forall a b . (Integral a,Integral b)  => a -> b -> Text
agePure t0 t = do
  let sec = fromIntegral @_ @Word64 t - fromIntegral t0
  fromString $ show $
    if | sec > 86400  -> pretty (sec `div` 86400)  <+> "days ago"
       | sec > 3600   -> pretty (sec `div` 3600)   <+> "hours ago"
       | otherwise    -> pretty (sec `div` 60)     <+> "minutes ago"
