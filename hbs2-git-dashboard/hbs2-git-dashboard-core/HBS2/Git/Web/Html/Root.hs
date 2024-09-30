{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# Language MultiWayIf #-}
module HBS2.Git.Web.Html.Root where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.State.Commits
import HBS2.Git.DashBoard.Manifest
import HBS2.Git.DashBoard.Fixme as Fixme

import HBS2.OrDie

import HBS2.Git.Data.Tx.Git
import HBS2.Git.Data.RepoHead
import HBS2.Git.Web.Assets

-- import Data.Text.Fuzzy.Tokenize as Fuzz

import Data.Map qualified as Map
import Data.Map (Map)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)
import Lucid.Htmx

import Skylighting qualified as Sky
import Skylighting.Tokenizer
import Skylighting.Format.HTML.Lucid as Lucid

import Control.Applicative
import Text.Pandoc hiding (getPOSIXTime)
import System.FilePath
import Data.Word
import Data.Either
import Data.List qualified as List
import Data.List (sortOn)

import Web.Scotty.Trans as Scotty
import Network.URI.Encode

import Data.Kind

import Streaming.Prelude qualified as S

import Network.HTTP.Types.Status

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

isActiveTab :: RepoPageTabs -> RepoPageTabs -> Bool
isActiveTab a b = case (a,b)  of
  (CommitsTab{},CommitsTab{}) -> True
  (ManifestTab{},ManifestTab{}) -> True
  (TreeTab{},TreeTab{}) -> True
  _ -> False

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


myCss :: Monad m => HtmlT m ()
myCss = do
  link_ [rel_ "stylesheet", href_ (path ["css/custom.css"])]

hyper_ :: Text -> Attribute
hyper_  = makeAttribute "_"

ariaLabel_ :: Text -> Attribute
ariaLabel_ = makeAttribute "aria-label"

onClickCopy :: Text -> Attribute
onClickCopy s =
  hyper_ [qc|on click writeText('{s}') into the navigator's clipboard
set my innerHTML to '{svgIconText IconCopyDone}'
set @data-tooltip to 'Copied!'
wait 2s
set my innerHTML to '{svgIconText IconCopy}'
set @data-tooltip to 'Copy'
|]

markdownToHtml :: Text -> Either PandocError String
markdownToHtml markdown = runPure $ do
  doc <- readMarkdown def {readerExtensions = pandocExtensions} markdown
  html <- writeHtml5String def {writerExtensions = pandocExtensions} doc
  return $ Text.unpack html

renderMarkdown' :: Text -> Text
renderMarkdown' markdown = case markdownToHtml markdown of
    Left{} -> markdown
    Right html -> Text.pack html

renderMarkdown :: Text -> Html ()
renderMarkdown markdown = case markdownToHtml markdown of
    Left{} -> blockquote_ (toHtml markdown)
    Right html -> toHtmlRaw $ Text.pack html

instance ToHtml RepoBrief where
  toHtml (RepoBrief txt) = toHtmlRaw (renderMarkdown' txt)
  toHtmlRaw (RepoBrief txt) = toHtmlRaw (renderMarkdown' txt)

data WithTime a = WithTime Integer a

agePure :: forall a b . (Integral a,Integral b)  => a -> b -> Text
agePure t0 t = do
  let sec = fromIntegral @_ @Word64 t - fromIntegral t0
  fromString $ show $
    if | sec > 86400  -> pretty (sec `div` 86400)  <+> "days ago"
       | sec > 3600   -> pretty (sec `div` 3600)   <+> "hours ago"
       | otherwise    -> pretty (sec `div` 60)     <+> "minutes ago"


instance ToHtml GitRef where
  toHtml (GitRef s)= toHtml s
  toHtmlRaw (GitRef s)= toHtmlRaw s

rootPage :: Monad m => HtmlT m () -> HtmlT m ()
rootPage content  = do
  doctypehtml_ do
    head_ do
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      -- FIXME: static-local-loading
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@picocss/pico@2.0.6/css/pico.min.css"]
      script_ [src_ "https://unpkg.com/hyperscript.org@0.9.12"] ""
      script_ [src_ "https://unpkg.com/htmx.org@1.9.11"] ""
      myCss

    body_ do

      header_ [class_ "container-fluid"] do
        nav_ do
          ul_ $ li_ $ a_ [href_ (toURL RepoListPage)] $ strong_ "hbs2-git dashboard"

      content


dashboardRootPage :: (DashBoardPerks m, MonadReader DashBoardEnv m) => HtmlT m ()
dashboardRootPage = rootPage do

  items <- lift $ selectRepoList mempty

  now  <- liftIO getPOSIXTime <&> fromIntegral . round

  main_ [class_ "container-fluid"] $ do
    div_ [class_ "wrapper"] $ do
      aside_ [class_ "sidebar"] $ do
        div_ [class_ "info-block"] $ small_ "Всякая разная рандомная информация хрен знает, что тут пока выводить"
        div_ [class_ "info-block"] $ small_ "Всякая разная рандомная информация хрен знает, что тут пока выводить"

      div_ [class_ "content"] do

        section_ do
          h2_ "Git repositories"
          form_ [role_ "search"] do
            input_ [name_ "search", type_ "search"]
            input_ [type_ "submit", value_ "Search"]

        section_ do

          for_ items $ \it@RepoListItem{..} -> do

            let locked = isJust $ coerce @_ @(Maybe HashRef) rlRepoGK0

            let url = toURL (RepoPage (CommitsTab Nothing) (coerce @_ @(LWWRefKey 'HBS2Basic) rlRepoLww))
            -- path ["repo", Text.unpack $ view rlRepoLwwAsText it]
            let t = fromIntegral $ coerce @_ @Word64 rlRepoSeq

            let updated = agePure t now

            article_ [class_ "repo-list-item"] do
              div_ do

                h5_ do
                  toHtml rlRepoName

                div_ [class_ "repo-list-item-link-wrapper"] $ do
                  a_ [href_ url] (toHtml $ view rlRepoLwwAsText it)
                  button_ [class_ "copy-button", onClickCopy (view rlRepoLwwAsText it), data_ "tooltip" "Copy"] do
                    svgIcon IconCopy

                toHtml rlRepoBrief

              div_ do

                div_ [class_ "text-nowrap"] do
                  small_ $ "Updated " <> toHtml updated

                when locked do
                  div_ do
                    small_ do
                      span_ [class_ "inline-icon-wrapper"] $ svgIcon IconLockClosed
                      "Encrypted"

                div_ do
                  small_ do
                    span_ [class_ "inline-icon-wrapper"] $ svgIcon IconGitCommit
                    strong_ $ toHtml $ show rlRepoCommits
                    " commits"

                div_ do
                  small_ do
                    span_ [class_ "inline-icon-wrapper"] $ svgIcon IconGitFork
                    strong_ $ toHtml $ show rlRepoForks
                    " forks"



tabClick :: Attribute
tabClick =
  hyper_ "on click take .contrast from .tab for event's target"


parsedManifest :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoListItem  -> m ([Syntax C], Text)
parsedManifest RepoListItem{..} = do

  sto <- asks _sto
  mhead <- readRepoHeadFromTx sto (coerce rlRepoTx)

  case mhead of
    Just x  -> parseManifest (snd x)
    Nothing -> pure (mempty, coerce rlRepoBrief)

thisRepoManifest :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoHead -> HtmlT m ()
thisRepoManifest rh = do
  (_, man) <- lift $ parseManifest rh
  toHtmlRaw (renderMarkdown' man)

repoRefs :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> HtmlT m ()
repoRefs lww = do

  refs <- lift $ gitShowRefs lww
  table_ [] do
    for_ refs $ \(r,h) -> do
      let r_ = Text.pack $ show $ pretty r
      let co = show $ pretty h
      let uri = toURL (RepoTree lww h h)

      let showRef = Text.isPrefixOf "refs" r_

      when showRef do
        tr_ do
          td_ do

            if | Text.isPrefixOf "refs/heads" r_ -> do
                  svgIcon IconGitBranch
               | Text.isPrefixOf "refs/tags" r_ -> do
                  svgIcon IconTag
               | otherwise -> mempty

          td_ (toHtml r_)
          td_ [class_ "mono"] $ do
              a_ [ href_ "#"
                 , hxGet_ uri
                 , hxTarget_ "#repo-tab-data"
                 ] (toHtml $ show $ pretty h)


treeLocator :: DashBoardPerks m
            => LWWRefKey 'HBS2Basic
            -> GitHash
            -> TreeLocator
            -> HtmlT m ()
            -> HtmlT m ()

treeLocator lww co locator next = do

  let repo = show $ pretty $ lww

  let co_ = show $ pretty co

  let prefixSlash x = if fromIntegral x > 1 then span_ "/" else ""
  let showRoot =
        [ hxGet_ (toURL (RepoTree lww co co))
        , hxTarget_ "#repo-tab-data"
        , href_ "#"
        ]

  span_ [] $ a_ [ hxGet_ (toURL (RepoRefs lww))
                , hxTarget_ "#repo-tab-data"
                , href_ "#"
                ] $ toHtml (take 10 repo <> "..")
  span_ [] "/"
  span_ [] $ a_ showRoot $ toHtml (take 10 co_ <> "..")
  unless (List.null locator) do
    span_ [] "/"
    for_ locator $ \(_,this,level,name) -> do
      prefixSlash level
      let uri = toURL (RepoTree lww co (coerce @_ @GitHash this))
      span_ [] do
        a_ [ href_ "#"
           , hxGet_ uri
           , hxTarget_ "#repo-tab-data"
           ] (toHtml (show $ pretty name))
  next


repoTreeEmbedded :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> GitHash -- ^ this
         -> GitHash -- ^ this
         -> HtmlT m ()

repoTreeEmbedded = repoTree_ True


repoTree :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> GitHash -- ^ this
         -> GitHash -- ^ this
         -> HtmlT m ()

repoTree = repoTree_ False

repoTree_ :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => Bool
         -> LWWRefKey 'HBS2Basic
         -> GitHash -- ^ this
         -> GitHash -- ^ this
         -> HtmlT m ()

repoTree_ embed lww co root = do

  tree <- lift $ gitShowTree lww root
  back' <- lift $ selectParentTree (TreeCommit co) (TreeTree root)

  let syntaxMap = Sky.defaultSyntaxMap

  let sorted = sortOn (\(tp, _, name) -> (tpOrder tp, name)) tree
        where
          tpOrder Tree = (0 :: Int)
          tpOrder Blob = 1
          tpOrder _    = 2

  locator <- lift $ selectTreeLocator (TreeCommit co) (TreeTree root)

  let target = if embed then "#repo-tab-data-embedded" else "#repo-tab-data"

  table_ [] do

    unless embed do

        tr_ do
          td_ [class_ "tree-locator", colspan_ "3"] do
            treeLocator lww co locator none

        tr_ mempty do

          for_ back' $ \r -> do
            let rootLink = toURL (RepoTree lww co (coerce @_ @GitHash r))
            td_ $ svgIcon IconArrowUturnLeft
            td_ ".."
            td_ do a_ [ href_ "#"
                      , hxGet_ rootLink
                      , hxTarget_ target
                      ] (toHtml $ show $ pretty r)

    for_ sorted $ \(tp,h,name) -> do
      let itemClass = pretty tp & show & Text.pack
      let hash_ = show $ pretty h
      let uri = toURL $ RepoTree lww co h
      tr_ mempty do
        td_  $ case tp of
          Commit -> mempty
          Tree   -> svgIcon IconFolderFilled
          Blob   -> do
            let syn = Sky.syntaxesByFilename syntaxMap (Text.unpack name)
                        & headMay
                        <&> Text.toLower . Sky.sName

            let icon = case syn of
                         Just "haskell"    -> IconHaskell
                         Just "markdown"   -> IconMarkdown
                         Just "nix"        -> IconNix
                         Just "bash"       -> IconBash
                         Just "python"     -> IconPython
                         Just "javascript" -> IconJavaScript
                         Just "sql"        -> IconSql
                         Just s | s `elem` ["cabal","makefile","toml","ini","yaml"]
                                           -> IconSettingsFilled
                         _                 -> IconFileFilled

            svgIcon icon

        -- debug $ red "PUSH URL" <+> pretty (path ["back", wtf])

        td_ [class_ itemClass] (toHtml $ show $ pretty name)
        td_ [class_ "mono"] do
          case tp of
            Blob -> do
              let blobUri = toURL $ RepoBlob lww co root h
              a_ [ href_ "#"
                 , hxGet_ blobUri
                 , hxTarget_ target
                 ] (toHtml hash_)

            Tree -> do
              a_ [ href_ "#"
                 , hxGet_ uri
                 , hxTarget_ target
                 ] (toHtml hash_)

            _ -> mempty


{- HLINT ignore "Functor law" -}

data RepoCommitStyle = RepoCommitSummary | RepoCommitPatch
                       deriving (Eq,Ord,Show)

repoCommit :: (DashBoardPerks m, MonadReader DashBoardEnv m)
            => RepoCommitStyle
            -> LWWRefKey 'HBS2Basic
            -> GitHash
            -> HtmlT m ()

repoCommit style lww hash = do
  let syntaxMap = Sky.defaultSyntaxMap

  txt <- lift $ getCommitRawBrief lww hash

  let header = Text.lines txt & takeWhile (not . Text.null)
                              & fmap Text.words

  let au = [ Text.takeWhile (/= '<') (Text.unwords a)
           | ("Author:" : a) <- header
           ] & headMay

  table_ [class_ "item-attr"] do

    tr_ do
      th_ [width_ "16rem"] $ strong_ "back"
      td_ $ a_ [ href_ (toURL (RepoPage (CommitsTab (Just hash)) lww))
               ] $ toHtml $ show $ pretty hash

    for_ au $ \author -> do
      tr_ do
        th_ $ strong_ "author"
        td_ $ toHtml  author

    tr_ $ do
      th_ $ strong_ "view"
      td_ do
        ul_ [class_ "misc-menu"]do
            li_ $ a_ [ href_ "#"
                     , hxGet_ (toURL (RepoCommitSummaryQ lww hash))
                     , hxTarget_ "#repo-tab-data"
                     ] "summary"

            li_ $ a_ [ href_ "#"
                     , hxGet_ (toURL (RepoCommitPatchQ lww hash))
                     , hxTarget_ "#repo-tab-data"
                     ] "patch"

            li_ $ a_ [ href_ (toURL (RepoPage (TreeTab (Just hash)) lww))
                     ] "tree"

  case style of
    RepoCommitSummary -> do

      let msyn = Sky.syntaxByName syntaxMap "default"

      for_ msyn $ \syn -> do

        let config = TokenizerConfig { traceOutput = False, syntaxMap = syntaxMap }

        case tokenize config syn txt of
          Left _ -> mempty
          Right tokens -> do
            let fo = Sky.defaultFormatOpts { Sky.numberLines = False, Sky.ansiColorLevel = Sky.ANSI256Color  }
            let code = renderText (Lucid.formatHtmlBlock fo tokens)
            toHtmlRaw code

    RepoCommitPatch -> do

      let msyn = Sky.syntaxByName syntaxMap "diff"

      for_ msyn $ \syn -> do

        txt <- lift $ getCommitRawPatch lww hash

        let config = TokenizerConfig { traceOutput = False, syntaxMap = syntaxMap }

        case tokenize config syn txt of
          Left _ -> mempty
          Right tokens -> do
            let fo = Sky.defaultFormatOpts { Sky.numberLines = False, Sky.ansiColorLevel = Sky.ANSI256Color  }
            let code = renderText (Lucid.formatHtmlBlock fo tokens)
            toHtmlRaw code


repoForks :: (DashBoardPerks m, MonadReader DashBoardEnv m)
          => LWWRefKey 'HBS2Basic
          -> HtmlT m ()

repoForks lww  = do
  forks <- lift $ selectRepoForks lww
  now <- getEpoch

  unless (List.null forks) do
    table_ $  do
      tr_ $ th_ [colspan_ "3"] mempty
      for_ forks $ \it@RepoListItem{..} -> do
        let lwwTo = coerce @_ @(LWWRefKey 'HBS2Basic) rlRepoLww
        tr_ [class_ "commit-brief-title"] do
          td_ $ svgIcon IconGitFork
          td_ [class_ "mono"] $
            a_ [ href_ (toURL (RepoPage (CommitsTab Nothing) lwwTo))
               ] do
              toHtmlRaw $ view rlRepoLwwAsText it
          td_ $ small_ $ toHtml (agePure rlRepoSeq now)


repoCommits :: (DashBoardPerks m, MonadReader DashBoardEnv m)
            => LWWRefKey 'HBS2Basic
            -> Either SelectCommitsPred SelectCommitsPred
            -> HtmlT m ()

repoCommits lww predicate' = do
  now <- getEpoch

  debug $ red "repoCommits"

  let predicate = either id id predicate'

  co <- lift $ selectCommits lww predicate

  let off = view commitPredOffset predicate
  let lim = view commitPredLimit  predicate
  let noff = off + lim

  let query = RepoCommitsQ lww noff lim --) path ["repo", repo, "commits", show noff, show lim]

  let normalizeText s = l $ (Text.take 60 . Text.unwords . Text.words) s
        where l x | Text.length x < 60 = x
                  | otherwise = x <> "..."

  let rows = do
        tr_ $ th_ [colspan_ "5"] mempty
        for_ co $ \case
          CommitListItemBrief{..} -> do
            tr_ [class_ "commit-brief-title"] do
              td_ [class_ "commit-icon"] $ svgIcon IconGitCommit

              td_ [class_ "commit-hash mono"] do
                  let hash = coerce @_ @GitHash commitListHash
                  a_ [ href_ "#"
                     , hxGet_ (toURL (RepoCommitDefault lww hash))
                     , hxTarget_ "#repo-tab-data"
                     , hxPushUrl_ (toURL query)
                     ] $ toHtml (ShortRef hash)

              td_ [class_ "commit-brief-title"] do
                  toHtml $ normalizeText $ coerce @_ @Text commitListTitle

            tr_ [class_ "commit-brief-details"] do
              td_ [colspan_ "3"] do
                small_ do
                  toHtml (agePure (coerce @_ @Integer commitListTime) now)
                  toHtml " by "
                  toHtml $ coerce @_ @Text commitListAuthor

        unless (List.null co) do
          tr_ [ class_ "commit-brief-last"
              , hxGet_ (toURL query)
              , hxTrigger_ "revealed"
              , hxSwap_ "afterend"
              ] do
            td_ [colspan_ "4"] do
              mempty

  if isRight predicate' then do
    table_ rows
  else do
    rows


repoSomeBlob :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> Text
         -> GitHash
         -> HtmlT m ()

repoSomeBlob lww syn hash = do

  bi <- lift (selectBlobInfo (BlobHash hash))
                      >>= orThrow (itemNotFound hash)

  doRenderBlob (pure mempty) lww bi

repoBlob :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> TreeCommit
         -> TreeTree
         -> BlobInfo
         -> HtmlT m ()

repoBlob lww co tree bi@BlobInfo{..} = do
  locator <- lift $ selectTreeLocator co tree

  table_ [] do
    tr_ do
      td_ [class_ "tree-locator", colspan_ "3"] do
        treeLocator lww (coerce co) locator do
          span_ "/"
          span_ $ toHtml (show $ pretty blobName)


  table_ [class_ "item-attr"] do
    tr_ do
      th_ $ strong_ "hash"
      td_ [colspan_ "7"] do
        span_ [class_ "mono"] $ toHtml $ show $ pretty blobHash

    tr_ do
      th_ $ strong_ "syntax"
      td_ $ toHtml $ show $ pretty blobSyn

      th_ $ strong_ "size"
      td_ $ toHtml $ show $ pretty blobSize

      td_ [colspan_ "3"] mempty

  doRenderBlob (pure mempty) lww bi

doRenderBlob fallback lww BlobInfo{..} = do
  fromMaybe mempty <$> runMaybeT do

    guard (blobSize < 10485760)

    let fn = blobName & coerce
    let syntaxMap = Sky.defaultSyntaxMap

    syn <- ( Sky.syntaxesByFilename syntaxMap fn
               & headMay
           ) <|> Sky.syntaxByName syntaxMap "default"
           & toMPlus

    lift do

      txt <- lift (readBlob lww blobHash)
                <&> LBS.toStrict
                <&> Text.decodeUtf8

      case blobSyn of
        BlobSyn (Just "markdown") -> do

          toHtmlRaw (renderMarkdown' txt)

        _ -> do

          txt <- lift (readBlob lww blobHash)
                    <&> LBS.toStrict
                    <&> Text.decodeUtf8

          let config = TokenizerConfig { traceOutput = False, syntaxMap = syntaxMap }

          case tokenize config syn txt of
            Left _ -> fallback txt
            Right tokens -> do
              let fo = Sky.defaultFormatOpts { Sky.numberLines = False, Sky.ansiColorLevel = Sky.ANSI256Color  }
              let code = renderText (Lucid.formatHtmlBlock fo tokens)
              toHtmlRaw code

raiseStatus :: forall m . MonadIO m => Status -> Text -> m ()
raiseStatus s t = throwIO (StatusError s t)

itemNotFound s = StatusError status404 (Text.pack $ show $ pretty s)

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


newtype H a = H a

instance ToHtml (H FixmeKey) where
  toHtmlRaw (H k) = toHtmlRaw $ take 10 $ show $ pretty k
  toHtml (H k)    = toHtml    $ take 10 $ show $ pretty k

instance ToHtml (H FixmeTag) where
  toHtmlRaw (H k) = toHtmlRaw $ coerce @_ @Text k
  toHtml (H k)    = toHtml    $ coerce @_ @Text k

instance ToHtml (H FixmeTitle) where
  toHtmlRaw (H k) = toHtmlRaw $ coerce @_ @Text k
  toHtml (H k)    = toHtml    $ coerce @_ @Text k

repoFixme :: ( MonadReader DashBoardEnv m
             , DashBoardPerks m
             , HasLimit q
             , HasPredicate q
             , q ~ FromParams 'FixmeDomain [Param]
             )
          => q
          -> LWWRefKey HBS2Basic
          -> HtmlT m ()

repoFixme q@(FromParams p') lww = do

  let p = Map.fromList p'

  now <- liftIO $ getPOSIXTime <&> round

  debug $ blue "repoFixme" <+> "LIMITS" <+> viaShow (limit q)

  let offset = maybe 0 fst (limit q)

  fme <- lift $ listFixme (RepoLww lww) (Reversed q)

  for_ fme $ \fixme -> do
    tr_ [class_ "commit-brief-title"] $ do
      td_ [class_ "mono", width_ "10"] do
         a_ [] $ toHtml (H $ fixmeKey fixme)
      td_ [width_ "10"] do
         strong_ [] $ toHtml (H $ fixmeTag fixme)
      td_ [] do
         toHtml (H $ fixmeTitle fixme)
    tr_ [class_ "commit-brief-details"] $ do
      td_ [colspan_ "3"] do
        mempty

        -- small_ do
        --   for_ (fixmeTs fixme) $ \t0 -> do
        --     toHtml ("updated " <> agePure (fromIntegral t0) now)

  unless (List.null fme) do
    tr_ [ class_ "commit-brief-last"
        , hxGet_ (toURL (Paged (offset + fromIntegral fixmePageSize) (RepoFixmeHtmx p (RepoLww lww))))
        , hxTrigger_ "revealed"
        , hxSwap_ "afterend"
        ] do
      td_ [colspan_ "3"] mempty

data TopInfoBlock =
  TopInfoBlock
  { author      :: Maybe Text
  , public      :: Maybe Text
  , forksNum    :: RepoForks
  , commitsNum  :: RepoCommitsNum
  , manifest    :: Text
  , fixme       :: Maybe MyRefChan
  , fixmeCnt    :: Int
  , pinned      :: [(Text, Syntax C)]
  , repoHeadRef :: RepoHeadRef
  , repoHead    :: Maybe RepoHead
  , repoName    :: RepoName
  }

repoTopInfoBlock :: (MonadIO m, DashBoardPerks m, MonadReader DashBoardEnv m)
                 => LWWRefKey 'HBS2Basic
                 -> TopInfoBlock
                 -> HtmlT m ()

repoTopInfoBlock lww TopInfoBlock{..} = do
  div_ [class_ "info-block" ] do

    summary_ [class_ "sidebar-title"] $ small_ $ strong_ "About"
    ul_ [class_ "mb-0"] do
      for_ author $ \a -> do
        li_ $ small_ do
          "Author: "
          toHtml a

      for_ public $ \p -> do
        li_ $ small_ do
          "Public: "
          toHtml p

      when (Text.length manifest > 100) do
        li_ $ small_ do
          a_ [class_ "secondary", href_ (toURL (RepoPage ManifestTab lww))] do
            span_ [class_ "inline-icon-wrapper"] $ svgIcon IconLicense
            "Manifest"

      for_ fixme $ \_ -> do
        li_ $ small_ do
          a_ [ class_ "secondary"
             , href_ (toURL (RepoPage IssuesTab lww)) ] do
            span_ [class_ "inline-icon-wrapper"] $ svgIcon IconFixme
            toHtml $ show fixmeCnt
            " Issues"

      when (forksNum > 0) do
        li_ $ small_ do
          a_ [class_ "secondary"
            , href_ "#"
            , hxGet_ (toURL (RepoForksHtmx lww))
            , hxTarget_ "#repo-tab-data"
            ] do
              span_ [class_ "inline-icon-wrapper"] $ svgIcon IconGitFork
              toHtml $ show forksNum
              " forks"

      li_ $ small_ do
        a_ [class_ "secondary"
          , href_ (toURL (RepoPage (CommitsTab Nothing) lww))
          ] do
          span_ [class_ "inline-icon-wrapper"] $ svgIcon IconGitCommit
          toHtml $ show commitsNum
          " commits"

      for_ pinned $ \(_,ref) ->  do
        case ref of
          PinnedRefBlob s n hash -> small_ do
            li_ $ a_ [class_ "secondary"
              , href_ (toURL (RepoPage (PinnedTab (Just (s,n,hash))) lww))
              ] do
                  span_ [class_ "inline-icon-wrapper"] $ svgIcon IconPinned
                  toHtml (Text.take 12 n)
                  " "
                  toHtml $ ShortRef hash


getTopInfoBlock lww = do

  debug $ red "getTopInfoBlock"

  it@RepoListItem{..} <- lift (selectRepoList ( mempty
                                  & set repoListByLww (Just lww)
                                  & set repoListLimit (Just 1))
                               <&> listToMaybe
                              ) >>= orThrow (itemNotFound lww)

  sto <- asks _sto
  mhead <- lift $ readRepoHeadFromTx sto (coerce rlRepoTx)

  let repoHead = snd <$> mhead

  (meta, manifest) <- lift $ parsedManifest it

  let author = headMay [ s | ListVal [ SymbolVal "author:", LitStrVal s ] <- meta ]
  let public = headMay [ s | ListVal [ SymbolVal "public:", SymbolVal (Id s) ] <- meta ]
  let pinned = [ (name,r) | ListVal [ SymbolVal "pinned:", r@(PinnedRefBlob _ name _) ] <- meta ] & take 5

  allowed <- lift $ checkFixmeAllowed (RepoLww lww)
  let fixme  = headMay [ x | allowed, FixmeRefChanP x <- meta ]

  fixmeCnt <- if allowed then
                lift (Fixme.countFixme (RepoLww lww))
                  <&> fromMaybe 0
              else
                pure 0

  let forksNum = rlRepoForks
  let commitsNum = rlRepoCommits
  let repoHeadRef = rlRepoHead
  let repoName = rlRepoName

  pure $ TopInfoBlock{..}

repoPage :: (MonadIO m, DashBoardPerks m, MonadReader DashBoardEnv m)
         => RepoPageTabs
         -> LWWRefKey 'HBS2Basic
         -> [(Text,Text)]
         -> HtmlT m ()

repoPage IssuesTab lww p' = rootPage do

  let p = Map.fromList p'

  topInfoBlock@TopInfoBlock{..} <- getTopInfoBlock lww
  tot <- lift $ countFixme (RepoLww lww)
  fmw <- lift $ countFixmeByAttribute (RepoLww lww) "workflow"
  fmt <- lift $ countFixmeByAttribute (RepoLww lww) "fixme-tag"
  ass <- lift $ countFixmeByAttribute (RepoLww lww) "assigned"

  main_ [class_ "container-fluid"] do
    div_ [class_ "wrapper"] do
      aside_ [class_ "sidebar"] do

        div_ [class_ "info-block" ] do
          let url = toURL (RepoPage (CommitsTab Nothing) lww)
          let txt = toHtml (ShortRef lww)
          a_ [href_ url, class_ "secondary"] txt

        repoTopInfoBlock lww topInfoBlock

        div_ [class_ "info-block" ] do

          summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Tag"

          -- FIXME: wtf?

          -- TODO: make-this-block-properly

          ul_ do
            for_ fmt $ \(s,n) -> do
              li_ [] $ small_ [] do
                a_ [ class_ "secondary"
                   , hxGet_ (toURL (Paged 0 (RepoFixmeHtmx (Map.insert "fixme-tag" (coerce s) p) (RepoLww lww))))
                   , hxTarget_ "#fixme-tab-data"
                   ] do
                  span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
                    toHtml $ show $ pretty n

                  span_ [] $ toHtml $ show $ pretty s

          summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Status"

          ul_ do

            li_ [] $ small_ [] do
              a_ [ class_ "secondary"
                 , hxGet_ (toURL (Paged 0 (RepoFixmeHtmx (Map.delete "workflow" p) (RepoLww lww))))
                 , hxTarget_ "#fixme-tab-data"
                 ] do
                span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
                  toHtml $ show $ pretty (fromMaybe 0 tot)

                span_ [] $ toHtml $ show $ pretty "[all]"

            for_ fmw $ \(s,n) -> do
              li_ [] $ small_ [] do
                a_ [ class_ "secondary"
                   , hxGet_ (toURL (Paged 0 (RepoFixmeHtmx (Map.insert "workflow" (coerce s) p) (RepoLww lww))))
                   , hxTarget_ "#fixme-tab-data"
                   ] do
                  span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
                    toHtml $ show $ pretty n

                  span_ [] $ toHtml $ show $ pretty s


          summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Assigned"

          for_ ass $ \(s,n) -> do
            li_ [] $ small_ [] do
              a_ [ class_ "secondary"
                 , hxGet_ (toURL (Paged 0 (RepoFixmeHtmx (Map.insert "assigned" (coerce s) p) (RepoLww lww))))
                 , hxTarget_ "#fixme-tab-data"
                 ] do
                span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
                  toHtml $ show $ pretty n

                span_ [] $ toHtml $ show $ pretty s


      div_ [class_ "content"] $ do

        section_ do
          h5_ $ toHtml (show $ "Issues ::" <+> pretty repoName)

          form_ [role_ "search"] do
            input_ [name_ "search", type_ "search"]
            input_ [type_ "submit", value_ "Search"]

        table_ [] do
          tbody_ [id_ "fixme-tab-data"] mempty

        div_ [ id_ "repo-tab-data"
             , hxTrigger_ "load"
             , hxTarget_ "#fixme-tab-data"
             , hxGet_ (toURL (RepoFixmeHtmx mempty (RepoLww lww)))
             ] mempty

        div_ [id_ "repo-tab-data-embedded"] mempty


repoPage tab lww params = rootPage do

  sto <- asks _sto

  topInfoBlock@TopInfoBlock{..} <- getTopInfoBlock lww

  main_ [class_ "container-fluid"] do
    div_ [class_ "wrapper"] do
      aside_ [class_ "sidebar"] do

        div_ [class_ "info-block" ] do
          let url = toURL (RepoPage (CommitsTab Nothing) lww)
          let txt = toHtml (ShortRef lww)
          a_ [href_ url, class_ "secondary"] txt

        repoTopInfoBlock lww topInfoBlock

        for_ repoHead $ \rh -> do

          let theHead = headMay [ v | (GitRef "HEAD", v) <- view repoHeadRefs rh ]

          let checkHead v what | v == theHead = strong_ what
                               | otherwise    = what

          div_ [class_ "info-block" ] do
            summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Heads"
            ul_ [class_ "mb-0"] $ do
              for_ (view repoHeadHeads rh) $ \(branch,v) -> do
                li_ $ small_ do
                  a_ [class_ "secondary", href_ (toURL (RepoPage (CommitsTab (Just v)) lww ))] do
                    checkHead (Just v) $ toHtml branch

          div_ [class_ "info-block" ] do
            summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Tags"
            ul_ [class_ "mb-0"] $ do
              for_ (view repoHeadTags rh) $ \(tag,v) -> do
                li_ $ small_ do
                  a_ [class_ "secondary", href_ (toURL (RepoPage (CommitsTab (Just v)) lww ))] do
                    checkHead (Just v) $ toHtml tag

      div_ [class_ "content"] $ do

        article_ [class_ "py-0"] $ nav_ [ariaLabel_ "breadcrumb", class_ "repo-menu"] $ ul_ do

          let menuTabClasses isActive = if isActive then "tab contrast" else "tab"
              menuTab t misc name = li_ do
                a_ ([class_ $ menuTabClasses $ isActiveTab tab t] <> misc <> [tabClick]) do
                  name

          menuTab (CommitsTab Nothing)
                         [ href_ "#"
                         , hxGet_ (toURL (RepoCommits lww))
                         , hxTarget_ "#repo-tab-data"
                         ] "commits"

          menuTab (TreeTab Nothing)
                      [ href_ "#"
                      , hxGet_ (toURL (RepoRefs lww))
                      , hxTarget_ "#repo-tab-data"
                      ] "tree"

        section_ do
          strong_ $ toHtml repoName

        div_ [id_ "repo-tab-data"] do

          case tab of

            TreeTab{} -> do

              let tree = [ fromStringMay @GitHash (Text.unpack v)
                         | ("tree", v) <- params
                         ] & catMaybes & headMay

              maybe (repoRefs lww) (\t -> repoTree lww t t) tree

            ManifestTab -> do
              for_ repoHead $ thisRepoManifest

            CommitsTab{} -> do
              let predicate = Right (fromQueryParams params)
              repoCommits lww predicate

            ForksTab -> do
              repoForks lww

            PinnedTab w -> do

              pinned' <- S.toList_ $ for_ pinned $ \(_,ref) ->  case ref of
                              PinnedRefBlob s n hash -> do
                                S.yield (hash, (s,n))

              let pinned = Map.fromList pinned'

              void $ runMaybeT do
                ref  <- [ fromStringMay @GitHash (Text.unpack v)
                        | ("ref", v) <- params
                        ] & catMaybes
                          & headMay
                          & toMPlus

                (s,n) <- Map.lookup ref pinned & toMPlus

                lift $ repoSomeBlob lww s ref

              mempty

        div_ [id_ "repo-tab-data-embedded"] mempty

