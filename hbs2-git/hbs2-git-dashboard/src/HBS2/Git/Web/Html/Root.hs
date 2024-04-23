{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language MultiWayIf #-}
module HBS2.Git.Web.Html.Root where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.State.Commits

import HBS2.OrDie

import HBS2.Git.Data.Tx.Git
import HBS2.Git.Data.RepoHead

-- import Data.Text.Fuzzy.Tokenize as Fuzz

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

import Data.Kind

import Streaming.Prelude qualified as S

import Network.HTTP.Types.Status

rootPath :: [String] -> [String]
rootPath = ("/":)

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
                    deriving stock (Eq,Ord,Show)

data RepoPage s a = RepoPage s a

data RepoRefs repo = RepoRefs repo

data RepoTree repo commit tree = RepoTree repo commit tree

data RepoTreeEmbedded repo commit tree = RepoTreeEmbedded repo commit tree

data RepoBlob repo commit tree blob = RepoBlob repo commit tree blob

newtype RepoManifest repo = RepoManifest repo

newtype RepoCommits repo = RepoCommits repo

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

instance FromStringMaybe RepoPageTabs where
  fromStringMay = \case
    "commits"  -> pure (CommitsTab Nothing)
    "manifest" -> pure ManifestTab
    "tree"     -> pure (TreeTab Nothing)
    _          -> pure (CommitsTab Nothing)

instance ToRoutePattern RepoListPage where
  routePattern = \case
    RepoListPage -> "/"

instance ToURL (RepoPage RepoPageTabs (LWWRefKey 'HBS2Basic)) where
  toURL (RepoPage s w)  = path @String [ "/", show (pretty s), show (pretty w)]
                            <> pred_
    where
     pred_ = case s of
      CommitsTab (Just p) -> Text.pack $ "?ref=" <> show (pretty p)
      TreeTab (Just p)    -> Text.pack $ "?tree=" <> show (pretty p)
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


myCss :: Monad m => HtmlT m ()
myCss = do
  link_ [rel_ "stylesheet", href_ (path ["css/custom.css"])]

hyper_ :: Text -> Attribute
hyper_  = makeAttribute "_"


onClickCopy :: Text -> Attribute
onClickCopy s =
  hyper_ [qc|on click writeText('{s}') into the navigator's clipboard add .clicked to me wait 2s remove .clicked from me|]

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

instance ToHtml (WithTime RepoListItem) where
  toHtmlRaw = pure mempty

  toHtml (WithTime t it@RepoListItem{..}) = do

    let now = t

    let locked = isJust $ coerce @_ @(Maybe HashRef) rlRepoGK0

    let url = toURL (RepoPage (CommitsTab Nothing) (coerce @_ @(LWWRefKey 'HBS2Basic) rlRepoLww))
    -- path ["repo", Text.unpack $ view rlRepoLwwAsText it]
    let t = fromIntegral $ coerce @_ @Word64 rlRepoSeq

    let updated = agePure t now

    div_ [class_ "repo-list-item"] do
      div_ [class_ "repo-info", style_ "flex: 1; flex-basis: 70%;"] do

        h2_ [class_ "xclip", onClickCopy (view rlRepoLwwAsText it)] $ toHtml rlRepoName
        p_ $ a_ [href_ url] (toHtml $ view rlRepoLwwAsText it)

        toHtml rlRepoBrief

      div_ [ ] do
        div_ [ class_ "attr" ] do
          div_ [ class_ "attrname"]  (toHtml updated)

        when locked do
          div_ [ class_ "attr" ] do
            div_ [ class_ "attrval icon"] do
              img_ [src_ "/icon/lock-closed.svg"]

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
      header_ do
        div_ [class_ "header-title"] $ h1_ [] $ a_ [href_ (toURL RepoListPage)] "hbs2-peer dashboard"
      content



dashboardRootPage :: (DashBoardPerks m, MonadReader DashBoardEnv m) => HtmlT m ()
dashboardRootPage = rootPage do

  items <- lift $ selectRepoList mempty

  now  <- liftIO getPOSIXTime <&> fromIntegral . round

  div_ [class_ "container main"] $ do
    nav_ [class_ "left"] $ do
      div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"
      div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"

    main_ do

      section_ do
        h1_ "Git repositories"
        form_ [class_ "search"] do
            input_ [type_ "search", id_ "search"]
            button_ [class_ "search"] mempty


      section_ [id_ "repo-search-results"] do

        for_ items $ \item@RepoListItem{..} -> do
          toHtml (WithTime now item)



tabClick :: Attribute
tabClick =
  hyper_ "on click take .active from .tab for event's target"

-- repoMenu :: Monad m => HtmlT m () -> HtmlT m ()
repoMenu :: Term [Attribute] (t1 -> t2) => t1 -> t2
repoMenu = ul_ []


repoMenuItem0 :: Term [Attribute] (t1 -> t2) => [Attribute] -> t1 -> t2
repoMenuItem0 misc name = li_ ([class_  "tab active"] <> misc <> [tabClick]) name

repoMenuItem :: Term [Attribute] (t1 -> t2) => [Attribute] -> t1 -> t2
repoMenuItem misc name = li_ ([class_  "tab"] <> misc <> [tabClick]) name


parsedManifest :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoListItem  -> m ([Syntax C], Text)
parsedManifest RepoListItem{..} = do

  sto <- asks _sto
  mhead <- readRepoHeadFromTx sto (coerce rlRepoTx)

  let rawManifest  = (_repoManifest . snd =<< mhead)
                      & fromMaybe (coerce rlRepoBrief)
                      & Text.lines

  w <- S.toList_ do
         flip fix rawManifest $ \next ss -> do
          case ss of
            ( "" : rest )  -> S.yield (Right (Text.stripStart (Text.unlines rest)))
            ( a : rest )   -> S.yield (Left a ) >> next rest
            [] -> pure ()

  let meta = Text.unlines (lefts w)
                & Text.unpack
                & parseTop
                & fromRight mempty

  let manifest = mconcat $ rights  w

  pure (meta, manifest)


thisRepoManifest :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoListItem -> HtmlT m ()
thisRepoManifest it@RepoListItem{..} = do
  (_, manifest) <- lift $ parsedManifest it
  toHtmlRaw (renderMarkdown' manifest)

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
                  img_ [src_ "/icon/git-branch.svg"]
               | Text.isPrefixOf "refs/tags" r_ -> do
                  img_ [src_ "/icon/git-tag.svg"]
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
            td_ $ img_ [src_ "/icon/tree-up.svg"]
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
          Tree   -> img_ [src_ "/icon/tree.svg"]
          Blob   -> do
            let syn = Sky.syntaxesByFilename syntaxMap (Text.unpack name)
                        & headMay
                        <&> Text.toLower . Sky.sName

            let icon = case syn of
                         Just "haskell"    -> [src_ "/icon/haskell.svg"]
                         Just "markdown"   -> [src_ "/icon/markdown.svg"]
                         Just "nix"        -> [src_ "/icon/nixos.svg"]
                         Just "bash"       -> [src_ "/icon/terminal.svg"]
                         Just "python"     -> [src_ "/icon/python.svg"]
                         Just "javascript" -> [src_ "/icon/javascript.svg"]
                         Just "sql"        -> [src_ "/icon/sql.svg"]
                         Just s | s `elem` ["cabal","makefile","toml","ini","yaml"]
                                           -> [src_ "/icon/gear.svg"]
                         _                 -> [src_ "/icon/blob-filled.svg"]

            img_ ([alt_ (fromMaybe "blob" syn)] <> icon)

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

repoCommits :: (DashBoardPerks m, MonadReader DashBoardEnv m)
            => LWWRefKey 'HBS2Basic
            -> Either SelectCommitsPred SelectCommitsPred
            -> HtmlT m ()

repoCommits lww predicate' = do
  now <- getEpoch

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
        for_ co $ \case
          CommitListItemBrief{..} -> do
            tr_ [class_ "commit-brief-title"] do
              td_ [class_ "commit-icon"] $ img_ [src_ "/icon/git-commit.svg"]

              td_ [class_ "commit-hash mono"] do
                  let hash = coerce @_ @GitHash commitListHash
                  a_ [ href_ "#"
                     , hxGet_ (toURL (RepoCommitDefault lww hash))
                     , hxTarget_ "#repo-tab-data"
                     , hxPushUrl_ (toURL query)
                     ] (toHtml $ take 10 (show $ pretty hash) <> "..")

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

repoBlob :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> TreeCommit
         -> TreeTree
         -> BlobInfo
         -> HtmlT m ()

repoBlob lww co tree BlobInfo{..} = do
  locator <- lift $ selectTreeLocator co tree

  let repo = show $ pretty lww
  let co_ = show $ pretty co
  let tree_ = show $ pretty tree

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


  let fallback _ = mempty


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

repoPage :: (MonadIO m, DashBoardPerks m, MonadReader DashBoardEnv m)
         => RepoPageTabs
         -> LWWRefKey 'HBS2Basic
         -> [(Text,Text)]
         -> HtmlT m ()
repoPage tab lww params = rootPage do

  it@RepoListItem{..} <- lift (selectRepoList ( mempty
                                  & set repoListByLww (Just lww)
                                  & set repoListLimit (Just 1))
                               <&> listToMaybe
                              ) >>= orThrow (itemNotFound lww)

  sto <- asks _sto
  mhead <- lift $ readRepoHeadFromTx sto (coerce rlRepoTx)

  (meta, manifest) <- lift $ parsedManifest it

  let author = headMay [ s | ListVal [ SymbolVal "author:", LitStrVal s ] <- meta ]
  let public = headMay [ s | ListVal [ SymbolVal "public:", SymbolVal (Id s) ] <- meta ]

  div_ [class_ "container main"] $ do
    nav_ [class_ "left"] $ do

      div_ [class_ "info-block" ] do
        for_ author $ \a -> do
            div_ [ class_ "attr" ] do
              div_ [ class_ "attrname"] "author:"
              div_ [ class_ "attrval"] $ toHtml a

        for_ public $ \p -> do
            div_ [ class_ "attr" ] do
              div_ [ class_ "attrname"] "public:"
              div_ [ class_ "attrval"] $ toHtml p

      div_ [class_ "info-block" ] do
        for_ (snd <$> mhead) $ \rh -> do
          h6_ [] "heads"
          for_ (view repoHeadHeads rh) $ \(branch,v) -> do
            div_ [ class_ "attrval onleft"] do
              a_ [ href_ (toURL (RepoPage (CommitsTab (Just v))  lww ))
                 ] $ toHtml branch

      div_ [class_ "info-block" ] do
        for_ (snd <$> mhead) $ \rh -> do
          h6_ [] "tags"
          for_ (view repoHeadTags rh) $ \(tag,v) -> do
            div_ [ class_ "attrval onleft"] do
              a_ [href_ (toURL (RepoPage (CommitsTab (Just v)) lww ))] $ toHtml tag

    main_ do

      nav_ [ role_ "tab-control" ] do
       repoMenu do
        repoMenuItem  mempty $ a_ [href_ "/"] "root"

        let menu t = if isActiveTab tab t then repoMenuItem0 else repoMenuItem

        menu (CommitsTab Nothing)
                       [ hxGet_ (toURL (RepoCommits lww))
                       , hxTarget_ "#repo-tab-data"
                       ] "commits"

        menu ManifestTab [ hxGet_ (toURL (RepoManifest lww))
                        , hxTarget_ "#repo-tab-data"
                        ] "manifest"

        menu (TreeTab Nothing)
                    [ hxGet_ (toURL (RepoRefs lww))
                    , hxTarget_ "#repo-tab-data"
                    ] "tree"

      section_ [id_ "repo-data"] do
        h1_ (toHtml $ rlRepoName)

      div_ [id_ "repo-tab-data"] do

        case tab of

          TreeTab{} -> do

            let tree = [ fromStringMay @GitHash (Text.unpack v)
                       | ("tree", v) <- params
                       ] & catMaybes & headMay

            maybe (repoRefs lww) (\t -> repoTree lww t t) tree

          ManifestTab -> do
            thisRepoManifest it

          CommitsTab{} -> do
            let predicate = Right (fromQueryParams params)
            repoCommits lww predicate

      div_ [id_ "repo-tab-data-embedded"] mempty


