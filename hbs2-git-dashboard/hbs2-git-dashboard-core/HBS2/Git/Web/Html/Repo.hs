{-# Language MultiWayIf #-}
module HBS2.Git.Web.Html.Repo where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.State.Commits
import HBS2.Git.DashBoard.Manifest

import HBS2.OrDie

import HBS2.Git.Data.Tx.Git
import HBS2.Git.Data.RepoHead
import HBS2.Git.Web.Assets

import HBS2.Git.Web.Html.Types
import HBS2.Git.Web.Html.Root
import HBS2.Git.Web.Html.Markdown
import HBS2.Git.Web.Html.Parts.Issues.Sidebar


import Data.Map qualified as Map
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
import Data.Either
import Data.List qualified as List
import Data.List (sortOn)

import Streaming.Prelude qualified as S

isActiveTab :: RepoPageTabs -> RepoPageTabs -> Bool
isActiveTab a b = case (a,b)  of
  (CommitsTab{},CommitsTab{}) -> True
  (ManifestTab{},ManifestTab{}) -> True
  (TreeTab{},TreeTab{}) -> True
  _ -> False



repoPage :: (MonadIO m, DashBoardPerks m, MonadReader DashBoardEnv m)
         => RepoPageTabs
         -> LWWRefKey 'HBS2Basic
         -> [(Text,Text)]
         -> HtmlT m ()

repoPage IssuesTab lww p' = rootPage do

  ti@TopInfoBlock{..} <- lift $ getTopInfoBlock lww

  main_ [class_ "container-fluid"] do
    div_ [class_ "wrapper"] do
      aside_ [class_ "sidebar"] do

        issuesSidebar lww ti p'

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

  topInfoBlock@TopInfoBlock{..} <- lift $ getTopInfoBlock lww

  main_ [class_ "container-fluid"] do
    div_ [class_ "wrapper"] do
      aside_ [class_ "sidebar"] do


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


thisRepoManifest :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoHead -> HtmlT m ()
thisRepoManifest rh = do
  (_, man) <- lift $ parseManifest rh
  div_ [class_ "lim-text"] $ toHtmlRaw (renderMarkdown' man)

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

          div_ [class_ "lim-text"] do
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



