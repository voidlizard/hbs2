{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language MultiWayIf #-}
module HBS2.Git.Web.Html.Root where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.State.Commits

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


import Streaming.Prelude qualified as S

data ViewContext =
  ViewContext
  { _baseUri :: String
  , _tab     :: Text
  }
  deriving stock Generic

instance Serialise ViewContext

rootPath :: [String] -> [String]
rootPath = ("/":)

path :: [String] -> Text
path = Text.pack . joinPath . rootPath

myCss :: Monad m => HtmlT m ()
myCss = do
  link_ [rel_ "stylesheet", href_ (path ["css/custom.css"])]

hyper_ :: Text -> Attribute
hyper_  = makeAttribute "_"

-- makeGetQuery :: String -> Attribute
-- makeGetQuery _ = termRaw "jop"

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

instance ToHtml (WithTime RepoListItem) where
  toHtmlRaw = pure mempty

  toHtml (WithTime t it@RepoListItem{..}) = do

    let now = t

    let locked = isJust $ coerce @_ @(Maybe HashRef) rlRepoGK0

    let url = path ["repo", Text.unpack $ view rlRepoLwwAsText it]
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
        div_ [class_ "header-title"] $ h1_ [] $ a_ [href_ "/"] "hbs2-peer dashboard"
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
         -> [(GitRef, GitHash)]
         -> HtmlT m ()
repoRefs lww refs = do
  table_ [] do
    for_ refs $ \(r,h) -> do
      let r_ = Text.pack $ show $ pretty r
      let co = show $ pretty h
      let uri = path [ "repo", show $ pretty lww, "tree", co, co ]

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


showRefsHtmxAttribs :: String -> [Attribute]
showRefsHtmxAttribs repo =
  [ hxGet_ (path ["repo", repo, "refs"])
  , hxTarget_ "#repo-tab-data"
  ]


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
        [ hxGet_ (path ["repo", repo, "tree", co_, co_])
        , hxTarget_ "#repo-tab-data"
        , href_ "#"
        ]

  span_ [] $ a_ (showRefsHtmxAttribs repo <> [href_ "#" ]) $ toHtml (take 10 repo <> "..")
  span_ [] "/"
  span_ [] $ a_ showRoot $ toHtml (take 10 co_ <> "..")
  unless (List.null locator) do
    span_ [] "/"
    for_ locator $ \(_,this,level,name) -> do
      prefixSlash level
      let uri = path [ "repo", show $ pretty lww, "tree", co_, show (pretty this) ]
      span_ [] do
        a_ [ href_ "#"
           , hxGet_ uri
           , hxTarget_ "#repo-tab-data"
           ] (toHtml (show $ pretty name))
  next

repoTree :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => ViewContext
         -> LWWRefKey 'HBS2Basic
         -> GitHash -- ^ this
         -> GitHash -- ^ this
         -> [(GitObjectType, GitHash, Text)]
         -> Maybe GitHash -- ^ back
         -> HtmlT m ()

repoTree ctx lww co root tree back' = do

  let repo = show $ pretty $ lww

  let syntaxMap = Sky.defaultSyntaxMap

  let co_ = show $ pretty co
  let this_ = show $ pretty $ root

  let sorted = sortOn (\(tp, _, name) -> (tpOrder tp, name)) tree
        where
          tpOrder Tree = (0 :: Int)
          tpOrder Blob = 1
          tpOrder _    = 2

  let wtf = show $ pretty $ AsBase58 (serialise ctx)

  locator <- lift $ selectTreeLocator (TreeCommit co) (TreeTree root)

  table_ [] do

    tr_ do
      td_ [class_ "tree-locator", colspan_ "3"] do
        treeLocator lww co locator none

    tr_ mempty do

      for_ back' $ \root -> do
        let rootLink = path [ "repo", show $ pretty lww, "tree", co_, show (pretty root) ]
        td_ $ img_ [src_ "/icon/tree-up.svg"]
        td_ ".."
        td_ do a_ [ href_ "#"
                  , hxGet_ rootLink
                  , hxTarget_ "#repo-tab-data"
                  ] (toHtml $ show $ pretty root)

    for_ sorted $ \(tp,h,name) -> do
      let itemClass = pretty tp & show & Text.pack
      let hash_ = show $ pretty h
      let uri = path [ "repo", show $ pretty lww, "tree", co_, hash_ ]
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
              let blobUri = path ["repo", repo, "blob", co_, this_, hash_ ]
              a_ [ href_ "#"
                 , hxGet_ blobUri
                 , hxTarget_ "#repo-tab-data"
                 ] (toHtml hash_)

            Tree -> do
              a_ [ href_ "#"
                 , hxGet_ uri
                 , hxTarget_ "#repo-tab-data"
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

  let repo = show $ pretty lww
  let co_ = show $ pretty hash
  let root = co_

  txt <- lift $ getCommitRawBrief lww hash

  let header = Text.lines txt & takeWhile (not . Text.null)
                              & fmap Text.words

  let au = [ Text.takeWhile (/= '<') (Text.unwords a)
           | ("Author:" : a) <- header
           ] & headMay

  table_ [class_ "item-attr"] do

    tr_ do
      th_ [width_ "16rem"] $ strong_ "commit"
      td_ $ a_ [ href_ "#"
               , hxGet_ (path [ "repo", show $ pretty lww, "tree", co_, co_ ])
               , hxTarget_ "#repo-tab-data"
               ] $ toHtml $ show $ pretty hash

    for_ au $ \author -> do
      tr_ do
        th_ $ strong_ "author"
        td_ $ toHtml  author

    tr_ $ do
      th_ $ strong_ "view"
      td_ do
        ul_ [class_ "misc-menu"]do
          unless (style == RepoCommitSummary ) do
            li_ $ a_ [ href_ "#"
                     , hxGet_ (path ["repo", repo, "commit", "summary", co_])
                     , hxTarget_ "#repo-tab-data"
                     ] "summary"
          unless (style == RepoCommitPatch ) do
            li_ $ a_ [ href_ "#"
                     , hxGet_ (path ["repo", repo, "commit", "patch", co_])
                     , hxTarget_ "#repo-tab-data"
                     ] "patch"

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
  let repo = show $ pretty lww

  let predicate = either id id predicate'

  co <- lift $ selectCommits lww predicate

  let off = view commitPredOffset predicate
  let lim = view commitPredLimit  predicate
  let noff = off + lim

  let query = path ["repo", repo, "commits", show noff, show lim]

  let rows = do
        for_ co $ \case
          CommitListItemBrief{..} -> do
            tr_ [class_ "commit-brief-title"] do
              td_ $ img_ [src_ "/icon/git-commit.svg"]
              td_ $ small_ $ toHtml (agePure (coerce @_ @Integer commitListTime) now)
              td_ [class_ "mono", width_ "20rem"] do
                  let hash = show $ pretty $ coerce @_ @GitHash commitListHash
                  a_ [ href_ "#"
                     , hxGet_ (path ["repo",repo,"commit",hash])
                     , hxTarget_ "#repo-tab-data"
                     , hxPushUrl_ query
                     ] (toHtml hash)
              td_ do
                small_ $ toHtml $ coerce @_ @Text commitListAuthor
            tr_ [class_ "commit-brief-details"] do
              td_ [colspan_ "1"] mempty
              td_ [colspan_ "3", class_ "commit-brief-title"] do
                  small_ $ toHtml $ coerce @_ @Text commitListTitle

        unless (List.null co) do
          tr_ [ class_ "commit-brief-last"
              , hxGet_ query
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


repoPage :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoListItem -> HtmlT m ()
repoPage it@RepoListItem{..} = rootPage do

  let lww = rlRepoLww & coerce
  let repo = show $ pretty lww

  sto <- asks _sto
  mhead <- lift $ readRepoHeadFromTx sto (coerce rlRepoTx)

  (meta, manifest) <- lift $ parsedManifest it

  debug $ yellow "HEAD" <+> pretty rlRepoTx
  debug $ yellow "META" <+> pretty meta

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
          for_ (view repoHeadHeads rh) $ \branch -> do
            div_ [ class_ "attrval onleft"] $ toHtml branch

      div_ [class_ "info-block" ] do
        for_ (snd <$> mhead) $ \rh -> do
          h6_ [] "tags"
          for_ (view repoHeadTags rh) $ \tag -> do
            div_ [ class_ "attrval onleft"] $ toHtml tag

    main_ do

      nav_ [ role_ "tab-control" ] do
       repoMenu do
        repoMenuItem  mempty $ a_ [href_ "/"] "root"

        repoMenuItem0 [ hxGet_ (path ["repo", repo, "commits"])
                      , hxTarget_ "#repo-tab-data"
                      ] "commits"

        repoMenuItem [ hxGet_ (path ["repo", repo, "manifest"])
                     , hxTarget_ "#repo-tab-data"
                     ] "manifest"

        repoMenuItem (showRefsHtmxAttribs repo) "tree"

      section_ [id_ "repo-data"] do
        h1_ (toHtml $ rlRepoName)

      div_ [id_ "repo-tab-data"] do
        let predicate = Right mempty
        repoCommits lww predicate

