{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language OverloadedStrings #-}
module HBS2.Git.Web.Html.Root where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State

import HBS2.Git.Data.Tx.Git
import HBS2.Git.Data.RepoHead

import Data.Text qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)
import Lucid.Htmx

import Control.Applicative
import Text.Pandoc hiding (getPOSIXTime)
import System.FilePath
import Data.Word
import Data.Either
import Safe
import Data.List (sortOn)
import Data.Ord (comparing, Down(..))


import Streaming.Prelude qualified as S

rootPath :: [String] -> [String]
rootPath = ("/":)

path :: [String] -> Text
path = Text.pack . joinPath . rootPath

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

instance ToHtml (WithTime RepoListItem) where
  toHtmlRaw = pure mempty

  toHtml (WithTime t it@RepoListItem{..}) = do

    let now = t

    let locked = isJust $ coerce @_ @(Maybe HashRef) rlRepoGK0

    let url = path ["repo", Text.unpack $ view rlRepoLwwAsText it]
    let t = fromIntegral $ coerce @_ @Word64 rlRepoSeq

    let updated = "" <+> d
          where
            sec = now - t
            d | sec > 86400  = pretty (sec `div` 86400)  <+> "days ago"
              | sec > 3600   = pretty (sec `div` 3600)   <+> "hours ago"
              | otherwise    = pretty (sec `div` 60)     <+> "minutes ago"

    div_ [class_ "repo-list-item"] do
      div_ [class_ "repo-info", style_ "flex: 1; flex-basis: 70%;"] do

        h2_ [class_ "xclip", onClickCopy (view rlRepoLwwAsText it)] $ toHtml rlRepoName
        p_ $ a_ [href_ url] (toHtml $ view rlRepoLwwAsText it)

        toHtml rlRepoBrief

      div_ [ ] do
        div_ [ class_ "attr" ] do
          div_ [ class_ "attrname"]  (toHtml $ show updated)

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


repoManifest :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoListItem -> HtmlT m ()
repoManifest it@RepoListItem{..} = do
  (_, manifest) <- lift $ parsedManifest it
  toHtmlRaw (renderMarkdown' manifest)

repoRefs :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> [(GitRef, GitHash)]
         -> HtmlT m ()
repoRefs lww refs = do
  table_ [] do
    for_ refs $ \(r,h) -> do
      let co = show $ pretty h
      let uri = path [ "repo", show $ pretty lww, "tree", co, co ]
      tr_ mempty do
        td_ mempty (toHtml $ show $ pretty r)
        td_ [class_ "mono"] $ a_ [ href_ "#"
                                 , hxGet_ uri
                                 , hxTarget_ "#repo-tab-data"
                                 ] (toHtml $ show $ pretty h)


repoTree :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> GitHash -- ^ this
         -> GitHash -- ^ this
         -> [(GitObjectType, GitHash, Text)]
         -> Maybe GitHash -- ^ back
         -> HtmlT m ()

repoTree lww co root tree back' = do

  let co_ = show $ pretty co

  let sorted = sortOn (\(tp, _, name) -> (tpOrder tp, name)) tree
        where
          tpOrder Tree = (0 :: Int)
          tpOrder Blob = 1
          tpOrder _    = 2

  table_ [] do
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
          Blob -> img_ [src_ "/icon/blob.svg"]
          Tree -> img_ [src_ "/icon/tree.svg"]
          _    -> mempty

        td_ [class_ itemClass] (toHtml $ show $ pretty name)
        td_ [class_ "mono"] do
          case tp of
            Blob -> do
              span_ do
                toHtml $ show $ pretty h

            Tree -> do
              a_ [ href_ "#"
                 , hxGet_ uri
                 , hxTarget_ "#repo-tab-data"
                 -- , hxPushUrl_ "true"
                 ] (toHtml $ show $ pretty h)

            _ -> mempty


repoPage :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoListItem -> HtmlT m ()
repoPage it@RepoListItem{..} = rootPage do

  let repo = show $ pretty rlRepoLww

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

        repoMenuItem0 [ hxGet_ (path ["repo", repo, "manifest"])
                      , hxTarget_ "#repo-tab-data"
                      ] "manifest"

        repoMenuItem  [
                      ] "commits"

        repoMenuItem  [ hxGet_ (path ["repo", repo, "refs"])
                      , hxTarget_ "#repo-tab-data"
                      ] "tree"

      section_ [id_ "repo-data"] do
        h1_ (toHtml $ rlRepoName)

      div_ [id_ "repo-tab-data"] do
        toHtmlRaw (renderMarkdown' manifest)

