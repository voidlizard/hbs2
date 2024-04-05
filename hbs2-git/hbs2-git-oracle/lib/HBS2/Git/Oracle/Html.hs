module HBS2.Git.Oracle.Html where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.State

import HBS2.Git.Oracle.Facts

import HBS2.Peer.HTTP.Root
import HBS2.Peer.Proto.BrowserPlugin

import Data.HashMap.Strict (HashMap)

import Lucid hiding (for_)
import Lucid.Base
import Lucid.Html5 hiding (for_)
import Lucid.Html5 qualified as Html

import Data.Coerce
import Data.Text (Text)
import Data.Maybe
import Data.Text qualified as Text
import Data.Word
import Data.List qualified as List
import Data.HashMap.Strict qualified as HM
import Data.ByteString.Lazy
import Text.Pandoc
import Text.Pandoc.Error (handleError)
import Text.InterpolatedString.Perl6 (qc)



markdownToHtml :: Text -> Either PandocError String
markdownToHtml markdown = runPure $ do
  doc <- readMarkdown def {readerExtensions = pandocExtensions} markdown
  html <- writeHtml5String def {writerExtensions = pandocExtensions} doc
  return $ Text.unpack html

renderMarkdown :: Text -> Html ()
renderMarkdown markdown = case markdownToHtml markdown of
    Left{} -> blockquote_ (toHtml markdown)
    Right html -> toHtmlRaw $ Text.pack html

-- <svg xmlns="http://www.w3.org/2000/svg" class="icon icon-tabler icon-tabler-copy-check" width="44" height="44" viewBox="0 0 24 24" stroke-width="1.5" stroke="#2c3e50" fill="none" stroke-linecap="round" stroke-linejoin="round">
--   <path stroke="none" d="M0 0h24v24H0z" fill="none"/>
--   <path d="M7 7m0 2.667a2.667 2.667 0 0 1 2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 2.667v8.666a2.667 2.667 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 -2.667z" />
--   <path d="M4.012 16.737a2.005 2.005 0 0 1 -1.012 -1.737v-10c0 -1.1 .9 -2 2 -2h10c.75 0 1.158 .385 1.5 1" />
--   <path d="M11 14l2 2l4 -4" />
-- </svg>


-- FIXME: move-to-hbs2-browser-lib
hyper_ :: Text -> Attribute
hyper_  = makeAttribute "_"

tabClick :: Attribute
tabClick =
  hyper_ "on click take .active from .tab for event's target"

-- FIXME: move-to-hbs2-browser-lib
onClickCopy :: Text -> Attribute
onClickCopy s =
  hyper_ [qc|on click writeText('{s}') into the navigator's clipboard add .clicked to me wait 2s remove .clicked from me|]

renderEntries :: Monad m => PluginMethod -> [(HashVal, Text, Text, Word64)] -> m ByteString
renderEntries (Method _ kw) items = pure $ renderBS do

  -- TODO: ugly
  let hrefBase = HM.lookup "URL_PREFIX" kw & List.singleton . maybe "/" Text.unpack

  wrapped do
      main_ do

          section_ do
            h1_ "Git repositories"
            form_ [class_ "search"] do
                input_ [type_ "search", id_ "search"]
                button_ [class_ "search"] mempty


          section_ [id_ "repo-search-results"] do

            for_ items $ \(h,n,b,t) -> do

              let s = if Text.length n > 2 then n else "unnamed"
              let refpart = Text.take 8 $ Text.pack $ show $ pretty h
              let sref = show $ pretty h
              let ref =  Text.pack sref

              let suff = ["repo", sref]

              let url = path (hrefBase <> suff)

              div_ [class_ "repo-list-item"] do
                div_ [class_ "repo-info"] do
                  h2_ [class_ "xclip", onClickCopy ref] $ toHtml (s <> "-" <> refpart)

                  p_ $ a_ [href_ url] (toHtml ref)

                  renderMarkdown b

wrapped :: Monad m => HtmlT m a -> HtmlT m a
wrapped f = do
  doctypehtml_ do
    head_ mempty do
      meta_ [charset_ "utf-8"]

    body_ mempty f


{- HLINT ignore "Eta reduce" -}

-- repoMenu :: Monad m => HtmlT m () -> HtmlT m ()
repoMenu :: Term [Attribute] (t1 -> t2) => t1 -> t2
repoMenu = ul_ []


repoMenuItem0 :: Term [Attribute] (t1 -> t2) => [Attribute] -> t1 -> t2
repoMenuItem0 misc name = li_ ([class_  "tab active"] <> misc <> [tabClick]) name

repoMenuItem :: Term [Attribute] (t1 -> t2) => [Attribute] -> t1 -> t2
repoMenuItem misc name = li_ ([class_  "tab"] <> misc <> [tabClick]) name

renderRepoHtml :: Monad m => PluginMethod -> GitRepoPage -> m ByteString
renderRepoHtml (Method _ kw) page@(GitRepoPage{..}) = pure $ renderBS $ wrapped do

  let mf = headDef "" [ fromMaybe "" s | GitManifest s <- universeBi page ]
            & Text.lines
            & List.dropWhile (not . Text.null)
            & Text.unlines

  let name' = coerce @_ @(Maybe Text) repoPageName
  let brief = coerce @_ @(Maybe Text) repoPageBrief & fromMaybe ""

  let hrefBase = HM.lookup "URL_PREFIX" kw & List.singleton . maybe "/" Text.unpack
                   & path

  main_ do

    -- FIXME: click-on-nav-make-tab-lost-active
    nav_ [ role_ "tab-control" ] do
     repoMenu do
      repoMenuItem  mempty $ a_ [href_ hrefBase] "root"
      repoMenuItem0 mempty "manifest"

    section_ [id_ "repo-data"] do
      for_ name' $ \name -> do
        h1_ (toHtml name)
        renderMarkdown brief

      renderMarkdown mf

