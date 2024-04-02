module HBS2.Git.Oracle.Html where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.State

import Data.HashMap.Strict (HashMap)

import Lucid hiding (for_)
import Lucid.Base
import Lucid.Html5 hiding (for_)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
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
    Left{} -> mempty
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

-- FIXME: move-to-hbs2-browser-lib
onClickCopy :: Text -> Attribute
onClickCopy s =
  hyper_ [qc|on click writeText('{s}') into the navigator's clipboard add .clicked to me wait 2s remove .clicked from me|]

renderEntries :: Monad m => HashMap Text Text -> [(HashVal, Text, Text, Word64)] -> m ByteString
renderEntries args items = pure $ renderBS do
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
              let ref =  Text.pack $ show $ pretty h

              div_ [class_ "repo-list-item"] do
                div_ [class_ "repo-info"] do
                  h2_ [class_ "xclip", onClickCopy ref] $ toHtml (s <> "-" <> refpart)

                  p_ $ a_ [href_ ""] (toHtml (show $ pretty h))

                  renderMarkdown b

  where

    -- wrapped f | not (HM.member "HTML_WRAPPED" args) = div_ f
    --           | otherwise = do
      wrapped f = do
        doctypehtml_ do
          head_ mempty do
            meta_ [charset_ "utf-8"]

          body_ mempty f
