module HBS2.Git.Oracle.Html where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.State

import Data.HashMap.Strict (HashMap)

import Lucid (Html,HtmlT,toHtml,toHtmlRaw,renderBS)
import Lucid.Html5 hiding (for_)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import Data.ByteString.Lazy
import Text.Pandoc
import Text.Pandoc.Error (handleError)


markdownToHtml :: Text -> Either PandocError String
markdownToHtml markdown = runPure $ do
  doc <- readMarkdown def {readerExtensions = pandocExtensions} markdown
  html <- writeHtml5String def {writerExtensions = pandocExtensions} doc
  return $ Text.unpack html

renderMarkdown :: Text -> Html ()
renderMarkdown markdown = case markdownToHtml markdown of
    Left{} -> mempty
    Right html -> toHtmlRaw $ Text.pack html


renderEntries :: Monad m => HashMap Text Text -> [(HashVal, Text, Text, Word64)] -> m ByteString
renderEntries _ items = pure $ renderBS do
  doctypehtml_ do
    head_ mempty do
      meta_ [charset_ "utf-8"]

    body_ mempty do
        for_ items $ \(h,n,b,t) -> do
          div_ do

            when ( Text.length n > 2) do
              h3_ [class_ "repo-name"] (toHtml (show $ pretty n))
              div_ [class_ "repo-reference"] (toHtml (show $ pretty h))
              div_ [class_ "repo-brief"] do
                renderMarkdown b

