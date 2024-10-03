module HBS2.Git.Web.Html.Markdown where

import HBS2.Git.DashBoard.Prelude
import Data.Text qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)

import Text.Pandoc hiding (getPOSIXTime)

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
