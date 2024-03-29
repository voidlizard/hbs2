module HBS2.Git.Oracle.Html where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.State

import Data.HashMap.Strict (HashMap)

import Lucid (Html,HtmlT,toHtml,toHtmlRaw,renderBS)
import Lucid.Html5 hiding (for_)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import Data.HashMap.Strict qualified as HM
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
renderEntries args items = pure $ renderBS do
  wrapped do
    for_ items $ \(h,n,b,t) -> do
      div_ [class_ "resource-box"] do

        let name = if Text.length n > 2 then toHtml n else toHtml (show $ pretty h)

        h3_ [class_ "repo-name"] name

        div_ [class_ "repo-brief"] do
          renderMarkdown b

        div_ [class_ "repo-reference"] $ a_ [] (toHtml (show $ pretty h))

  where

    wrapped f | not (HM.member "HTML_WRAPPED" args) = div_ f
              | otherwise = do
      doctypehtml_ do
        head_ mempty do
          meta_ [charset_ "utf-8"]

        body_ mempty f

