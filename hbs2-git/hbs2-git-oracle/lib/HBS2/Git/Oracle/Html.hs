module HBS2.Git.Oracle.Html where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.State

import Data.HashMap.Strict (HashMap)

import Lucid (Html,HtmlT,toHtml,renderBS)
import Lucid.Html5 hiding (for_)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString.Lazy
import Control.Monad.Identity

renderEntries :: Monad m => HashMap Text Text -> [(HashVal, Text, Text)] -> m ByteString
renderEntries _ items = pure $ renderBS do
  doctypehtml_ do
    head_ mempty do
      meta_ [charset_ "utf-8"]

    body_ mempty do
        for_ items $ \(h,n,b) -> do
          div_ do

            when ( Text.length n > 2) do
              h3_ [class_ "repo-name"] (toHtml (show $ pretty n))
              span_ [class_ "repo-reference"] (toHtml (show $ pretty h))

            -- td_ (toHtml (show $ pretty n))
            -- td_ (toHtml (show $ pretty b))

