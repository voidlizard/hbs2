module Browser.Root
  ( module Lucid
  , browserRootPage
  ) where

import HBS2.Prelude.Plated

import Lucid (Html,HtmlT)
import Lucid.Html5
import Text.InterpolatedString.Perl6 (qc,qq,q)
import Control.Monad
import Control.Monad.Identity

browserRootPage :: Monad m => HtmlT m ()
browserRootPage = do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "hbs2-peer browser"
    link_ [rel_ "stylesheet", href_ "/css/pico.min.css"]
    link_ [rel_ "stylesheet", href_ "/css/custom.css"]

    style_ [type_ "text/css"]  [q|
      .flex-container { display: flex; justify-content: space-around; }
      .flex-item { margin: 10px; padding: 20px; border: 1px solid #ccc; }
      .resource-box { box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); margin: 20px; padding: 20px; border-radius: 8px; }
      h2 { font-size: 1.00rem; };
      .container header h2 { font-color: red; }
      |]

  body_ $ do
    div_ [class_ "container"] $ do

      header_ $ do
        h1_ "hbs2-peer browser"

      main_ $ do

        replicateM_ 6 do

          div_ [class_ "resource-box"] $ do
            h2_ "Metadata channel"
            div_ do
              a_ [href_ "/wtf"] "5GnroAC8FXNRL8rcgJj6RTu9mt1AbuNd5MZVnDBcCKzb"
              p_ "some-shitty-wtf"

      footer_ "Это подвал страницы."

