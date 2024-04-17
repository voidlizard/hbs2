module HBS2.Git.Web.Html.Root where

import HBS2.Prelude
import HBS2.Base58
import HBS2.Peer.Proto.RefChan.Types

import Data.Config.Suckless

import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)
import System.FilePath
import Text.InterpolatedString.Perl6 (q)

rootPath :: [String] -> [String]
rootPath = ("/":)

path :: [String] -> Text
path = Text.pack . joinPath . rootPath

myCss :: Monad m => HtmlT m ()
myCss = do
  link_ [rel_ "stylesheet", href_ (path ["css/custom.css"])]

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
        div_ [class_ "header-title"] $ h1_ "hbs2-peer dashboard"
      content



dashboardRootPage :: Monad m => [Syntax c] -> HtmlT m ()
dashboardRootPage syn = rootPage do

  let channels = mempty
  -- [ mchan | ListVal (SymbolVal "channel" : mchan) <- bro ]

  div_ [class_ "container main"] $ do
    nav_ [class_ "left"] $ do
      div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"
      div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"

    main_ do
      for_ channels $ \chan -> void $ runMaybeT do

        let title = headDef "unknown" [ t
                                          | ListVal [ SymbolVal "title", LitStrVal t ] <- chan
                                          ]
        let desc = mconcat [ d
                           | ListVal (SymbolVal "description" : d) <- chan
                           ] & take 5

        rchan <- headMay ( catMaybes
                       [ fromStringMay @(RefChanId L4Proto) (Text.unpack rc)
                       | ListVal [SymbolVal "refchan", LitStrVal rc] <- chan
                       ] ) & toMPlus


        let alias = headMay [ x
                            | ListVal [SymbolVal "alias", LitStrVal x] <- chan
                            ]

        let url = case alias of
                    Just x -> Text.unpack x
                    Nothing -> (show . pretty . AsBase58) rchan

        lift do
          div_ [class_ "channel-list-item"] do
            h2_ $ toHtml title

            p_ $ a_ [href_ (path [url])] (toHtml (show $ pretty $ AsBase58 rchan))

            for_ [ s | LitStrVal s <- desc ] $ \s -> do
              p_ (toHtml s)

