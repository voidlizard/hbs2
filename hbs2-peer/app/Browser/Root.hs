module Browser.Root
  ( module Lucid
  , browserRootPage
  , pluginPage
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Net.Proto.Types
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.Proto.BrowserPlugin

import Data.Config.Suckless.Syntax

import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Lucid (Html,HtmlT,toHtml,toHtmlRaw)
import Lucid.Html5 hiding (for_)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding as TLE
import Data.Text.Lazy qualified as TL
import Text.InterpolatedString.Perl6 (q)
import Data.ByteString.Lazy.Char8 qualified as LBS
import System.FilePath
import Control.Monad
import Control.Monad.Trans.Maybe
import Lens.Micro.Platform

import Text.HTML.TagSoup

import UnliftIO

rootPath :: [String] -> [String]
rootPath = ("/browser":)

path :: [String] -> Text
path = Text.pack . joinPath . rootPath

myCss :: Monad m => HtmlT m ()
myCss = style_ $ [q|

input, button  {
  font-size: var(--form-element-font-size);
  height: 2.5rem;
  padding: 0.25rem 0.5rem;
  border-radius: 0.25rem;
  border: 1px solid #ccc;
}

input[type="search"] {
  font-size: var(--form-element-font-size);
  height: 2.5rem;
  padding: 0.25rem 0.5rem;
  border-radius: 0.25rem;
  border: 1px solid #ccc;
}

button.search {
  background: url('/icon/refresh.svg') no-repeat center center;
  background-size: 24px 24px;
  min-width:  32px;
  height: 2.5rem;
}

button.search svg {
}

body, html {
  margin: 0;
  height: 100%;
  font-size: 18px;
}


header {
  width: 100%;

  font-size: 20px;

  display: flex;
  align-items: center;

  padding: 8px;

  top: 0;
  left: 0;
  z-index: 100;
  box-shadow: 0 2px 5px rgba(0,0,0,0.2);
}

/* height: 64px; */

header h1 {
  font-size: 20px;
  margin: 0 0 0 2.21rem;
  font-weight: 500;
}

.header-title {
}

.container {
  width: 100%;
}

.header-links {
  display: flex;
  margin-left: 10em;
  gap: 2rem;
  background: white;
}

header a {
  /* display: inline; */
  height: 1rem;
  text-decoration: none;
}

.header-actions {
  margin-left: auto;
}

nav.left {
  flex: 0 0 20rem;
  padding: 4rem 0rem 0 1rem;
  font-size: 20px;
  flex-direction: column;
  justify-content: normal;
  background: #FAFAFA;
}

nav.left .info-block {
  margin-bottom: 4rem;
}

section#repo-data {
  margin-top: 1.5rem;
}

section#repo-data> h1::after,
section#repo-data> h2::after,
section#repo-data> h3::after,
section#repo-data> h4::after
{
  content: "";
  display: block;
  margin: 8px 0;
  height: 1px;
  background-color: #ccc;
}

/*  height: calc(100vh - 64px); */

section {
  margin-top: 1rem;
}


main {
  flex-grow: 1;
  padding: 2rem 0 0 4rem;
}


.main {
  display: flex;
  padding: 4px 0 0 0;
  margin: 0;
  min-height: 100vh;
}

main h1 {
  font-size: 1.5rem;
}

main h2 {
  font-size: 1.45rem;
  font-weight: 400;
}

div .repo-list-item {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;


  background: #FAFAFA;
  padding: 0.75rem;
  margin-top: 1.75rem;
  border-radius: 0.25rem;
  border: 1px solid #BFC7D9;
}

.channel-list-item {
  display: block;

  background: #FAFAFA;
  padding: 1.45rem;
  margin-top: 2rem;
  border-radius: 0.25rem;
  border: 1px solid #BFC7D9;

}

.repo-info, .repo-info-misc {
  flex: 1;
  padding: 1.25rem;
}

.repo-info h2 a {
  text-decoration: none;
  color: inherit;
}

.repo-info h2 {
  font-size: 1.35rem;
}

.repo-info h2 a:hover {
  text-decoration: underline dotted;
}


.repo-info-misc {
  text-align: right;
  font-size: 0.85rem;
}


.attr {
  display: flex;
  flex-direction: row;
  gap: 2rem;
  margin-bottom: 0.5em;
  padding-right: 1rem;
}

.attrname,  {
  flex: 1;
  margin-right: 0.5em;
}

.attrval {
  flex-basis: 70%;
  text-align: right;
}

.icon {
  flex-basis: 90%;
  text-align: right;
}


form.search {
  display: flex;
  align-items: center;
  align-items: flex-start;
  gap: 0.5em;
}

form.search input[type="search"] {
  align: center;
  flex-grow: 1;
  margin-right: 0.5em;
}

form.search button {
  align: center;
  min-width: 4rem;
}

.xclip::after {
  display: inline-block;
  content: url('/icon/xclip.svg');
  vertical-align: middle;
  width: 24px;
  height: 24px;
  opacity: 0;
  transition: opacity 0.2s;
  left: 16px;
  position: relative;
}

.xclip:hover::after {
  left: 16px;
  position: relative;
  content: url('/icon/xclip.svg');
  vertical-align: middle;
  height: 24x;
  width: 24x;
  opacity: 1;
}

.xclip {
  /*position: relative;*/
  text-decoration: underline dotted;
}

.xclip:hover {
  text-decoration: underline dotted;
}

.clicked:hover::after {
  content: url('/icon/xclipdone.svg');
  vertical-align: middle;
  right: 16px;
  height: 24x;
  width: 24x;
  opacity: 1;
}

nav[role="tab-control"] {
  min-height: 24px;
  font-size: 18px;
 /*  border: 1px solid black; */
  display: block;
  margin-bottom: 4rem;
}

nav[role="tab-control"] li {
  display: block;
  padding: 0 0 0 0;
  padding-right: 2rem;
  margin-right: 2rem;
  border-right: 2px solid gray;
  font-weight: bolder;
}

nav[role="tab-control"] li a {
  color: inherit;
  text-decoration: none;
}

nav[role="tab-control"] li.active {
  display: block;
  color: #0089D1;
}

|]


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


-- <svg xmlns="http://www.w3.org/2000/svg"
-- class="icon icon-tabler icon-tabler-refresh-dot" width="44" height="44" viewBox="0 0 24 24" stroke-width="1.5" stroke="#2c3e50" fill="none" stroke-linecap="round" stroke-linejoin="round">
--   <path stroke="none" d="M0 0h24v24H0z" fill="none"/>
--   <path d="M20 11a8.1 8.1 0 0 0 -15.5 -2m-.5 -4v4h4" />
--   <path d="M4 13a8.1 8.1 0 0 0 15.5 2m.5 4v-4h-4" />
--   <path d="M12 12m-1 0a1 1 0 1 0 2 0a1 1 0 1 0 -2 0" />
-- </svg>

-- <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round">
--   <path stroke="none" d="M0 0h24v24H0z" fill="none"/>
--   <path d="M20 11a8.1 8.1 0 0 0 -15.5 -2m-.5 -4v4h4" />
--   <path d="M4 13a8.1 8.1 0 0 0 15.5 2m.5 4v-4h-4" />
-- </svg>


{- HLINT ignore "Functor law" -}

browserRootPage :: Monad m => [Syntax c] -> HtmlT m ()
browserRootPage syn = rootPage do

  let bro = mconcat [ b | ListVal (SymbolVal "browser": b ) <- syn ]

  let channels = [ mchan | ListVal (SymbolVal "channel" : mchan) <- bro ]

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


pluginPage :: MonadIO m
            => ServiceCaller BrowserPluginAPI PIPE
            -> PluginMethod
            -> HtmlT m ()
pluginPage api method' = do

  let method = method' & over getArgs ( HM.singleton "OUTPUT" "html" <> )

  r <- liftIO (callRpcWaitMay @RpcChannelQuery (TimeoutSec 1) api method)
         <&> join
         <&> fromMaybe mempty

  let str = TLE.decodeUtf8 r & TL.unpack

  let stripped = extractBodyHtml str & TL.pack & TLE.encodeUtf8

  rootPage $ do

    div_ [class_ "container main"] $ do
      nav_ [class_ "left"] $ do
        div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"
        div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"

      main_ do
        toHtmlRaw stripped

  where
    extractBodyHtml :: String -> String
    extractBodyHtml html =
        let tags = parseTags html
            bodyTags = takeWhile (~/= "</main>") . dropWhile (~/= "<main>") $ tags
            -- Убираем начальный и конечный тег <body>, если это необходимо
            contentTags = drop 1 $ take (length bodyTags - 1) bodyTags
        in renderTags contentTags

