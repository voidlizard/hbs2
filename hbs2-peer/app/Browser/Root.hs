module Browser.Root
  ( module Lucid
  , browserRootPage
  , channelPage
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Net.Proto.Types
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.Proto.BrowserPlugin
import HBS2.Net.Messaging.Pipe

import Data.Config.Suckless.Syntax

import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Lucid (Html,HtmlT,toHtml,toHtmlRaw)
import Lucid.Html5 hiding (for_)
import Data.Text qualified as Text
import Text.InterpolatedString.Perl6 (q)
import Data.ByteString.Lazy.Char8 qualified as LBS
import System.FilePath
import Control.Monad
import Control.Monad.Trans.Maybe

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
  fill: white;
  min-width:  32px;
  height: 2.5rem;
}

body, html {
  margin: 0;
  height: 100%;
  font-size: 16px;
}


header {
  width: 100%;

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
  font-size: 1.45rem;
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
  display: block;
  padding: 2rem;
  margin: 0;
  background: #FAFAFA;
  width: 20em;
}

main {
  flex-grow: 1;
  padding: 2rem 0 0 4rem;
}


/*  height: calc(100vh - 64px); */

section {
  margin-top: 1rem;
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

.repo-info h2 a:hover {
  text-decoration: underline dotted;
}


.repo-info-misc {
  text-align: right;
  font-size: 0.85rem;
}


.attr {
  display: flex;
  margin-bottom: 0.5em;
}

.attrname, .attribute-value {
  flex: 1;
  margin-right: 0.5em;
}

.attrval {
  text-align: right;
  font-weight: bold;
  flex-basis: 30%;
  text-align: right;
}

nav.left .info-block {
  margin-top: 4em;
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
}
|]


rootPage :: Monad m => HtmlT m () -> HtmlT m ()
rootPage content  = do
  doctypehtml_ do
    head_ do
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      -- link_ [rel_ "stylesheet", href_"/css/pico.min.css"]
      link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@picocss/pico@2.0.6/css/pico.min.css"]
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

            a_ [href_ (path [url])] (toHtml (show $ pretty $ AsBase58 rchan))

            for_ [ s | LitStrVal s <- desc ] $ \s -> do
              p_ (toHtml s)


channelPage :: MonadIO m
            => ServiceCaller BrowserPluginAPI PIPE
            -> [(Text,Text)]
            -> HtmlT m ()
channelPage api env' = do
  let env = HM.toList $ HM.fromList env' <> HM.fromList [("METHOD","list-entries"),("OUTPUT","html")]

  r <- liftIO (callRpcWaitMay @RpcChannelQuery (TimeoutSec 1) api env)
         <&> join
         <&> fromMaybe mempty

  let str = LBS.unpack r

  rootPage $ do

    div_ [class_ "container main"] $ do
      nav_ [class_ "left"] $ do
        div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"
        div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"

      main_ do
        toHtmlRaw (extractBodyHtml str)

  where
    extractBodyHtml :: String -> String
    extractBodyHtml html =
        let tags = parseTags html
            bodyTags = takeWhile (~/= "</main>") . dropWhile (~/= "<main>") $ tags
            -- Убираем начальный и конечный тег <body>, если это необходимо
            contentTags = drop 1 $ take (length bodyTags - 1) bodyTags
        in renderTags contentTags

