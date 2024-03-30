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
import System.FilePath
import Control.Monad

import UnliftIO

rootPath :: [String] -> [String]
rootPath = ("/browser":)

path :: [String] -> Text
path = Text.pack . joinPath . rootPath

myCss :: Monad m => HtmlT m ()
myCss = style_ $ [q|

input, button  {
  font-size: var(--form-element-font-size);
  height: 2rem;
  padding: 0.25rem 0.5rem;
  border-radius: 0.25rem;
  border: 1px solid #ccc;
}

input[type="search"] {
  font-size: var(--form-element-font-size);
  height: 2rem;
  padding: 0.25rem 0.5rem;
  border-radius: 0.25rem;
  border: 1px solid #ccc;
}


body, html {
  margin: 0;
  height: 100%;
}

header {
  width: 100%;

  display: flex;
  align-items: center;
  padding: 1rem;

  top: 0;
  left: 0;
  z-index: 100;
  box-shadow: 0 2px 5px rgba(0,0,0,0.2);
  height: 64px;
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
  height: calc(100vh - 64px);
}

header h1 {
  font-size: 16pt;
  margin-left: 2rem;
}

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
  font-size: 24pt;
}

main h2 {
  font-size: 18pt;
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
  padding: 0.75rem;
  margin-top: 1.75rem;
  border-radius: 0.25rem;
  border: 1px solid #BFC7D9;

}

.repo-info, .repo-info-misc {
  flex: 1;
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
        div_ [class_ "header-title"] $ strong_ "hbs2-peer dashboard"
      content


browserRootPage :: Monad m => [Syntax c] -> HtmlT m ()
browserRootPage syn = rootPage do

  let bro = mconcat [ b | ListVal (SymbolVal "browser": b ) <- syn ]

  let channels = [ mchan | ListVal (SymbolVal "channel" : mchan) <- bro ]

  div_ [class_ "container main"] $ do
    nav_ [class_ "left"] $ do
      div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"
      div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"

    main_ do
      for_ channels $ \chan -> do

        let title = headDef "unknown" [ t
                                          | ListVal [ SymbolVal "title", LitStrVal t ] <- chan
                                          ]
        let desc = mconcat [ d
                           | ListVal (SymbolVal "description" : d) <- chan
                           ] & take 5

        let rchan = headMay $ catMaybes
                            [ fromStringMay @(RefChanId L4Proto) (Text.unpack rc)
                            | ListVal [SymbolVal "refchan", LitStrVal rc] <- chan
                            ]


        for_ rchan $ \r -> do

          let rcs = show $ pretty (AsBase58 r)

          section_ do

            div_ [class_ "channel-list-item"] do
              h2_ $ toHtml title

              a_ [href_ (path ["channel", rcs])] (toHtml rcs)

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

  rootPage $ toHtmlRaw r


