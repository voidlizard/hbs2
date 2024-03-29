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

rootPage :: Monad m => HtmlT m () -> HtmlT m ()
rootPage content  = do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "hbs2-peer browser"
    link_ [rel_ "stylesheet", href_ "/css/pico.min.css"]
    link_ [rel_ "stylesheet", href_ "/css/custom.css"]

    style_ [type_ "text/css"]  [q|
      /* jopakita */
      body, html {
        height: 100%;
        margin: 0;
      }
      .root {
        display: flex;
        flex-direction: column;
        min-height: 100vh;
      }
      main {
        flex-grow: 1;
      }
      .flex-container {
        display: flex;
        justify-content: space-around;
      }
      .flex-item {
        margin: 10px;
        padding: 20px;
        border: 1px solid #ccc;
      }
      .resource-box {
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        margin: 4px;
        padding: 20px;
        border-radius: 8px;
      }

      h2 {
        font-size: 1.15rem;
      };

      |]

  body_ $ do
    div_ [class_ "container root"] $ do

      header_ $ do
        h1_ "hbs2-peer browser"

      main_ content

      footer_ [class_ "footer"]"hbs2-peer by hbs2.net 2024"


browserRootPage :: Monad m => [Syntax c] -> HtmlT m ()
browserRootPage syn = rootPage do

  let bro = mconcat [ b | ListVal (SymbolVal "browser": b ) <- syn ]

  let channels = [ mchan | ListVal (SymbolVal "channel" : mchan) <- bro ]

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

      div_ [class_ "resource-box"] do
        h2_ ( "Channel: "  <> toHtml title)
        div_ do
          a_ [href_ (path ["channel", rcs])] (toHtml rcs)

          p_ mempty

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


