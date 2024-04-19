{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Git.Web.Html.Root where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State

import HBS2.Base58
import HBS2.Peer.Proto.RefChan.Types

import Data.Config.Suckless

import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Maybe
import Data.Text qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)
import Lucid.Htmx

import Text.Pandoc hiding (getPOSIXTime)
import Control.Monad.Identity
import System.FilePath
import Text.InterpolatedString.Perl6 (q)
import Data.Word

rootPath :: [String] -> [String]
rootPath = ("/":)

path :: [String] -> Text
path = Text.pack . joinPath . rootPath

myCss :: Monad m => HtmlT m ()
myCss = do
  link_ [rel_ "stylesheet", href_ (path ["css/custom.css"])]

hyper_ :: Text -> Attribute
hyper_  = makeAttribute "_"

onClickCopy :: Text -> Attribute
onClickCopy s =
  hyper_ [qc|on click writeText('{s}') into the navigator's clipboard add .clicked to me wait 2s remove .clicked from me|]

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

instance ToHtml RepoBrief where
  toHtml (RepoBrief txt) = toHtmlRaw (renderMarkdown' txt)
  toHtmlRaw (RepoBrief txt) = toHtmlRaw (renderMarkdown' txt)

data WithTime a = WithTime Integer a

instance ToHtml (WithTime RepoListItem) where
  toHtmlRaw = pure mempty

  toHtml (WithTime t it@RepoListItem{..}) = do

    let now = t

    let locked = isJust $ coerce @_ @(Maybe HashRef) rlRepoGK0

    let url = path ["repo", Text.unpack $ view rlRepoLwwAsText it]
    let t = fromIntegral $ coerce @_ @Word64 rlRepoSeq

    let updated = "" <+> d
          where
            sec = now - t
            d | sec > 86400  = pretty (sec `div` 86400)  <+> "days ago"
              | sec > 3600   = pretty (sec `div` 3600)   <+> "hours ago"
              | otherwise    = pretty (sec `div` 60)     <+> "minutes ago"

    div_ [class_ "repo-list-item"] do
      div_ [class_ "repo-info", style_ "flex: 1; flex-basis: 70%;"] do

        h2_ [class_ "xclip", onClickCopy (view rlRepoLwwAsText it)] $ toHtml rlRepoName
        p_ $ a_ [href_ url] (toHtml $ view rlRepoLwwAsText it)

        toHtml rlRepoBrief

      div_ [ ] do
        div_ [ class_ "attr" ] do
          div_ [ class_ "attrname"]  (toHtml $ show updated)

        when locked do
          div_ [ class_ "attr" ] do
            div_ [ class_ "attrval icon"] do
              img_ [src_ "/icon/lock-closed.svg"]

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



dashboardRootPage :: (DashBoardPerks m, MonadReader DashBoardEnv m) => HtmlT m ()
dashboardRootPage = rootPage do

  now  <- liftIO getPOSIXTime <&> fromIntegral . round
  items <- lift selectRepoList

  div_ [class_ "container main"] $ do
    nav_ [class_ "left"] $ do
      div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"
      div_ [class_ "info-block"] "Всякая разная рандомная информация хрен знает, что тут пока выводить"

    main_ do

      section_ do
        h1_ "Git repositories"
        form_ [class_ "search"] do
            input_ [type_ "search", id_ "search"]
            button_ [class_ "search"] mempty


      section_ [id_ "repo-search-results"] do

        for_ items $ \item@RepoListItem{..} -> do
          toHtml (WithTime now item)


      pure ()
      -- for_ channels $ \chan -> void $ runMaybeT do

      --   let title = headDef "unknown" [ t
      --                                     | ListVal [ SymbolVal "title", LitStrVal t ] <- chan
      --                                     ]
      --   let desc = mconcat [ d
      --                      | ListVal (SymbolVal "description" : d) <- chan
      --                      ] & take 5

      --   rchan <- headMay ( catMaybes
      --                  [ fromStringMay @(RefChanId L4Proto) (Text.unpack rc)
      --                  | ListVal [SymbolVal "refchan", LitStrVal rc] <- chan
      --                  ] ) & toMPlus


      --   let alias = headMay [ x
      --                       | ListVal [SymbolVal "alias", LitStrVal x] <- chan
      --                       ]

      --   let url = case alias of
      --               Just x -> Text.unpack x
      --               Nothing -> (show . pretty . AsBase58) rchan

      --   lift do
      --     div_ [class_ "channel-list-item"] do
      --       h2_ $ toHtml title

      --       p_ $ a_ [href_ (path [url])] (toHtml (show $ pretty $ AsBase58 rchan))

      --       for_ [ s | LitStrVal s <- desc ] $ \s -> do
      --         p_ (toHtml s)

