{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# Language MultiWayIf #-}
module HBS2.Git.Web.Html.Root
  ( module HBS2.Git.Web.Html.Root
  , module HBS2.Git.Web.Html.Types
  , module HBS2.Git.Web.Html.Parts.TopInfoBlock
  ) where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.Web.Assets

import HBS2.Git.Web.Html.Types
import HBS2.Git.Web.Html.Markdown
import HBS2.Git.Web.Html.Parts.TopInfoBlock

import Lucid.Base
import Lucid.Html5 hiding (for_)

import Data.Word


myCss :: Monad m => HtmlT m ()
myCss = do
  link_ [rel_ "stylesheet", href_ (path ["css/custom.css"])]

hyper_ :: Text -> Attribute
hyper_  = makeAttribute "_"

ariaLabel_ :: Text -> Attribute
ariaLabel_ = makeAttribute "aria-label"

onClickCopy :: Text -> Attribute
onClickCopy s =
  hyper_ [qc|on click writeText('{s}') into the navigator's clipboard
set my innerHTML to '{svgIconText IconCopyDone}'
set @data-tooltip to 'Copied!'
wait 2s
set my innerHTML to '{svgIconText IconCopy}'
set @data-tooltip to 'Copy'
|]


onClickCopyText :: Text -> Attribute
onClickCopyText s =
  hyper_ [qc|on click writeText('{s}') into the navigator's clipboard
set @data-tooltip to 'Copied!'
wait 2s
set @data-tooltip to 'Copy'
|]


instance ToHtml RepoBrief where
  toHtml (RepoBrief txt) = toHtmlRaw (renderMarkdown' txt)
  toHtmlRaw (RepoBrief txt) = toHtmlRaw (renderMarkdown' txt)

data WithTime a = WithTime Integer a


instance ToHtml GitRef where
  toHtml (GitRef s)= toHtml s
  toHtmlRaw (GitRef s)= toHtmlRaw s

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

      header_ [class_ "container-fluid"] do
        nav_ do
          ul_ $ li_ $ a_ [href_ (toURL RepoListPage)] $ strong_ "hbs2-git dashboard"

      content


dashboardRootPage :: (DashBoardPerks m, MonadReader DashBoardEnv m) => HtmlT m ()
dashboardRootPage = rootPage do

  items <- lift $ selectRepoList mempty

  now  <- liftIO getPOSIXTime <&> fromIntegral . round

  main_ [class_ "container-fluid"] $ do
    div_ [class_ "wrapper"] $ do
      aside_ [class_ "sidebar"] $ do
        div_ [class_ "info-block"] $ small_ "Всякая разная рандомная информация хрен знает, что тут пока выводить"
        div_ [class_ "info-block"] $ small_ "Всякая разная рандомная информация хрен знает, что тут пока выводить"

      div_ [class_ "content"] do

        section_ do
          h2_ "Git repositories"
          form_ [role_ "search"] do
            input_ [name_ "search", type_ "search"]
            input_ [type_ "submit", value_ "Search"]

        section_ do

          for_ items $ \it@RepoListItem{..} -> do

            let locked = isJust $ coerce @_ @(Maybe HashRef) rlRepoGK0

            let url = toURL (RepoPage (CommitsTab Nothing) (coerce @_ @(LWWRefKey 'HBS2Basic) rlRepoLww))
            -- path ["repo", Text.unpack $ view rlRepoLwwAsText it]
            let t = fromIntegral $ coerce @_ @Word64 rlRepoSeq

            let updated = agePure t now

            article_ [class_ "repo-list-item"] do
              div_ do

                h5_ do
                  toHtml rlRepoName

                div_ [class_ "repo-list-item-link-wrapper"] $ do
                  a_ [href_ url] (toHtml $ view rlRepoLwwAsText it)
                  button_ [class_ "copy-button", onClickCopy (view rlRepoLwwAsText it), data_ "tooltip" "Copy"] do
                    svgIcon IconCopy

                toHtml rlRepoBrief

              div_ do

                div_ [class_ "whitespace-nowrap"] do
                  small_ $ "Updated " <> toHtml updated

                when locked do
                  div_ do
                    small_ do
                      span_ [class_ "inline-icon-wrapper"] $ svgIcon IconLockClosed
                      "Encrypted"

                div_ do
                  small_ do
                    span_ [class_ "inline-icon-wrapper"] $ svgIcon IconGitCommit
                    strong_ $ toHtml $ show rlRepoCommits
                    " commits"

                div_ do
                  small_ do
                    span_ [class_ "inline-icon-wrapper"] $ svgIcon IconGitFork
                    strong_ $ toHtml $ show rlRepoForks
                    " forks"


tabClick :: Attribute
tabClick =
  hyper_ "on click take .contrast from .tab for event's target"

