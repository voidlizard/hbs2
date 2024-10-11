module HBS2.Git.Web.Html.Parts.Issues.Sidebar where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.Fixme as Fixme

import HBS2.Git.Web.Html.Types
import HBS2.Git.Web.Html.Parts.TopInfoBlock

import Data.Map qualified as Map
import Lucid.Base
import Lucid.Html5 hiding (for_)
import Lucid.Htmx



issuesSidebar :: (MonadIO m, DashBoardPerks m, MonadReader DashBoardEnv m)
              => LWWRefKey 'HBS2Basic
              -> TopInfoBlock
              -> [(Text,Text)]
              -> HtmlT m ()
issuesSidebar lww topInfoBlock p' = asksBaseUrl $ withBaseUrl do

  let p = Map.fromList p'

  tot <- lift $ countFixme (RepoLww lww)
  fmw <- lift $ countFixmeByAttribute (RepoLww lww) "workflow"
  fmt <- lift $ countFixmeByAttribute (RepoLww lww) "fixme-tag"
  ass <- lift $ countFixmeByAttribute (RepoLww lww) "assigned"
  cla  <- lift $ countFixmeByAttribute (RepoLww lww) "class"

  repoTopInfoBlock lww topInfoBlock

  div_ [class_ "info-block" ] do

    summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Tag"

    -- TODO: make-this-block-properly

    ul_ do
      for_ fmt $ \(s,n) -> do
        li_ [] $ small_ [] do
          a_ [ class_ "secondary"
             , hxGet_ (toBaseURL (Paged 0 (RepoFixmeHtmx (Map.insert "fixme-tag" (coerce s) p) (RepoLww lww))))
             , hxTarget_ "#fixme-tab-data"
             ] do
            span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
              toHtml $ show $ pretty n

            span_ [] $ toHtml $ show $ pretty s

    summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Status"

    ul_ do

      li_ [] $ small_ [] do
        a_ [ class_ "secondary"
           , hxGet_ (toBaseURL (Paged 0 (RepoFixmeHtmx (Map.delete "workflow" p) (RepoLww lww))))
           , hxTarget_ "#fixme-tab-data"
           ] do
          span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
            toHtml $ show $ pretty (fromMaybe 0 tot)

          span_ [] $ toHtml $ show $ pretty "[all]"

      for_ fmw $ \(s,n) -> do
        li_ [] $ small_ [] do
          a_ [ class_ "secondary"
             , hxGet_ (toBaseURL (Paged 0 (RepoFixmeHtmx (Map.insert "workflow" (coerce s) p) (RepoLww lww))))
             , hxTarget_ "#fixme-tab-data"
             ] do
            span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
              toHtml $ show $ pretty n

            span_ [] $ toHtml $ show $ pretty s


    summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Assigned"

    for_ ass $ \(s,n) -> do
      li_ [] $ small_ [] do
        a_ [ class_ "secondary"
           , hxGet_ (toBaseURL (Paged 0 (RepoFixmeHtmx (Map.insert "assigned" (coerce s) p) (RepoLww lww))))
           , hxTarget_ "#fixme-tab-data"
           ] do
          span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
            toHtml $ show $ pretty n

          span_ [] $ toHtml $ show $ pretty s

    summary_ [class_ "sidebar-title"] $ small_ $ strong_ "Class"

    for_ cla $ \(s,n) -> do
      li_ [] $ small_ [] do
        a_ [ class_ "secondary"
           , hxGet_ (toBaseURL (Paged 0 (RepoFixmeHtmx (Map.insert "class" (coerce s) p) (RepoLww lww))))
           , hxTarget_ "#fixme-tab-data"
           ] do
          span_ [style_ "display: inline-block; width: 4ch; text-align: right; padding-right: 0.5em;"] $
            toHtml $ show $ pretty n

          span_ [] $ toHtml $ show $ pretty s

  pure ()
