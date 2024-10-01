module HBS2.Git.Web.Html.Issue (issuePage) where


import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.Fixme as Fixme

import HBS2.OrDie

import HBS2.Git.Web.Assets

import HBS2.Git.Web.Html.Types
import HBS2.Git.Web.Html.Root
import HBS2.Git.Web.Html.Markdown
import HBS2.Git.Web.Html.Fixme()

import Data.Text qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)


data IssueOptionalArg w t = IssueOptionalArg w t

issueOptionalArg :: Fixme -> FixmeAttrName -> IssueOptionalArg Fixme FixmeAttrName
issueOptionalArg  = IssueOptionalArg

instance ToHtml (IssueOptionalArg Fixme FixmeAttrName) where
  toHtml (IssueOptionalArg fxm n)  = do
    for_ (fixmeGet n fxm) $ \t -> do
      tr_ do
        th_ $ strong_ (toHtml $ show $ pretty n)
        td_ (toHtml $ show $ pretty t)

  toHtmlRaw  = toHtml

issuePage :: (MonadIO m, DashBoardPerks m, MonadReader DashBoardEnv m)
         => RepoLww
         -> FixmeKey
         -> HtmlT m ()

issuePage repo@(RepoLww lww) f = rootPage do

  ti@TopInfoBlock{} <- lift $ getTopInfoBlock (coerce repo)

  fxm <- lift (getFixme repo f)
            >>= orThrow (itemNotFound f)

  let txt = fixmePlain fxm & fmap coerce & Text.intercalate "\n"

  let mbFile = fixmeGet "file" fxm

  mbBlob <- runMaybeT do
              blobHashText <- fixmeGet "blob" fxm & toMPlus
              debug $ red "BLOB HASH TEXT" <+> pretty blobHashText
              hash <- coerce blobHashText
                             & Text.unpack
                             & fromStringMay @GitHash
                             & toMPlus
              debug $ red "BLOB" <+> pretty hash
              lift (lift $ selectBlobInfo (BlobHash hash))
                 >>= toMPlus

  debug $ "BLOB INFO" <> line <> pretty (fmap blobHash mbBlob)

  main_ [class_ "container-fluid"] do
    div_ [class_ "wrapper"] do
      aside_ [class_ "sidebar"] do

        -- issuesSidebar (coerce repo) ti mempty
        repoTopInfoBlock (coerce repo) ti

      div_ [class_ "content"] $ do

        nav_ [style_ "margin-bottom: 2em;"] do

          div_ do
            small_ do
              a_ [ href_ (toURL (RepoPage IssuesTab lww))
                 ] do
                span_ [class_ "inline-icon-wrapper"] $ svgIcon IconArrowUturnLeft
                span_ [] "back to issues"

        section_ do
          table_ do
            tr_ do
              td_ [colspan_ "2"] do
                let fkKey = coerce @_ @Text $ fixmeKey fxm
                strong_ [style_ "margin-right: 1ch;"] $ toHtml (coerce @_ @Text $ fixmeTag fxm)
                span_ [ style_ "margin-right: 1ch;"
                      -- FIXME: make-underlined-on-hover
                      --  $assigned fastpok
                      , class_ "copyable-text"
                      , onClickCopyText $ Text.take 10 fkKey
                      ] $ toHtml (H $ fixmeKey fxm)
                " "
                span_ [] $ toHtml (coerce @_ @Text $ fixmeTitle fxm)

            toHtml (issueOptionalArg fxm "workflow")
            toHtml (issueOptionalArg fxm "committer-name")
            toHtml (issueOptionalArg fxm "commit")


            maybe1 mbFile none $ \file -> do
              tr_ do
                th_ $ strong_ [] $ "file"

                case mbBlob of
                  Nothing -> do
                    td_ do
                      toHtml $ show $ pretty file
                  Just (BlobInfo{}) -> do
                    td_ do
                      a_ [ href_ "#" ] do
                        toHtml $ show $ pretty file

            -- toHtml (issueOptionalArg fxm "file")

        section_ [class_ "lim-text"] do
          toHtmlRaw $ renderMarkdown txt




