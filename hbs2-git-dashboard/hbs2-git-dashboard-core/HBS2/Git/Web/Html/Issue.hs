{-# LANGUAGE CPP #-}

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
import HBS2.Git.Web.Html.Parts.Blob

import Data.Text qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)

#if __GLASGOW_HASKELL__ < 906
import Control.Applicative -- add liftA2 into scope
#endif

data IssueOptionalArg w t = IssueOptionalArg w t

issueOptionalArg :: Fixme -> FixmeAttrName -> IssueOptionalArg Fixme FixmeAttrName
issueOptionalArg  = IssueOptionalArg

instance ToHtml (IssueOptionalArg Fixme FixmeAttrName) where
  toHtml (IssueOptionalArg fxm n)  = do
    for_ (fixmeGet n fxm) $ \t -> do
      tr_ do
        td_ [class_ "whitespace-nowrap"] $ strong_ (toHtml $ show $ pretty n)
        td_ [class_ "w-full"] (toHtml $ show $ pretty t)

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

        nav_ [class_ "mb-1"] do

          div_ do
            small_ do
              a_ [ href_ (toURL (RepoPage IssuesTab lww))
                 ] do
                span_ [class_ "inline-icon-wrapper"] $ svgIcon IconArrowUturnLeft
                span_ [] "back to issues"

        article_ [class_ "issue-info-card"] do
          header_ do
            h5_ do
              toHtml (coerce @_ @Text $ fixmeTag fxm)
              " "
              span_ [class_ "font-normal"] do
                let fkKey = coerce @_ @Text $ fixmeKey fxm
                span_ [ class_ "issue-id secondary"
                      , data_ "tooltip" "Copy"
                      , onClickCopyText $ Text.take 10 fkKey
                      ] $ toHtml (H $ fixmeKey fxm)
                " "
                toHtml (coerce @_ @Text $ fixmeTitle fxm)

          div_ [class_ "overflow-x-auto"] $ table_ [class_ "issue-info-table mb-0"] do
           
            toHtml (issueOptionalArg fxm "workflow")
            toHtml (issueOptionalArg fxm "class")
            toHtml (issueOptionalArg fxm "assigned")
            toHtml (issueOptionalArg fxm "scope")
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
                      a_ [ href_ "#"
                         , hyper_ "on click toggle .hidden on #issue-blob"
                         ] do
                        toHtml $ show $ pretty file

            -- toHtml (issueOptionalArg fxm "file")

        section_ [class_ "lim-text"] do
          toHtmlRaw $ renderMarkdown txt

        let s0 = fixmeStart fxm
        let e0 = fixmeEnd fxm
        let n  = liftA2 (-) e0 s0 & fromMaybe 0

        let hide = if n > 3 then "hidden" else ""

        section_ [id_ "issue-blob", class_ hide ] $ void $ runMaybeT do
          blob <- toMPlus mbBlob
          s <- s0 & toMPlus <&> fromIntegral
          e <- e0 & toMPlus <&> fromIntegral

          let before = max 0 (s - 2)
          let seize  = max 1 (e - s + 100)

          debug $ "PREPROCESS BLOB" <+> pretty before <+> pretty seize

          lift $ doRenderBlob' (pure mempty) (trim before seize) lww blob

    where
      trim before seize txt =
        Text.lines txt & drop before & take seize & Text.unlines


