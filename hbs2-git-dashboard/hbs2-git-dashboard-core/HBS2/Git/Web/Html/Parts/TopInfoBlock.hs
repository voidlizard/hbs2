module HBS2.Git.Web.Html.Parts.TopInfoBlock where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.Manifest
import HBS2.Git.DashBoard.Fixme as Fixme

import HBS2.OrDie

import HBS2.Git.Data.Tx.Git
import HBS2.Git.Web.Assets

import HBS2.Git.Web.Html.Types

import Data.Text qualified as Text
import Lucid.Base
import Lucid.Html5 hiding (for_)

data TopInfoBlock =
  TopInfoBlock
  { author      :: Maybe Text
  , public      :: Maybe Text
  , forksNum    :: RepoForks
  , commitsNum  :: RepoCommitsNum
  , manifest    :: Text
  , fixme       :: Maybe MyRefChan
  , fixmeCnt    :: Int
  , pinned      :: [(Text, Syntax C)]
  , repoHeadRef :: RepoHeadRef
  , repoHead    :: Maybe RepoHead
  , repoName    :: RepoName
  }

repoTopInfoBlock :: (MonadIO m, DashBoardPerks m, MonadReader DashBoardEnv m)
                 => LWWRefKey 'HBS2Basic
                 -> TopInfoBlock
                 -> HtmlT m ()

repoTopInfoBlock lww TopInfoBlock{..} = do

  div_ [class_ "info-block" ] do
    let url = toURL (RepoPage (CommitsTab Nothing) lww)
    let txt = toHtml (ShortRef lww)
    a_ [href_ url, class_ "secondary"] txt

  div_ [class_ "info-block" ] do

    summary_ [class_ "sidebar-title"] $ small_ $ strong_ "About"
    ul_ [class_ "mb-0"] do
      for_ author $ \a -> do
        li_ $ small_ do
          "Author: "
          toHtml a

      for_ public $ \p -> do
        li_ $ small_ do
          "Public: "
          toHtml p

      when (Text.length manifest > 100) do
        li_ $ small_ do
          a_ [class_ "secondary", href_ (toURL (RepoPage ManifestTab lww))] do
            span_ [class_ "inline-icon-wrapper"] $ svgIcon IconLicense
            "Manifest"

      for_ fixme $ \_ -> do
        li_ $ small_ do
          a_ [ class_ "secondary"
             , href_ (toURL (RepoPage IssuesTab lww)) ] do
            span_ [class_ "inline-icon-wrapper"] $ svgIcon IconFixme
            toHtml $ show fixmeCnt
            " Issues"

      when (forksNum > 0) do
        li_ $ small_ do
          a_ [class_ "secondary"
            , href_  (toURL (RepoPage ForksTab lww))
            ] do
              span_ [class_ "inline-icon-wrapper"] $ svgIcon IconGitFork
              toHtml $ show forksNum
              " forks"

      li_ $ small_ do
        a_ [class_ "secondary"
          , href_ (toURL (RepoPage (CommitsTab Nothing) lww))
          ] do
          span_ [class_ "inline-icon-wrapper"] $ svgIcon IconGitCommit
          toHtml $ show commitsNum
          " commits"

      for_ pinned $ \(_,ref) ->  do
        case ref of
          PinnedRefBlob s n hash -> small_ do
            li_ $ a_ [class_ "secondary"
              , href_ (toURL (RepoPage (PinnedTab (Just (s,n,hash))) lww))
              ] do
                  span_ [class_ "inline-icon-wrapper"] $ svgIcon IconPinned
                  toHtml (Text.take 12 n)
                  " "
                  toHtml $ ShortRef hash

parsedManifest :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoListItem  -> m ([Syntax C], Text)
parsedManifest RepoListItem{..} = do

  sto <- asks _sto
  mhead <- readRepoHeadFromTx sto (coerce rlRepoTx)

  case mhead of
    Just x  -> parseManifest (snd x)
    Nothing -> pure (mempty, coerce rlRepoBrief)


getTopInfoBlock :: ( MonadUnliftIO m, MonadIO m
                   , MonadReader DashBoardEnv m
                   )
                =>  LWWRefKey HBS2Basic -> m TopInfoBlock
getTopInfoBlock lww = do

  debug $ red "getTopInfoBlock"

  it@RepoListItem{..} <- (selectRepoList ( mempty
                                  & set repoListByLww (Just lww)
                                  & set repoListLimit (Just 1))
                               <&> listToMaybe
                              ) >>= orThrow (itemNotFound lww)

  sto <- asks _sto
  mhead <- readRepoHeadFromTx sto (coerce rlRepoTx)

  let repoHead = snd <$> mhead

  (meta, manifest) <- parsedManifest it

  let author = headMay [ s | ListVal [ SymbolVal "author:", LitStrVal s ] <- meta ]
  let public = headMay [ s | ListVal [ SymbolVal "public:", SymbolVal (Id s) ] <- meta ]
  let pinned = [ (name,r) | ListVal [ SymbolVal "pinned:", r@(PinnedRefBlob _ name _) ] <- meta ] & take 5

  allowed <- checkFixmeAllowed (RepoLww lww)
  let fixme  = headMay [ x | allowed, FixmeRefChanP x <- meta ]

  fixmeCnt <- if allowed then
                Fixme.countFixme (RepoLww lww) <&> fromMaybe 0
              else
                pure 0

  let forksNum = rlRepoForks
  let commitsNum = rlRepoCommits
  let repoHeadRef = rlRepoHead
  let repoName = rlRepoName

  pure $ TopInfoBlock{..}

