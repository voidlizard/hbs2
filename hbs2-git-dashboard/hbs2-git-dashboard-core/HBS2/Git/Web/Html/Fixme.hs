{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module HBS2.Git.Web.Html.Fixme where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.Fixme as Fixme


import HBS2.Git.Web.Html.Types

import Data.Map qualified as Map
import Lucid.Base
import Lucid.Html5 hiding (for_)
import Lucid.Htmx

import Data.Word
import Data.List qualified as List

import Web.Scotty.Trans as Scotty


#if __GLASGOW_HASKELL__ < 906
import Control.Applicative -- add liftA2 into scope
#endif

instance ToHtml (H FixmeKey) where
  toHtmlRaw (H k) = toHtmlRaw $ take 10 $ show $ pretty k
  toHtml (H k)    = toHtml    $ take 10 $ show $ pretty k

instance ToHtml (H FixmeTag) where
  toHtmlRaw (H k) = toHtmlRaw $ coerce @_ @Text k
  toHtml (H k)    = toHtml    $ coerce @_ @Text k

instance ToHtml (H FixmeTitle) where
  toHtmlRaw (H k) = toHtmlRaw $ coerce @_ @Text k
  toHtml (H k)    = toHtml    $ coerce @_ @Text k

repoFixme :: ( MonadReader DashBoardEnv m
             , DashBoardPerks m
             , HasLimit q
             , HasPredicate q
             , q ~ FromParams 'FixmeDomain [Param]
             )
          => q
          -> LWWRefKey HBS2Basic
          -> HtmlT m ()

repoFixme q@(FromParams p') lww = asksBaseUrl $ withBaseUrl do

  let p = Map.fromList p'

  now <- liftIO $ getPOSIXTime <&> round

  debug $ blue "repoFixme" <+> "LIMITS" <+> viaShow (limit q)

  let offset = maybe 0 fst (limit q)

  fme <- lift $ listFixme (RepoLww lww) (Reversed q)

  for_ fme $ \fixme -> do
    tr_ [class_ "commit-brief-title"] $ do
      td_ [class_ "mono", width_ "10"] do
         a_ [ href_ (toBaseURL (IssuePage (RepoLww lww) (fixmeKey fixme)))
            ] $ toHtml (H $ fixmeKey fixme)
      td_ [width_ "10"] do
         strong_ [] $ toHtml (H $ fixmeTag fixme)
      td_ [] do
         toHtml (H $ fixmeTitle fixme)
    tr_ [class_ "commit-brief-details"] $ do
      td_ [colspan_ "3"] do
        let mco  = fixmeGet "commit-time" fixme & pretty & show & readMay @Word64
        let mw   = fixmeGet "workflow" fixme <&> coerce @_ @Text
        let cla = fixmeGet  "class" fixme <&> coerce @_ @Text
        let mn = liftA2 (-) (fixmeEnd fixme) (fixmeStart fixme)

        small_ do
          for_ mw $ \w -> do
            span_ [] (toHtml $ show $ brackets $ pretty w)
            " "

          for_ mco $ \co ->
            span_ [] $ toHtml $ show $ brackets ("commited" <+> pretty (agePure co now))

          for_ cla $ \c ->
            span_ [] $ toHtml $ show $ brackets (pretty c)

          for_ mn $ \n -> do
            when (n > 0) do
              span_ [] $ toHtml $ show $ brackets ("text:" <+> pretty n)


  unless (List.null fme) do
    tr_ [ class_ "commit-brief-last"
        , hxGet_ (toBaseURL (Paged (offset + fromIntegral fixmePageSize) (RepoFixmeHtmx p (RepoLww lww))))
        , hxTrigger_ "revealed"
        , hxSwap_ "afterend"
        ] do
      td_ [colspan_ "3"] mempty
