module HBS2.Git3.Repo ( waitRepo
                      , getRepoRefMaybe
                      , getRepoManifest
                      , listRemotes
                      , HasGitRemoteKey(..)
                      , module Exported
                      ) where

import HBS2.Git3.Prelude
import HBS2.System.Dir

import HBS2.Git.Local.CLI
import HBS2.Git3.State

import HBS2.Git3.Repo.Types as Exported
import HBS2.Git3.Repo.Init  as Exported

import Data.Config.Suckless

import Data.Either

{- HLINT ignore "Functor law" -}

listRemotes :: MonadIO m => m [(GitRef, GitRepoKey)]
listRemotes = do

  git <- findGitDir >>= orThrow NoGitDir

  conf <- liftIO (readFile (git </> "config"))
            <&> parseTop
            <&> fromRight mempty

  let urls = flip fix (mempty,Nothing,conf) $ \next -> \case
       (acc,_, ListVal [SymbolVal "remote", StringLike x] : rest) ->
         next (acc,Just x, rest)

       (acc, Just x, ListVal [SymbolVal "url", _, RepoURL3 u] : rest) ->
         next ( (fromString x, u) : acc, Nothing, rest)

       (acc, x, _ : rest) -> next ( acc, x, rest)

       (acc,_,[])     -> acc

  pure  urls
