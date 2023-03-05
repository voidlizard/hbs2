{-# Language TemplateHaskell #-}
module HBS2Git.Export where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Hash


import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2Git.App
import HBS2Git.State

import Data.List (sortBy)
import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Cache as Cache
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform


data HashCache =
  HashCache
  { hCache :: Cache GitHash (Set GitHash)
  , hDb    :: DBEnv
  }

instance Hashable GitHash => HasCache HashCache GitHash (Set GitHash) IO where
  cacheInsert (HashCache cache _) = Cache.insert cache

  cacheLookup (HashCache cache db) k = do
    refs <- withDB db (stateGetDeps k)
    case refs of
      [] -> Cache.lookup' cache k
      xs -> pure $ Just $ Set.fromList xs

newHashCache :: MonadIO m => DBEnv -> m HashCache
newHashCache db = do
  ca <- liftIO $ Cache.newCache Nothing
  pure $ HashCache ca db


runExport :: MonadIO m => HashRef -> App m ()
runExport h = do
  trace $ "Export" <+> pretty h

  git <- asks (view appGitDir)

  trace $ "git directory is" <+> pretty git

  env <- ask

  branches <- cfgValue @ConfBranch
  headBranch' <- cfgValue @HeadBranch

  let defSort a b = case (a,b) of
        ("master",_) -> LT
        ("main", _)  -> LT
        _            -> GT

  let sortedBr = sortBy defSort $ Set.toList branches

  let headBranch = fromMaybe "master"
                     $ headBranch' <|> (fromString <$> headMay sortedBr)

  refs <- gitReadRefs git branches

  dbPath <- makeDbPath h

  trace $ "dbPath" <+> pretty dbPath

  db <- dbEnv dbPath

  cache <- newHashCache db

  for_ refs $ \(_, h) -> do
    liftIO $ gitGetTransitiveClosure cache mempty h <&> Set.toList

  withDB db $ transactional do
    els <- liftIO $ Cache.toList (hCache cache)
    for_ els $ \(k,vs,_) -> do
      for_ (Set.toList vs) $ \h -> do
        stateAddDep k h

  fullHead <- gitHeadFullName headBranch

  debug $ "HEAD" <+> pretty fullHead

  let repoHead = RepoHead (Just fullHead)
                          (HashMap.fromList refs) & show . pretty . AsGitRefsFile
                                                  & LBS.pack

  deps <- withDB db $ do
            x <- forM refs $ stateGetDeps . snd
            pure $ mconcat x

  ae <- ask

  withDB db $ transactional do -- to speedup inserts

    let metaApp = "application:" <+> "hbs2-git" <> line

    let metaHead = fromString $ show
                              $ metaApp <> "type:" <+> "head" <> line

    -- let gha = gitHashObject (GitObject Blob repoHead)
    hh  <- withApp ae $ storeObject metaHead repoHead `orDie` "cant save repo head"

    for_ deps $ \d -> do
      here <- stateGetHash d <&> isJust
      -- FIXME: asap-check-if-objects-is-in-hbs2
      unless here do
        lbs <- gitReadObject Nothing d
        -- TODO: why-not-default-blob
        --   anything is blob
        tp <- gitGetObjectType d <&> fromMaybe Blob --

        let metaO = fromString $ show
                               $ metaApp
                               <> "type:" <+> pretty tp <+> pretty d
                               <> line

        hr' <- withApp ae $ storeObject metaO lbs
        maybe1 hr' (pure ()) $ \hr -> do
          withDB db $ statePutHash tp d hr
          trace $ "store" <+> pretty tp <+> pretty d <+> pretty hr

    hashes <- (hh : ) <$> stateGetAllHashes

    let pt = toPTree (MaxSize 512) (MaxNum 512) hashes -- FIXME: settings

    root <- makeMerkle 0 pt $ \(_,_,bss) -> do
      void $ withApp ae $ storeObject (fromString (show metaApp)) bss

    info $ "objects:" <+> pretty (length hashes)
    info $ "head:" <+> pretty hh
    info $ "merkle:" <+> pretty root

