{-# Language TemplateHaskell #-}
module HBS2Git.Export where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Hash
import HBS2.Data.Detect hiding (Blob)
import HBS2.Data.Detect qualified as Detect

import Data.Config.Suckless

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2Git.App
import HBS2Git.State

import System.Exit
import Data.Maybe
import Data.Foldable (for_)
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Cache as Cache
import System.FilePath
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet  qualified as HashSet
import Codec.Serialise
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue qualified as Q

newtype AsGitRefsFile a = AsGitRefsFile a

newtype RepoHead =
  RepoHead
  { _repoHeads :: HashMap GitRef GitHash
  }
  deriving stock (Generic)

makeLenses 'RepoHead

instance Pretty (AsGitRefsFile RepoHead) where
  pretty (AsGitRefsFile h) = vcat (fmap fmt els)
    where
      els = HashMap.toList (view repoHeads h)
      fmt (r,hx) = pretty hx <+> pretty r

instance Serialise RepoHead

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


runDumpStateTree :: MonadIO m => HashRef -> Maybe HashRef-> App m ()
runDumpStateTree ref rfv = do

  -- FIXME: readRefValue
  root <- pure rfv `orDie` "ref value not set"      -- readRefValue ref

  q <- liftIO newTQueueIO

  let walk h = walkMerkle h (readBlock . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
        case hr of
          Left hx -> liftIO $ die $ show $ pretty "missed block:" <+> pretty hx
          Right (hrr :: [HashRef]) -> do
             forM_ hrr $ liftIO . atomically . Q.writeTQueue q

  walk (fromHashRef root)

  entries <- liftIO $ atomically $ Q.flushTQueue q

  hd <- pure (headMay entries) `orDie` "no head block found"

  -- TODO: what-if-metadata-is-really-big?
  hdData <- readBlock hd `orDie` "empty head block"

  let hdBlk = tryDetect (fromHashRef hd) hdData

  case hdBlk of
    MerkleAnn{} -> pure ()
    _ -> liftIO $ die "invalid head block format"

  let meta = headDef "" [ Text.unpack s | ShortMetadata s <- universeBi hdBlk ]

  syn <- liftIO $ parseTop meta & either (const $ die "invalid head block meta") pure

  let app = headDef False
            [ True
            | ListVal @C (Key "application:" [SymbolVal "hbs2-git"]) <- syn
            ]

  let hdd = headDef False
            [ True
            | ListVal @C (Key "type:" [SymbolVal "head"]) <- syn
            ]


  unless ( app && hdd ) do
    liftIO $ die "invalid head block meta"

  headBlk <- readObject hd `orDie` "empty head block data"

  -- shutUp

  -- liftIO $ LBS.putStr headBlk

  dbPath <- makeDbPath ref
  db <- dbEnv dbPath

  -- dn <-
  -- withDB


  pure ()

runExport :: MonadIO m => HashRef -> App m ()
runExport h = do
  trace $ "Export" <+> pretty h

  -- TODO: read-repo-head
  trace "read repository head"

  -- TODO: create-repo-head-block
  trace "create-repo-head-block"


  git <- asks (view appGitDir)

  trace $ "git directory is" <+> pretty git

  env <- ask

  let branches = cfgValue @ConfBranch env

  refs <- gitReadRefs git branches

  -- TODO: build-transitive-closure
  trace "build-transitive-closure"

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

  let repoHead = RepoHead (HashMap.fromList refs) & show . pretty . AsGitRefsFile
                                                  & LBS.pack

  -- shutUp

  els <- liftIO $ Cache.toList (hCache cache)

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

    trace $ pretty $ length hashes
    trace $ "head" <+> pretty hh
    trace $ "merkle" <+> pretty root


