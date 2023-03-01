{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.Local.CLI where

import HBS2.Git.Types

import HBS2.System.Logger.Simple

import Control.Concurrent.Async
import Data.String
import Control.Monad.IO.Class
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad.Writer
import Safe
import Text.InterpolatedString.Perl6 (qc)
import Prettyprinter
import System.Process.Typed


parseHash :: BS8.ByteString -> GitHash
parseHash = fromString . BS8.unpack

parseHashLazy :: LBS.ByteString -> GitHash
parseHashLazy = fromString . BS8.unpack . LBS.toStrict

gitGetDepsPure :: GitObject -> Set GitHash

gitGetDepsPure (GitObject Tree bs) = Set.fromList $ execWriter (go bs)
  where
    go :: ByteString -> Writer [GitHash] ()
    go s = case LBS.uncons s of
      Nothing -> pure ()
      Just ('\x00', rest) -> do
        let (hash, rest') = LBS.splitAt 20 rest
        tell [GitHash (LBS.toStrict hash)]
        go rest'

      Just (_, rest) -> go rest

gitGetDepsPure (GitObject Commit bs) = Set.fromList (recurse ls)
  where
    ls = LBS.lines bs
    recurse :: [LBS.ByteString] -> [GitHash]
    recurse [] = []
    recurse ("":_)  = []
    recurse (x:xs) =
      case LBS.words x of
        ["tree", s]   -> fromString (LBS.unpack s) : recurse xs
        ["parent", s] -> fromString (LBS.unpack s) : recurse xs
        _             -> recurse xs


gitGetDepsPure _ = mempty




gitGetObjectType :: MonadIO m => GitHash -> m (Maybe GitObjectType)
gitGetObjectType hash = do
  (_, out, _) <- readProcess (shell [qc|git cat-file -t {pretty hash}|])
  case headMay (LBS.words out) of
    Just "commit" -> pure (Just Commit)
    Just "tree"   -> pure (Just Tree)
    Just "blob"   -> pure (Just Blob)
    _        -> pure Nothing



gitGetCommitDeps :: MonadIO m => GitHash -> m [GitHash]
gitGetCommitDeps hash  = do
  (_, out, _) <- readProcess (shell [qc|git cat-file commit {pretty hash}|])
  pure $ Set.toList (gitGetDepsPure (GitObject Commit out))

gitGetTreeDeps :: MonadIO m => GitHash -> m [GitHash]
gitGetTreeDeps hash = do
  (_, out, _) <- readProcess (shell [qc|git ls-tree {pretty hash}|])
  let ls =  fmap parseHash . getHash <$> BS8.lines (LBS.toStrict out)
  pure (catMaybes ls)
  where
    getHash = flip atMay 2 . BS8.words


gitGetDependencies :: MonadIO m => GitHash -> m [GitHash]
gitGetDependencies hash = do
  ot <- gitGetObjectType hash
  case ot of
    Just Commit -> gitGetCommitDeps hash
    Just Tree   -> gitGetTreeDeps hash
    _           -> pure mempty


gitGetTransitiveClosure :: forall cache . (HasCache cache GitHash (Set GitHash) IO)
                        => cache
                        -> Set GitHash
                        -> GitHash
                        -> IO (Set GitHash)

gitGetTransitiveClosure cache exclude hash = do
    -- trace $ "gitGetTransitiveClosure" <+> pretty hash
    r <- cacheLookup cache hash :: IO (Maybe (Set GitHash))
    case r of
      Just xs -> pure xs
      Nothing -> do
        deps <- gitGetDependencies hash
        clos <- mapConcurrently (gitGetTransitiveClosure cache exclude) deps
        let res = (Set.fromList (hash:deps) <> Set.unions clos) `Set.difference` exclude
        cacheInsert cache hash res
        pure res

