module HBS2.Git.Local
  ( module HBS2.Git.Types
  , module HBS2.Git.Local
  )where

import HBS2.Git.Types

import Data.Functor
import Data.String
import Control.Monad
import Control.Monad.IO.Class
import Data.Set (Set)
import Data.Set qualified as Set
import System.Directory
import System.FilePath

gitReadRefs :: MonadIO m => FilePath -> Set String -> m [(GitRef, GitHash)]
gitReadRefs p m = do

  xs <- forM (Set.toList m) $ \br -> do
          let fn = p </> "refs/heads" </> br
          here <- liftIO $ doesFileExist fn
          if here then do
            s <- liftIO $ readFile fn <&> (fromString br,) . fromString
            pure [s]
          else do
            pure mempty

  pure $ mconcat xs


