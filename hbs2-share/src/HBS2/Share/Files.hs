module HBS2.Share.Files where

import HBS2.Prelude.Plated

import System.Directory
import System.FilePath
import Data.List qualified as List
import System.FilePattern
import Data.Function
import UnliftIO


listFiles :: MonadUnliftIO m => [FilePattern] -> FilePath -> (FilePath -> m ()) -> m ()
listFiles ignore dir action = go dir
  where
    matches p f = or [ i ?== f | i <- p ]

    go fn = do

      let skip = or [ i ?== fn | i <- ignore ]

      unless skip do
        isF <- liftIO $ doesFileExist fn
        if isF then do
          action fn
        else do
          isD <- liftIO $ doesDirectoryExist fn
          when isD do
            content <- liftIO $ listDirectory  fn
            forConcurrently_ [ fn </> x | x <- content, not (matches ignore x) ] $ \e -> do
              go e


