{-# Language MultiWayIf #-}
module Data.Config.Suckless.Script.File where

import Data.Config.Suckless
import Data.Config.Suckless.Script.Internal

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Maybe
import Data.Either
import Data.Foldable
import System.Directory
import System.FilePath
import System.FilePattern
import Data.HashSet qualified as HS

import Prettyprinter

import Lens.Micro.Platform
import UnliftIO
import Control.Concurrent.STM qualified as STM
import Streaming.Prelude qualified as S

-- FIXME: skip-symlink
glob :: forall m . MonadIO m
     => [FilePattern]           -- ^ search patterns
     -> [FilePattern]           -- ^ ignore patterns
     -> FilePath                -- ^ directory
     -> (FilePath -> m Bool)    -- ^ file action
     -> m ()

glob pat ignore dir action = do
  q <- newTQueueIO
  void $ liftIO (async $ go q dir >> atomically (writeTQueue q Nothing))
  fix $ \next -> do
    atomically (readTQueue q) >>= \case
      Nothing -> pure ()
      Just x -> do
        r <- action x
        when r next

  where

    matches p f = or [ i ?== f | i <- p ]
    skip p = or [ i ?== p | i <- ignore ]

    go q f = do

      isD <- doesDirectoryExist f

      if not isD then do
        isF <- doesFileExist f
        when (isF && matches pat f && not (skip f)) do
          atomically $ writeTQueue q (Just f)
      else do
        co' <- (try @_ @IOError $ listDirectory f)
                 <&> fromRight mempty

        forConcurrently_ co' $ \x -> do
          let p = normalise (f </> x)
          unless (skip p) (go q p)

entries :: forall c m . ( IsContext c
                        , Exception (BadFormException c)
                        , MonadUnliftIO m)
         => MakeDictM c m ()
entries = do
  entry $ bindMatch "glob" $ \syn -> do

    (p,i,d) <- case syn of
                 [] -> pure (["**/*"], ["**/.*"], ".")

                 s@[StringLike d, ListVal (StringLikeList i) ] -> do
                    pure (i, [], d)

                 s@[StringLike d, ListVal (StringLikeList i), ListVal (StringLikeList e) ] -> do
                    pure (i, e, d)

                 _ -> throwIO (BadFormException @c nil)

    r <- S.toList_ $ glob p i d $ \fn -> do
          S.yield (mkStr @c fn) -- do
          pure True

    pure (mkList r)

