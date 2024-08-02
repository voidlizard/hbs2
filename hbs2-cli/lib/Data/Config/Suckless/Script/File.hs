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

import UnliftIO
import Control.Concurrent.STM qualified as STM
import Streaming.Prelude qualified as S

glob :: forall m . MonadIO m
     => [FilePattern]           -- ^ search patterns
     -> [FilePattern]           -- ^ ignore patterns
     -> FilePath                -- ^ directory
     -> (FilePath -> m Bool)    -- ^ file action
     -> m ()

glob pat ignore dir action = do
  let seed = [dir]

  flip fix seed $ \next items -> do
    case items of
      [] -> pure ()
      (p:rest) -> do
        isD <- liftIO (doesDirectoryExist p)
        if isD && not (skip p) then do
          content <- liftIO (try @_ @IOError $ listDirectory  p)
                       <&> fromRight mempty
          let found = [ p </> x | x <- content ]
          next (rest <> found)
        else do
          isF <- liftIO (doesFileExist p)
          when (matches pat p && not (skip p) ) do
            void $ action p
          next rest

  where
    matches p f = or [ i ?== f | i <- p ]
    skip p = or [ i ?== p | i <- ignore ]

entries :: forall c m . ( IsContext c
                        , Exception (BadFormException c)
                        , MonadUnliftIO m)
         => MakeDictM c m ()
entries = do
  entry $ bindMatch "glob" $ \syn -> do

    (p,i,d) <- case syn of
                 [] -> pure (["*"], [], ".")

                 [StringLike d, StringLike i] -> do
                    pure ([i], [], d)

                 [StringLike d, StringLike i, StringLike e] -> do
                    pure ([i], [e], d)

                 [StringLike d, ListVal (StringLikeList i), ListVal (StringLikeList e)] -> do
                    pure (i, e, d)

                 _ -> throwIO (BadFormException @c nil)

    r <- S.toList_ $ glob p i d $ \fn -> do
          S.yield (mkStr @c fn) -- do
          pure True

    pure (mkList r)

