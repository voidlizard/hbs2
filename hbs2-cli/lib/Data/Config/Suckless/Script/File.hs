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

import Lens.Micro.Platform
import UnliftIO
import Control.Concurrent.STM qualified as STM
import Streaming.Prelude qualified as S

import Data.List.Split (chunksOf)

glob :: forall m . MonadIO m
     => [FilePattern]           -- ^ search patterns
     -> [FilePattern]           -- ^ ignore patterns
     -> FilePath                -- ^ directory
     -> (FilePath -> m Bool)    -- ^ file action
     -> m ()

glob pat ignore dir action = do

  q <- newTQueueIO

  void $ liftIO (async $ go q dir >> atomically (writeTQueue q Nothing))

  flip runContT pure do
    callCC \exit -> do
      fix $ \next -> do
        e <- atomically do
                void (peekTQueue q)
                STM.flushTQueue q

        for_ e $ \case
          Nothing -> exit ()
          Just x -> do
            r <- lift (action x)
            unless r (exit ())

        next

  where

    matches p f = or [ i ?== f | i <- p ]
    skip p = or [ i ?== p | i <- ignore ]

    go q f = do

      isD <- doesDirectoryExist f

      if not isD then do
        isF <- doesFileExist f
        when (isF && matches pat f) do
          atomically $ writeTQueue q (Just f)
      else do
        co' <- (try @_ @IOError $ listDirectory f)
                 <&> fromRight mempty

        let co = [ normalise (f </> x)  | x <- co' ]
                   & filter (not . skip)

        forConcurrently_ co (go q)


entries :: forall c m . ( IsContext c
                        , Exception (BadFormException c)
                        , MonadUnliftIO m)
         => MakeDictM c m ()
entries = do
  entry $ bindMatch "glob" $ \syn -> do

    (p,i,d) <- case syn of
                 [] -> pure (["*"], [], ".")

                 [StringLike d, StringLike i, StringLike e] -> do
                    pure ([i], [e], d)

                 [StringLike d, ListVal (StringLikeList i), ListVal (StringLikeList e)] -> do
                    pure (i, e, d)

                 _ -> throwIO (BadFormException @c nil)

    r <- S.toList_ $ glob p i d $ \fn -> do
          S.yield (mkStr @c fn) -- do
          pure True

    pure (mkList r)

