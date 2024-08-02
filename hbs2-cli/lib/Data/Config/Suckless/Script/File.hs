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

  go dir

  where

    ppat = zip (repeat True) pat
    spat = zip (repeat True) ignore

    go f = do
      isF <- liftIO $ doesFileExist f

      when isF do
        liftIO $ print f

      isD <- liftIO $ doesDirectoryExist f

      when isD do
        co <- liftIO (try @_ @IOError $ listDirectory f)
               <&> fromRight mempty

        let fns  = matchMany ppat [ (f </> x, x) | x <- co ]
        let stop = matchMany spat [ (f </> x, x) | x <- co ]
        let ss = HS.fromList $ (fmap (view _2))  stop

        -- matchMany :: [(a, FilePattern)] -> [(b, FilePath)] -> [(a, b, [String])]

        liftIO $ print spat
        liftIO $ print stop
        -- for_ co $ \p -> do
          -- liftIO $ print p
          -- unless (HS.member p ss) do
            -- go (f </> p)

  -- q <- newTQueueIO

  -- a <- liftIO $ async do
  --       flip fix seed $ \next items -> do
  --         case items of
  --           [] -> atomically (writeTQueue q Nothing)
  --           (p:rest) -> do
  --             isD <- liftIO (doesDirectoryExist p)
  --             if isD && not (skip p)  then do
  --               content <- liftIO (try @_ @IOError $ listDirectory  p)
  --                            <&> fromRight mempty

  --               let found = [ p </> x | x <- content ]

  --               forConcurrently_ found $ \f -> do
  --                 glob pat ignore f $ \fn -> do
  --                   atomically $ writeTQueue q (Just fn)
  --                   pure True

  --               next []

  --             else do
  --               isF <- liftIO (doesFileExist p)
  --               when (isF && matches pat p ) do
  --                 atomically (writeTQueue q (Just p))

  --               next rest

  -- fix \next -> do
  --   r <- atomically (readTQueue q)
  --   case r of
  --     Nothing -> pure ()
  --     Just e  -> void (action e) >> next

  -- where
  --   matches p f = or [ i ?== f | i <- p ]
  --   skip p = or [ i ?== p | i <- ignore ]

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

