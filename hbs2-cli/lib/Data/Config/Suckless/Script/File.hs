module Data.Config.Suckless.Script.File where

import Data.Config.Suckless
import Data.Config.Suckless.Script.Internal

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
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

glob pat ignore dir action = runContT (callCC $ \exit -> go exit dir) pure

  where
    matches p f = or [ i ?== f | i <- p ]

    go exit fn = do

      let skip = or [ i ?== fn | i <- ignore ]

      unless skip do
        isF <- liftIO $ doesFileExist fn
        if isF then do
          when (matches pat fn) do
            continue <- lift (action fn)
            unless continue (exit ())
        else do
          isD <- liftIO $ doesDirectoryExist fn
          if not isD then exit ()
          else do
            content <- liftIO (try @_ @IOError $ listDirectory  fn)
                         <&> fromRight mempty
            -- TODO: memory-hungry
            for_ [ fn </> x | x <- content, not (matches ignore x) ] $ \e -> do
              go exit e

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

