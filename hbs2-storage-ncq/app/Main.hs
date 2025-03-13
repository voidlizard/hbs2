module Main where

import HBS2.Prelude.Plated
import HBS2.Storage.NCQ

import Data.Config.Suckless.Script

import System.Environment
import UnliftIO


runTop :: forall c m . ( IsContext c
                       , NCQPerks m
                       , MonadUnliftIO m
                       , Exception (BadFormException c)
                       ) => [Syntax c] -> m ()
runTop forms = do


  let dict = makeDict @c do

       internalEntries

       entry $ bindMatch "--help" $ nil_ \case
         HelpEntryBound what -> helpEntry what
         [StringLike s]      -> helpList False (Just s)
         _                   -> helpList False Nothing

       entry $ bindMatch "ncq:init" $ nil_ $ \case
        [ StringLike path ] -> do
          ncqStorageInit path

        e -> throwIO $ BadFormException @c (mkList e)

  tvd  <- newTVarIO dict
  runEval tvd forms >>= eatNil display


main :: IO ()
main = do
  argz <- getArgs

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
     & either (error.show) pure

  runTop forms


