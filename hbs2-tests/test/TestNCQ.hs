{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple.ANSI
import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import NCQTestCommon
import NCQ3
import System.Environment
import UnliftIO

{- HLINT ignore "Functor law" -}

main :: IO ()
main = do

  tvd <- newTVarIO mempty

  let dict = makeDict @C do


        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList True (Just s)
          _                   -> helpList True Nothing


        entry $ bindMatch "--run" $ \case
          (StringLike what : args) -> liftIO do

            liftIO (readFile what)
              <&> parseTop
              >>= either (error.show) pure
              >>= \syn -> do
                    runTM tvd do

                      for_ (zip [1..] args) $ \(i,a) -> do
                        let n = Id ("$" <> fromString (show i))
                        SC.bind n a

                      SC.bind "$argv" (mkList args)

                      evalTop syn

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "debug" $ nil_ \case

          [ LitBoolVal False ] -> do
             setLoggingOff @DEBUG

          [ StringLike "off" ] -> do
             setLoggingOff @DEBUG

          _ ->
             setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

        entry $ bindMatch "test:root" $ nil_ $ \case
          [ s@(StringLike _) ] -> do
            SC.bind "test:root" s

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:dir:keep" $ nil_ $ \case
          [] -> SC.bind "test:dir:keep" (mkBool True)
          e -> throwIO $ BadFormException @C (mkList e)

-- NCQ3 tests

        ncq3Tests

        -- hidden do
        internalEntries
        entry $ bindMatch "#!" $ nil_ $ const none

  setupLogger

  argz <- liftIO getArgs

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  atomically $ writeTVar tvd dict

  (runEval tvd forms >>= eatNil display)
    `finally` flushLoggers


