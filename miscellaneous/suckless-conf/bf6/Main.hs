{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import Data.Config.Suckless.Script
import Data.Config.Suckless.Script.File as SF

import Safe
import System.Environment
import System.IO qualified as IO
import UnliftIO


main :: IO ()
main = do

  cli <- getArgs <&> unlines . fmap unwords . splitForms
           >>= either (error.show) pure . parseTop

  let dict = makeDict do

        internalEntries
        SF.entries

        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

        entry $ bindMatch "debug:cli:show" $ nil_ \case
          _ -> display cli

        hidden do
          entry $ bindMatch "#!" $ nil_ $ const do
            pure ()

  case cli of

    [ListVal (SymbolVal "#!" :_)] -> do
      pure ()

    [ListVal [SymbolVal "--run", StringLike fn]] -> do
      what <- liftIO $ readFile fn
                >>= either (error.show) pure . parseTop

      run dict what >>= eatNil display

    [ListVal [SymbolVal "stdin"]] -> do
      what <- liftIO getContents
                >>= either (error.show) pure . parseTop

      run dict what >>= eatNil display

    [ListVal (SymbolVal "file" : s@(StringLike fn : args))] -> do
      what <- liftIO (IO.readFile fn)
                >>= either (error.show) pure . parseTop

      runM dict do
        bindCliArgs s
        void $ evalTop what

    [] -> do
      eof <- liftIO IO.isEOF
      if eof then
        void $ run dict [mkForm  "help" []]
      else do
        what <- liftIO getContents
                  >>= either (error.show) pure . parseTop

        run dict what >>= eatNil display

    _ -> do
      run dict cli >>= eatNil display

