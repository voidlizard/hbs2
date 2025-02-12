{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import Data.Config.Suckless.Script
import Data.Config.Suckless.Script.File as SF

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


  case cli of
    [ListVal [SymbolVal "stdin"]] -> do
      what <- liftIO getContents
                >>= either (error.show) pure . parseTop

      run dict what >>= eatNil display

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

