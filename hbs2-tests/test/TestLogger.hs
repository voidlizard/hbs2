module Main where

import HBS2.System.Logger.Simple

import Control.Monad
import Control.Concurrent.Async

import System.Log.FastLogger
import Prettyprinter

main :: IO ()
main = do
  withSimpleLogger do
    replicateConcurrently_ 1000 do
      debug $ "DEBUG" <+> pretty 1000





