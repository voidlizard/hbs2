module Main where

import HBS2.System.Logger.Simple

import Control.Monad
import Control.Concurrent.Async
import Lens.Micro.Platform

import Prettyprinter
-- import System.Log.FastLogger


main :: IO ()
main = do
  coo <- async $ do
    withSimpleLogger do
      setLogging @DEBUG id -- (set loggerTr ("debug: " <>))
      setLogging @INFO  id -- (set loggerTr ("info: " <>))
      forConcurrently_ [1..1000] $ \i -> do
        debug $ "DEBUG" <+> pretty i
        info  $ "INFO!" <+> pretty (i*1000)


  wait coo


