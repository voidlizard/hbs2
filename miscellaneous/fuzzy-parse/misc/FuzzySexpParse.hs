module Main where

import Data.Text.Fuzzy.SExp
import Data.Text.IO qualified as IO
import Data.Text qualified as Text
import Data.Either
import System.TimeIt
import Control.Monad.Except
import Data.Functor
import Data.Function
import Data.Fixed
import Prettyprinter
import System.IO

main :: IO ()
main = do
  s <- IO.getContents

  (tt,toks) <- timeItT do
                pure (tokenizeSexp s)

  (pt,top) <- timeItT  do
                runExceptT (parseTop @() s) <&> either (error.show) id

  print (vcat (fmap pretty top))

  hPrint stderr $ pretty (Text.length s) <+> "chars, parsed in" <+> viaShow (realToFrac pt  :: Fixed E6)



