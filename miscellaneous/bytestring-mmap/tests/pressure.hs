-- A non-copying cp based on mmap.

import System.IO.Posix.MMap
import Control.Monad
import System.Mem
import qualified Data.ByteString as S
import Text.Printf
import Control.Exception
import System.CPUTime

main = do

    --should run in constant space, and be faster:
    time $ forM_ [0..1000] $ \_ -> do
        unsafeMMapFile "/usr/share/dict/words"

    putStrLn "\nShould be faster than:\n"

    --should run in constant space:
    time $ forM_ [0..1000] $ \_ -> do
        S.readFile "/usr/share/dict/words"


time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    v `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v
