-- A non-copying cp based on mmap.

import System.IO.Posix.MMap
import qualified Data.ByteString as S

import Text.Printf
import Control.Exception
import System.CPUTime
import System.Cmd
import System.Directory

import System.Environment

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    v `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
    [f] <- getArgs

    putStrLn "mmap copy"
    time $ S.writeFile "file-1" =<< unsafeMMapFile   f
    putChar '\n'

    putStrLn "lazy copy"
    time $ S.writeFile "file-2" =<< S.readFile f
    putChar '\n'

    system $ "diff " ++ "file-1 " ++ "file-2"
    removeFile "file-1"
    removeFile "file-2"
