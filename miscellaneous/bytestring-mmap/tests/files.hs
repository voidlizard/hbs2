import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import System.IO.Posix.MMap
import qualified System.IO.Posix.MMap.Lazy as LM

import System.Directory
import System.Posix.Files
import System.IO
import System.FilePath
import Control.Monad
import Control.Applicative
import Text.Printf
import System.Cmd
import System.Exit
import System.Mem
import Control.Exception

main = do
    print "Testing Lazy.mmap == Strict.mmap == Strict.ByteString.readFile"
    system "find /home/dons/ghc/ -type f > files_to_read"
    always (removeFile "files_to_read") $ do
        fs <- lines <$> readFile "files_to_read"

    {-
        ss <- getDirectoryContents dir
        fs <- filterM (\f -> do st <- getFileStatus (dir </> f)
                                return (not $ isDirectory st)) ss
    -}

        printf "Comparing %d files\n" (length fs)
        forM_ (zip [1..] fs) $ \(i,f) -> do
                t <- eq f
                if t
                   then when (i `mod` 1000 == 0) $ putStr "Ok. " >> hFlush stdout
                   else exitWith (ExitFailure 1)

        print "All good."

  where 
    always = flip finally

eq f = do
        m  <-    unsafeMMapFile f
        lm <- LM.unsafeMMapFile f
        s  <- S.readFile f
        return (m == s && L.fromChunks [m] == lm)
