import System.Directory
import System.IO.Posix.MMap
import System.Posix.Files
import System.FilePath
import Control.Monad
import Control.Applicative

main = do
--  let dir = "/home/dons/lambdabot/_darcs/patches"
--  ss <- getDirectoryContents dir
--  fs <- filterM (\f -> do st <- getFileStatus (dir </> f)
--                          return (not $ isDirectory st)) ss

    fs <- lines <$> readFile "/tmp/files"
    mapM_ unsafeMMapFile fs
