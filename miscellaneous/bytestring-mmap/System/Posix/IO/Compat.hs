{-# LANGUAGE CPP #-}
module System.Posix.IO.Compat where

import qualified System.Posix as Unix


openFd :: FilePath -> Unix.OpenMode -> Unix.OpenFileFlags -> IO Unix.Fd
#if MIN_VERSION_unix(2,8,0)
openFd = Unix.openFd
#else
openFd file openMode = Unix.openFd file openMode Nothing
#endif
