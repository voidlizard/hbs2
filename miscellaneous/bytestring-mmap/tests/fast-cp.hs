import qualified System.IO.Posix.MMap.Lazy     as L
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString               as S

import Foreign.ForeignPtr
import System.Environment
import System.IO
import Control.Exception

main = do
    [f,g] <- getArgs
    writeFile' g =<< L.unsafeMMapFile f

--
-- An implementation of writeFile for bytestrings that 
-- that finalises chunks as they go out the door.
--
writeFile' :: FilePath -> L.ByteString -> IO ()
writeFile' f txt = bracket (openBinaryFile f WriteMode) hClose (\hdl -> hPut hdl txt)

hPut :: Handle -> L.ByteString -> IO ()
hPut h cs = L.foldrChunks (\chunk rest -> do S.hPut h chunk
                                             unmap chunk
                                             rest)
                          (return ()) cs

    where unmap c = finalizeForeignPtr fp where (fp,_,_) = S.toForeignPtr c
