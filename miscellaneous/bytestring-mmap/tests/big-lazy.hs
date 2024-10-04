import qualified Data.ByteString.Lazy as L
import System.IO.Posix.MMap.Lazy
import Control.Monad
import Text.Printf
import System.Mem

main = do
      s <- unsafeMMapFile "/usr/obj/data/1G"
      go 0 s
   where
      go n s
        | L.null s = return ()
        | otherwise
          = do -- printf "%d\n" 
               L.head s `seq` return ()
               when (n `mod` 1000 == 0) $ do
                    performGC -- tune this value for when to run the GC
               go (n+1) (L.drop 4096 s)


--    forM_ [0, (1024) .. L.length s-1] $ \n -> do

