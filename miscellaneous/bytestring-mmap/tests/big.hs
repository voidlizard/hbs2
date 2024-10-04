import qualified Data.ByteString as S
import System.IO.Posix.MMap
import Control.Monad
import Text.Printf

main = do
      s <- unsafeMMapFile "/usr/obj/data/1G"
      print "This program should touch only 1 page per 100k"

      forM_ [0, (1024) .. S.length s-1] $ \n -> do
          printf "n=%d := %d\n" n (S.index s n)

