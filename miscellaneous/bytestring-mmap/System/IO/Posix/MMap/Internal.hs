{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    :  System.IO.Posix.MMap.Internal
-- Copyright :  (c) Galois, Inc. 2007
-- License   :  BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: non-portable -- posix only
--
-- Low level mmap access.
--
module System.IO.Posix.MMap.Internal (

    -- * Converting an mmapped pointer to a 'ByteString'
    unsafePackMMapPtr,      -- :: Ptr Word8 -> CSize -> IO ByteString

    -- * Low level bindings
    c_mmap,                 -- :: CSize -> CInt -> IO (Ptr Word8)
    c_munmap                -- :: Ptr Word8 -> CSize -> IO CInt

  ) where

import System.IO
import qualified System.IO as IO
import Foreign.C.Types
import Foreign.Ptr
import qualified Foreign.Concurrent as FC

import Control.Monad
import Data.Word
import Data.ByteString.Internal
-- import Data.ByteString

-- | Create a bytestring from a memory mapped Ptr.
-- A finalizer will be associated with the resource, that will call
-- munmap when the storage manager detects that the resource is no longer
-- in use.
unsafePackMMapPtr :: Ptr Word8 -> CSize -> IO ByteString
unsafePackMMapPtr p s = do
    fp <- FC.newForeignPtr p $ do
                 v <- c_munmap p s
                 when (v == -1) $ IO.hPutStrLn stderr $
                         "System.IO.Posix.MMap: warning, failed to unmap "
                          ++ show s ++" bytes at "++show p
    return (fromForeignPtr fp 0 (fromIntegral s))
{-# INLINE unsafePackMMapPtr #-}

foreign import ccall unsafe "hs_bytestring_mmap.h hs_bytestring_mmap"
    c_mmap   :: CSize -> CInt -> IO (Ptr Word8)

foreign import ccall unsafe "hs_bytestring_mmap.h munmap"
    c_munmap :: Ptr Word8 -> CSize -> IO CInt
