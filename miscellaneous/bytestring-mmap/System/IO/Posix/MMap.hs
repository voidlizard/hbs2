{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    :  System.IO.Posix.MMap
-- Copyright :  (c) Galois, Inc. 2007
-- License   :  BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: non-portable -- posix only
--
-- mmap a file or device into memory as a strict ByteString.
--
module System.IO.Posix.MMap (

      -- $mmap_intro
      -- $mmap_unmap

      -- * Memory mapped files
      unsafeMMapFile -- :: FilePath -> IO ByteString

-- $mmap_intro
--
-- 'unsafeMMapFile' mmaps a file or device into memory as a strict
-- 'ByteString'. The file is not actually copied strictly into memory,
-- but instead pages from the file will be loaded into the address
-- space on demand.
--
-- We can consider mmap as lazy IO pushed into the virtual memory
-- subsystem.
--
-- The file is mapped using MAP_SHARED: modifications to the file
-- will be immediately shared with any other process accessing the
-- file. This has no effect from the Haskell point of view, since
-- ByteStrings are treated as immutable values.
--
-- However, if the file is written to by any other process on the
-- system while it is in use in Haskell, those changes will be
-- immediately reflected on the Haskell side, destroying referential
-- transparency.
--
-- It is only safe to mmap a file if you know you are the sole user.
--
-- For more details about mmap, and its consequences, see:
-- 
-- * <http://opengroup.org/onlinepubs/009695399/functions/mmap.html>
--
-- * <http://www.gnu.org/software/libc/manual/html_node/Memory_002dmapped-I_002fO.html>
--

-- $mmap_unmap
--
-- When the entire file is out of scope, the Haskell storage manager
-- will call munmap to free the file, using a finaliser. Until then, as
-- much of the file as you access will be allocated.
--
-- Note that the Haskell storage manager doesn't know how large a
-- resource is associated with an mmapped file. If you allocate many
-- such files, the garbage collector will only see the 'ForeignPtr's
-- that have been allocated, not the corresponding ByteArrays. The
-- result will be that the GC runs less often that you hoped, as it 
-- looks like only a few bytes have been allocated on the Haskell heap.
-- 
-- Use of 'performGC' or 'finalizeForeignPtr' when you know that
-- the object is going out of scope can ensure that resources are
-- released appropriately.
--

    ) where

import System.IO.Posix.MMap.Internal

-- import System.IO
-- import qualified System.IO as IO
import Foreign.Ptr

import Control.Exception
import Data.ByteString

import System.Posix hiding (openFd)
import System.Posix.IO.Compat (openFd)

-- | The 'unsafeMMapFile' function maps a file or device into memory,
-- returning a strict 'ByteString' that accesses the mapped file.
-- If the mmap fails for some reason, an error is thrown.
--
-- Memory mapped files will behave as if they were read lazily -- 
-- pages from the file will be loaded into memory on demand.
--
-- The storage manager is used to free the mapped memory. When
-- the garbage collector notices there are no further references to the 
-- mapped memory, a call to munmap is made. It is not necessary to do
-- this yourself. In tight memory situations, it may be profitable to
-- use 'performGC' or 'finalizeForeignPtr' to force an unmap.
--
-- Note: this operation may break referential transparency! If 
-- any other process on the system changes the file when it is mapped
-- into Haskell, the contents of your 'ByteString' will change.
--
unsafeMMapFile :: FilePath -> IO ByteString
unsafeMMapFile f = do
    fd   <- openFd f ReadOnly defaultFileFlags
    always (closeFd fd) $ do
        stat <- getFdStatus fd
        let size = fromIntegral (fileSize stat)
        if size <= 0
            then return empty -- BSD mmap won't accept a length of zero
            else do
        ptr <- c_mmap size (fromIntegral fd)
        if ptr == nullPtr
            then error "System.IO.Posix.MMap.mmapFile: unable to mmap file"
            else unsafePackMMapPtr ptr size

  where always = flip finally
