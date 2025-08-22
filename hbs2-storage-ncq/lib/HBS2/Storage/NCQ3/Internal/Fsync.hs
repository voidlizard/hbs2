{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
module HBS2.Storage.NCQ3.Internal.Fsync where

import Foreign.C.Types
import System.Posix.Types
import System.Posix.Unistd (fileSynchronise)

foreign import capi unsafe "unistd.h fsync"
  c_fsync :: CInt -> IO CInt

#if defined(darwin_HOST_OS)
foreign import capi unsafe "fcntl.h fcntl"
  c_fcntl :: CInt -> CInt -> CInt -> IO CInt

foreign import capi unsafe "fcntl.h value F_FULLFSYNC"
  f_FULLFSYNC :: CInt
#endif

fileSynchronisePortable :: Fd -> IO ()
fileSynchronisePortable fd@(Fd fdi) = do
#if defined(darwin_HOST_OS)
  c_fcntl fdi f_FULLFSYNC 0
#else
  fileSynchronise fd
#endif
{-# INLINE fileSynchronisePortable #-}


