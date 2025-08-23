{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
module HBS2.Storage.NCQ3.Internal.Fsync where

import HBS2.Storage.NCQ3.Internal.Prelude

import Foreign.C.Types
import System.Posix.Types
import System.Posix.Unistd (fileSynchronise)
import Foreign.C.Error (throwErrnoIfMinus1_)

#ifdef darwin_HOST_OS

foreign import capi unsafe "fcntl.h fcntl"
  c_fcntl_raw :: CInt -> CInt -> CInt -> IO CInt

foreign import capi unsafe "fcntl.h value F_FULLFSYNC"
  f_FULLFSYNC :: CInt

c_fcntl :: CInt -> CInt -> CInt -> IO ()
c_fcntl fd cmd arg = do
--  debug $ "c_fcntl" <+> viaShow fd <+> viaShow cmd <+> viaShow arg
  throwErrnoIfMinus1_ "fcntl(F_FULLFSYNC)" (c_fcntl_raw fd cmd arg)
  pure ()

#endif

fileSynchronisePortable :: Fd -> IO ()
fileSynchronisePortable fd@(Fd fdi) = do
#if defined(darwin_HOST_OS)
  c_fcntl fdi f_FULLFSYNC 0
#else
  fileSynchronise fd
#endif
{-# INLINE fileSynchronisePortable #-}



