{-# LANGUAGE ForeignFunctionInterface #-}
module System.GetRUsage (getUserspaceCPUTime) where

#include "HsBaseConfig.h"

import Foreign
import Foreign.C

#if !defined(mingw32_HOST_OS) && !defined(irix_HOST_OS)
# if HAVE_SYS_RESOURCE_H
#  include <sys/resource.h>
# endif

realToInteger :: Real a => a -> Integer
realToInteger ct = round (realToFrac ct :: Double)
#endif

-- | Returns the userspace portion of the execution time of the current
-- program, in picoseconds.
--
-- This might fall back to total time if the system is not compliant to
-- POSIX-1.2001, SVr4 or 4.3BSD.
getUserspaceCPUTime :: IO Integer
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) \
    && defined(HAVE_GETRUSAGE) && ! irix_HOST_OS && ! solaris2_HOST_OS

getUserspaceCPUTime =
  allocaBytes (#const sizeof(struct rusage)) $ \p_rusage -> do
    throwErrnoIfMinus1_ "getrusage" $ getrusage (#const RUSAGE_SELF) p_rusage

    let ru_utime = (#ptr struct rusage, ru_utime) p_rusage
    u_sec  <- (#peek struct timeval,tv_sec)  ru_utime :: IO CTime
    u_usec <- (#peek struct timeval,tv_usec) ru_utime :: IO CSUSeconds
    return ((realToInteger u_sec * 1000000 + realToInteger u_usec) * 1000000)

foreign import ccall unsafe "HsBase.h getrusage"
    getrusage :: CInt -> Ptr () -> IO CInt

#else // as a fallback
getUserspaceCPUTime = getCPUTime
#endif
