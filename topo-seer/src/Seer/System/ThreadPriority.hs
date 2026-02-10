{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Windows thread priority boost and CPU affinity for the main render thread.
--
-- On Windows, the GHC runtime creates many OS worker threads for capabilities.
-- Every safe FFI call (including all SDL2 bindings) temporarily releases the
-- calling thread's capability, creating a window where other OS threads can
-- preempt the main bound thread.  During CPU-intensive terrain generation the
-- main thread can be starved for seconds.
--
-- Boosting the main thread to @THREAD_PRIORITY_ABOVE_NORMAL@ ensures the OS
-- scheduler always prefers it over normal-priority worker threads, eliminating
-- multi-second render stalls.
--
-- Pinning the main thread to CPU core 0 via @SetThreadAffinityMask@ works in
-- concert with GHC's @-qa@ RTS flag (which maps capability /i/ → core /i/).
-- Since no Haskell actors are scheduled on capability 0, the render thread
-- effectively owns core 0.
--
-- On non-Windows platforms these functions are no-ops.

module Seer.System.ThreadPriority
  ( boostMainThreadPriority
  , pinMainThreadToCore0
  ) where

#if defined(mingw32_HOST_OS)

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import System.IO (hPutStrLn, stderr)

-- | Pseudo-handle representing the current thread (value @-2@).
type HANDLE = Ptr ()

-- | Windows @DWORD_PTR@ used for affinity masks.
type DWORD_PTR = Word

foreign import ccall unsafe "windows.h GetCurrentThread"
  c_GetCurrentThread :: IO HANDLE

foreign import ccall unsafe "windows.h SetThreadPriority"
  c_SetThreadPriority :: HANDLE -> Int -> IO Word8
  -- BOOL is typedef int on Windows; we read it as Word8 (0 = failure).

foreign import ccall unsafe "windows.h SetThreadAffinityMask"
  c_SetThreadAffinityMask :: HANDLE -> DWORD_PTR -> IO DWORD_PTR
  -- Returns previous affinity mask, or 0 on failure.

-- | @THREAD_PRIORITY_ABOVE_NORMAL@ (value 1).
threadPriorityAboveNormal :: Int
threadPriorityAboveNormal = 1

-- | Boost the calling (main) thread to above-normal priority.
-- Logs a warning to stderr on failure; never throws.
boostMainThreadPriority :: IO ()
boostMainThreadPriority = do
  h <- c_GetCurrentThread
  ok <- c_SetThreadPriority h threadPriorityAboveNormal
  if ok /= 0
    then pure ()
    else hPutStrLn stderr "warning: SetThreadPriority(ABOVE_NORMAL) failed"

-- | Pin the calling (main) thread to CPU core 0.
--
-- Sets the thread affinity mask to @1@ (bit 0 only), restricting the OS
-- scheduler to core 0.  Combined with GHC's @-qa@ flag (capability /i/ →
-- core /i/) and no actors on capability 0, this gives the render thread
-- exclusive access to core 0.
--
-- Logs a warning to stderr on failure; never throws.
pinMainThreadToCore0 :: IO ()
pinMainThreadToCore0 = do
  h <- c_GetCurrentThread
  prev <- c_SetThreadAffinityMask h 1  -- bitmask 0x1 = core 0 only
  if prev /= 0
    then pure ()
    else hPutStrLn stderr "warning: SetThreadAffinityMask(core 0) failed"

#else

-- | No-op on non-Windows platforms.
boostMainThreadPriority :: IO ()
boostMainThreadPriority = pure ()

-- | No-op on non-Windows platforms.
pinMainThreadToCore0 :: IO ()
pinMainThreadToCore0 = pure ()

#endif
