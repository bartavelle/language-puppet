{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Puppet.Utils (mGetExecutablePath) where

-- copy pasted from base 4.6.0.0

import Foreign.C
import Foreign.Marshal.Array
import System.Posix.Internals

foreign import ccall unsafe "readlink" c_readlink :: CString -> CString -> CSize -> IO CInt

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
    allocaArray0 4096 $ \buf -> do
        withFilePath file $ \s -> do
            len <- throwErrnoPathIfMinus1 "readSymbolicLink" file $
                   c_readlink s buf 4096
            peekFilePathLen (buf,fromIntegral len)

-- | Returns the absolute pathname of the current executable.
--
-- Note that for scripts and interactive sessions, this is the path to
-- the interpreter (e.g. ghci.)
-- (Stolen from base 4.6.0)
mGetExecutablePath :: IO FilePath
mGetExecutablePath = readSymbolicLink $ "/proc/self/exe"

