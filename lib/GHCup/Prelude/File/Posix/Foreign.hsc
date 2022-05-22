{-# LANGUAGE PatternSynonyms #-}

module GHCup.Prelude.File.Posix.Foreign where

import Data.Bits
import Data.List (foldl')
import Foreign.C.Types

#include <limits.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

newtype DirType = DirType Int deriving (Eq, Show)
data Flags = Flags Int | UnsupportedFlag String deriving (Eq, Show)

unFlags :: Flags -> Int
unFlags (Flags i) = i
unFlags (UnsupportedFlag name) = error (name ++ " is not supported on this platform")

-- |Returns @True@ if posix-paths was compiled with support for the provided
-- flag. (As of this writing, the only flag for which this check may be
-- necessary is 'oCloexec'; all other flags will always yield @True@.)
isSupported :: Flags -> Bool
isSupported (Flags _) = True
isSupported _ = False

-- |@O_CLOEXEC@ is not supported on every POSIX platform. Use
-- @'isSupported' oCloexec@ to determine if support for @O_CLOEXEC@ was
-- compiled into your version of posix-paths. (If not, using @oCloexec@ will
-- throw an exception.)
oCloexec :: Flags
#ifdef O_CLOEXEC
oCloexec = Flags #{const O_CLOEXEC}
#else
{-# WARNING oCloexec
    "This version of posix-paths was compiled without @O_CLOEXEC@ support." #-}
oCloexec = UnsupportedFlag "O_CLOEXEC"
#endif



-- If these enum declarations occur earlier in the file, haddock
-- gets royally confused about the above doc comments.
-- Probably http://trac.haskell.org/haddock/ticket/138

#{enum DirType, DirType, DT_BLK, DT_CHR, DT_DIR, DT_FIFO, DT_LNK, DT_REG, DT_SOCK, DT_UNKNOWN}

#{enum Flags, Flags, O_APPEND, O_ASYNC, O_CREAT, O_DIRECTORY, O_EXCL, O_NOCTTY, O_NOFOLLOW, O_NONBLOCK, O_RDONLY, O_WRONLY, O_RDWR, O_SYNC, O_TRUNC}

pathMax :: Int
pathMax = #{const PATH_MAX}

unionFlags :: [Flags] -> CInt
unionFlags = fromIntegral . foldl' ((. unFlags) . (.|.)) 0

