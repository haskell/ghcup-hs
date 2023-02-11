{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}


module GHCup.Prelude.File.Posix.Traversals (
-- lower-level stuff
  readDirEnt
, readDirEntPortable
, openDirStreamPortable
, closeDirStreamPortable
, unpackDirStream
, DirStreamPortable
) where

#include <limits.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif
import GHCup.Prelude.File.Posix.Foreign

import Unsafe.Coerce (unsafeCoerce)
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Posix
import Foreign (alloca)
import System.Posix.Internals (peekFilePath)
import System.FilePath





----------------------------------------------------------
-- dodgy stuff

data {-# CTYPE "DIR" #-} CDir
data {-# CTYPE "struct dirent" #-} CDirent

-- Posix doesn't export DirStream, so to re-use that type we need to use
-- unsafeCoerce.  It's just a newtype, so this is a legitimate usage.
-- ugly trick.
unpackDirStream :: DirStream -> Ptr CDir
unpackDirStream = unsafeCoerce

-- the __hscore_* functions are defined in the unix package.  We can import them and let
-- the linker figure it out.
foreign import ccall unsafe "__hscore_readdir"
  c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt

foreign import ccall unsafe "__hscore_free_dirent"
  c_freeDirEnt  :: Ptr CDirent -> IO ()

foreign import ccall unsafe "__hscore_d_name"
  c_name :: Ptr CDirent -> IO CString

foreign import capi unsafe "dirutils.h __posixdir_d_type"
  c_type :: Ptr CDirent -> IO DirType

----------------------------------------------------------
-- less dodgy but still lower-level


readDirEnt :: DirStream -> IO (DirType, FilePath)
readDirEnt (unpackDirStream -> dirp) =
  alloca $ \ptr_dEnt  -> loop ptr_dEnt
 where
  loop ptr_dEnt = do
    resetErrno
    r <- c_readdir dirp ptr_dEnt
    if r == 0
       then do
         dEnt <- peek ptr_dEnt
         if dEnt == nullPtr
            then return (dtUnknown, mempty)
            else do
                 dName <- c_name dEnt >>= peekFilePath
                 dType <- c_type dEnt
                 c_freeDirEnt dEnt
                 return (dType, dName)
       else do
         errno <- getErrno
         if errno == eINTR
            then loop ptr_dEnt
            else do
                 let (Errno eo) = errno
                 if eo == 0
                    then return (dtUnknown, mempty)
                    else throwErrno "readDirEnt"


newtype DirStreamPortable = DirStreamPortable (FilePath, DirStream)

openDirStreamPortable :: FilePath -> IO DirStreamPortable
openDirStreamPortable fp = do
  dirs <- openDirStream fp
  pure $ DirStreamPortable (fp, dirs)

closeDirStreamPortable :: DirStreamPortable -> IO ()
closeDirStreamPortable (DirStreamPortable (_, dirs)) = closeDirStream dirs

readDirEntPortable :: DirStreamPortable -> IO (DirType, FilePath)
readDirEntPortable (DirStreamPortable (basedir, dirs)) = do
  (dt, fp) <- readDirEnt dirs
  case (dt, fp) of
    (DirType #{const DT_BLK}, _)     -> pure (dt, fp)
    (DirType #{const DT_CHR}, _)     -> pure (dt, fp)
    (DirType #{const DT_DIR}, _)     -> pure (dt, fp)
    (DirType #{const DT_FIFO}, _)    -> pure (dt, fp)
    (DirType #{const DT_LNK}, _)     -> pure (dt, fp)
    (DirType #{const DT_REG}, _)     -> pure (dt, fp)
    (DirType #{const DT_SOCK}, _)    -> pure (dt, fp)
    (DirType #{const DT_UNKNOWN}, _) -> pure (dt, fp)
    (_, _)
      | fp /= "" -> do
          stat <- getSymbolicLinkStatus (basedir </> fp)
          pure $ (, fp) $ if | isBlockDevice stat     -> DirType #{const DT_BLK}
                             | isCharacterDevice stat -> DirType #{const DT_CHR}
                             | isDirectory stat       -> DirType #{const DT_DIR}
                             | isNamedPipe stat       -> DirType #{const DT_FIFO}
                             | isSymbolicLink stat    -> DirType #{const DT_LNK}
                             | isRegularFile stat     -> DirType #{const DT_REG}
                             | isSocket stat          -> DirType #{const DT_SOCK}
                             | otherwise              -> DirType #{const DT_UNKNOWN}

