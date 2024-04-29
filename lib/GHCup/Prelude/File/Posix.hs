{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE MultiWayIf  #-}
{-# LANGUAGE CApiFFI #-}

{-|
Module      : GHCup.Utils.File.Posix
Description : File and directory handling for unix
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX
-}
module GHCup.Prelude.File.Posix where

import           Conduit
import           Control.Exception.Safe
import           Foreign.C.String
import           Foreign.C.Error
import           Foreign.C.Types
import           System.IO                      ( hClose, hSetBinaryMode )
import           System.IO.Error      hiding    ( catchIOError )
import           System.FilePath
import           System.Directory               ( removeFile, pathIsSymbolicLink, getSymbolicLinkTarget, doesPathExist )
import           System.Posix.Error             ( throwErrnoPathIfMinus1Retry )
import           System.Posix.Internals         ( withFilePath )
import           System.Posix.Files
import           System.Posix.Types


import qualified System.Posix.Directory        as PD
import qualified System.Posix.Files            as PF
import qualified System.Posix.IO               as SPI
import qualified System.Posix as Posix
import qualified GHCup.Prelude.File.Posix.Foreign as FD
import GHCup.Prelude.File.Posix.Traversals
import GHC.IO.Exception (IOException(ioe_type), IOErrorType (..))

import qualified Data.Conduit.Combinators as C


-- | On unix, we can use symlinks, so we just get the
-- symbolic link target.
--
-- On windows, we have to emulate symlinks via shims,
-- see 'createLink'.
getLinkTarget :: FilePath -> IO FilePath
getLinkTarget = getSymbolicLinkTarget


-- | Checks whether the path is a link.
pathIsLink :: FilePath -> IO Bool
pathIsLink = pathIsSymbolicLink


chmod_755 :: MonadIO m => FilePath -> m ()
chmod_755 fp = do
  let exe_mode =
          nullFileMode
            `unionFileModes` ownerExecuteMode
            `unionFileModes` ownerReadMode
            `unionFileModes` ownerWriteMode
            `unionFileModes` groupExecuteMode
            `unionFileModes` groupReadMode
            `unionFileModes` otherExecuteMode
            `unionFileModes` otherReadMode
  liftIO $ setFileMode fp exe_mode


-- |Default permissions for a new file.
newFilePerms :: FileMode
newFilePerms =
  ownerWriteMode
    `unionFileModes` ownerReadMode
    `unionFileModes` groupWriteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherWriteMode
    `unionFileModes` otherReadMode


-- | Checks whether the binary is a broken link.
isBrokenSymlink :: FilePath -> IO Bool
isBrokenSymlink fp = do
  try (pathIsSymbolicLink fp) >>= \case
    Right True -> do
      let symDir = takeDirectory fp
      tfp <- getSymbolicLinkTarget fp
      not <$> doesPathExist
        -- this drops 'symDir' if 'tfp' is absolute
        (symDir </> tfp)
    Right b -> pure b
    Left e | isDoesNotExistError e -> pure False
           | otherwise -> throwIO e

copyFile :: FilePath   -- ^ source file
         -> FilePath   -- ^ destination file
         -> Bool       -- ^ fail if file exists
         -> IO ()
copyFile from to fail' = do
  bracket
    (openFdHandle from SPI.ReadOnly [FD.oNofollow] Nothing)
    (hClose . snd)
    $ \(fromFd, fH) -> do
        sourceFileMode <- fileMode <$> getFdStatus fromFd
        let dflags = [ FD.oNofollow
                     , if fail' then FD.oExcl else FD.oTrunc
                     ]
        let openFdHandle' = openFdHandle to SPI.WriteOnly dflags $ Just sourceFileMode
        bracket
          (handleIO (\e -> if
                              -- if we copy from regular file to symlink, we need
                              -- to delete the symlink
                              | ioe_type e == InvalidArgument
                              , not fail' -> do
                                 removeLink to
                                 openFdHandle'
                              | otherwise -> throwIO e
                    )
            openFdHandle')
          (hClose . snd)
          $ \(_, tH) -> do
              hSetBinaryMode fH True
              hSetBinaryMode tH True
              runConduitRes $ sourceHandle fH .| sinkHandle tH
 where
  openFdHandle fp omode flags fM = do
    fd      <- openFd' fp omode flags fM
    handle' <- SPI.fdToHandle fd
    pure (fd, handle')

foreign import capi unsafe "fcntl.h open"
   c_open :: CString -> CInt -> Posix.CMode -> IO CInt


open_  :: CString
       -> Posix.OpenMode
       -> [FD.Flags]
       -> Maybe Posix.FileMode
       -> IO Posix.Fd
open_ str how optional_flags maybe_mode = do
    fd <- c_open str all_flags mode_w
    return (Posix.Fd fd)
  where
    all_flags  = FD.unionFlags $ optional_flags ++ [open_mode] ++ creat


    (creat, mode_w) = case maybe_mode of
                        Nothing -> ([],0)
                        Just x  -> ([FD.oCreat], x)

    open_mode = case how of
                   Posix.ReadOnly  -> FD.oRdonly
                   Posix.WriteOnly -> FD.oWronly
                   Posix.ReadWrite -> FD.oRdwr


-- |Open and optionally create this file. See 'System.Posix.Files'
-- for information on how to use the 'FileMode' type.
--
-- Note that passing @Just x@ as the 4th argument triggers the
-- `oCreat` status flag, which must be set when you pass in `oExcl`
-- to the status flags. Also see the manpage for @open(2)@.
openFd' :: FilePath
        -> Posix.OpenMode
        -> [FD.Flags]               -- ^ status flags of @open(2)@
        -> Maybe Posix.FileMode  -- ^ @Just x@ => creates the file with the given modes, Nothing => the file must exist.
        -> IO Posix.Fd
openFd' name how optional_flags maybe_mode =
   withFilePath name $ \str ->
     throwErrnoPathIfMinus1Retry "openFd" name $
       open_ str how optional_flags maybe_mode


-- |Deletes the given file. Raises `eISDIR`
-- if run on a directory. Does not follow symbolic links.
--
-- Throws:
--
--    - `InappropriateType` for wrong file type (directory)
--    - `NoSuchThing` if the file does not exist
--    - `PermissionDenied` if the directory cannot be read
--
-- Notes: calls `unlink`
deleteFile :: FilePath -> IO ()
deleteFile = removeLink


-- |Recreate a symlink.
--
-- In `Overwrite` copy mode only files and empty directories are deleted.
--
-- Safety/reliability concerns:
--
--    * `Overwrite` mode is inherently non-atomic
--
-- Throws:
--
--    - `InvalidArgument` if source file is wrong type (not a symlink)
--    - `PermissionDenied` if output directory cannot be written to
--    - `PermissionDenied` if source directory cannot be opened
--    - `SameFile` if source and destination are the same file
--      (`HPathIOException`)
--
--
-- Throws in `Strict` mode only:
--
--    - `AlreadyExists` if destination already exists
--
-- Throws in `Overwrite` mode only:
--
--    - `UnsatisfiedConstraints` if destination file is non-empty directory
--
-- Notes:
--
--    - calls `symlink`
recreateSymlink :: FilePath   -- ^ the old symlink file
                -> FilePath   -- ^ destination file
                -> Bool       -- ^ fail if destination file exists
                -> IO ()
recreateSymlink symsource newsym fail' = do
  sympoint <- readSymbolicLink symsource
  case fail' of
    True  -> pure ()
    False ->
      handleIO (\e -> if doesNotExistErrorType  == ioeGetErrorType e then pure () else liftIO . ioError $ e) $ deleteFile newsym
  createSymbolicLink sympoint newsym


-- copies files, recreates symlinks, fails on all other types
install :: FilePath -> FilePath -> Bool -> IO ()
install from to fail' = do
  fs <- PF.getSymbolicLinkStatus from
  decide fs
 where
  decide fs | PF.isRegularFile fs     = copyFile from to fail'
            | PF.isSymbolicLink fs    = recreateSymlink from to fail'
            | otherwise               = ioError $ mkIOError illegalOperationErrorType "install: not a regular file or symlink" Nothing (Just from)

moveFile :: FilePath -> FilePath -> IO ()
moveFile = rename


moveFilePortable :: FilePath -> FilePath -> IO ()
moveFilePortable from to = do
  catchErrno [eXDEV] (moveFile from to) $ do
    copyFile from to True
    removeFile from


catchErrno :: [Errno] -- ^ errno to catch
           -> IO a    -- ^ action to try, which can raise an IOException
           -> IO a    -- ^ action to carry out in case of an IOException and
                      --   if errno matches
           -> IO a
catchErrno en a1 a2 =
  catchIOError a1 $ \e -> do
    errno <- getErrno
    if errno `elem` en
      then a2
      else ioError e

removeEmptyDirectory :: FilePath -> IO ()
removeEmptyDirectory = PD.removeDirectory

sourceDirectory' :: MonadResource m => FilePath -> ConduitT i (FD.DirType, FilePath) m ()
sourceDirectory' dir =
    bracketP (openDirStreamPortable dir) closeDirStreamPortable go
  where
    go ds =
        loop
      where
        loop = do
            (typ, e) <- liftIO $ readDirEntPortable ds
            if
              | null e    -> return ()
              | "." == e  -> loop
              | ".." == e -> loop
              | otherwise -> do
                  yield (typ, e)
                  loop

sourceDirectoryDeep' :: MonadResource m
                     => FilePath -- ^ Root directory
                     -> ConduitT i FilePath m ()
sourceDirectoryDeep' fp' = start "" .| C.map snd
  where
    start :: MonadResource m => FilePath -> ConduitT i (FD.DirType, FilePath) m ()
    start dir = sourceDirectory' (fp' </> dir) .| awaitForever go
     where
      go :: MonadResource m => (FD.DirType, FilePath) -> ConduitT (FD.DirType, FilePath) (FD.DirType, FilePath) m ()
      go (typ, fp)
        | FD.dtDir == typ = start (dir </> fp)
        | otherwise       = yield (typ, dir </> fp)

