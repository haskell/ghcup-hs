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

import           GHCup.Prelude.File.Posix.Traversals

import           Control.Exception.Safe
import           Control.Monad.Reader
import           Foreign.C.String
import           Foreign.C.Error
import           Foreign.C.Types
import           System.IO                      ( hClose, hSetBinaryMode )
import           System.IO.Error      hiding    ( catchIOError )
import           System.FilePath
import           System.Directory               ( removeFile, pathIsSymbolicLink, getSymbolicLinkTarget, doesPathExist )
import           System.Posix.Directory
import           System.Posix.Error             ( throwErrnoPathIfMinus1Retry )
import           System.Posix.Internals         ( withFilePath )
import           System.Posix.Files
import           System.Posix.Types


import qualified System.Posix.Directory        as PD
import qualified System.Posix.Files            as PF
import qualified System.Posix.IO               as SPI
import qualified System.Posix as Posix
import qualified Streamly.FileSystem.Handle    as FH
import qualified Streamly.Internal.FileSystem.Handle
                                               as IFH
import qualified Streamly.Prelude              as S
import qualified GHCup.Prelude.File.Posix.Foreign as FD
import qualified Streamly.Internal.Data.Stream.StreamD.Type
                                               as D
import           Streamly.Internal.Data.Unfold.Type
import qualified Streamly.Internal.Data.Unfold as U
import           Streamly.Internal.Control.Concurrent ( withRunInIO )
import           Streamly.Internal.Data.IOFinalizer   ( newIOFinalizer, runIOFinalizer )
import GHC.IO.Exception (IOException(ioe_type), IOErrorType (..))


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
              streamlyCopy (fH, tH)
 where
  openFdHandle fp omode flags fM = do
    fd      <- openFd' fp omode flags fM
    handle' <- SPI.fdToHandle fd
    pure (fd, handle')
  streamlyCopy (fH, tH) =
    S.fold (FH.writeChunks tH) $ IFH.toChunksWithBufferOf (256 * 1024) fH

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


-- copys files, recreates symlinks, fails on all other types
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


-- | Create an 'Unfold' of directory contents.
unfoldDirContents :: (MonadMask m, MonadIO m, S.MonadAsync m) => Unfold m FilePath (FD.DirType, FilePath)
unfoldDirContents = U.bracket (liftIO . openDirStream) (liftIO . closeDirStream) (Unfold step return)
 where
  {-# INLINE [0] step #-}
  step dirstream = do
    (typ, e) <- liftIO $ readDirEnt dirstream
    return $ if
      | null e    -> D.Stop
      | "." == e  -> D.Skip dirstream
      | ".." == e -> D.Skip dirstream
      | otherwise -> D.Yield (typ, e) dirstream


getDirectoryContentsRecursiveDFSUnsafe :: (MonadMask m, MonadIO m, S.MonadAsync m)
                                       => FilePath
                                       -> S.SerialT m FilePath
getDirectoryContentsRecursiveDFSUnsafe fp = go ""
 where
  go cd = flip S.concatMap (S.unfold unfoldDirContents (fp </> cd)) $ \(t, f) ->
    if | t == FD.dtDir -> go (cd </> f)
       | otherwise     -> pure (cd </> f)


getDirectoryContentsRecursiveUnfold :: (MonadMask m, MonadIO m, S.MonadAsync m) => Unfold m FilePath FilePath
getDirectoryContentsRecursiveUnfold = Unfold step (\s -> return (s, Nothing, [""]))
 where
  {-# INLINE [0] step #-}
  step (_, Nothing, []) = return D.Stop

  step (topdir, Just (cdir, dirstream, finalizer), dirs) = flip onException (runIOFinalizer finalizer) $ do
    (dt, f) <- liftIO $ readDirEnt dirstream
    if | FD.dtUnknown == dt -> do
           runIOFinalizer finalizer
           return $ D.Skip (topdir, Nothing, dirs)
       | f == "." || f == ".."
                        -> return $ D.Skip               (topdir, Just (cdir, dirstream, finalizer), dirs)
       | FD.dtDir == dt -> return $ D.Skip               (topdir, Just (cdir, dirstream, finalizer), (cdir </> f):dirs)
       | otherwise      -> return $ D.Yield (cdir </> f) (topdir, Just (cdir, dirstream, finalizer), dirs)

  step (topdir, Nothing, dir:dirs) = do
    (s, f) <- acquire (topdir </> dir)
    return $ D.Skip (topdir, Just (dir, s, f), dirs)

  acquire dir =
    withRunInIO $ \run -> mask_ $ run $ do
        dirstream <- liftIO $ openDirStream dir
        ref <- newIOFinalizer (liftIO $ closeDirStream dirstream)
        return (dirstream, ref)

getDirectoryContentsRecursiveBFSUnsafe :: (MonadMask m, MonadIO m, S.MonadAsync m)
                                       => FilePath
                                       -> S.SerialT m FilePath
getDirectoryContentsRecursiveBFSUnsafe = S.unfold getDirectoryContentsRecursiveUnfold


