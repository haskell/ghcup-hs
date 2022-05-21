{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds  #-}

{-|
Module      : GHCup.Utils.File.Windows
Description : File and directory handling for windows
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : Windows
-}
module GHCup.Prelude.File.Windows where

import           GHCup.Utils.Dirs
import           GHCup.Prelude.Internal

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Data.List
import qualified GHC.Unicode                  as U
import           System.FilePath
import qualified System.IO.Error              as IOE

import qualified System.Win32.Info             as WS
import qualified System.Win32.File             as WS

import qualified Streamly.Internal.Data.Stream.StreamD.Type
                                               as D
import           Streamly.Internal.Data.Unfold.Type hiding ( concatMap )
import           Data.Bits ((.&.))
import qualified Streamly.Prelude              as S
import qualified Streamly.Internal.Data.Unfold as U
import           Streamly.Internal.Control.Concurrent ( withRunInIO )
import           Streamly.Internal.Data.IOFinalizer   ( newIOFinalizer, runIOFinalizer )



-- | On unix, we can use symlinks, so we just get the
-- symbolic link target.
--
-- On windows, we have to emulate symlinks via shims,
-- see 'createLink'.
getLinkTarget :: FilePath -> IO FilePath
getLinkTarget fp = do
  content <- readFile (dropExtension fp <.> "shim")
  [p] <- pure . filter ("path = " `isPrefixOf`) . lines $ content
  pure $ stripNewline $ dropPrefix "path = " p


-- | Checks whether the path is a link.
pathIsLink :: FilePath -> IO Bool
pathIsLink fp = doesPathExist (dropExtension fp <.> "shim")



chmod_755 :: MonadIO m => FilePath -> m ()
chmod_755 fp =
  let perm = setOwnerWritable True emptyPermissions
  in liftIO $ setPermissions fp perm


-- | Checks whether the binary is a broken link.
isBrokenSymlink :: FilePath -> IO Bool
isBrokenSymlink fp = do
  b <- pathIsLink fp
  if b
  then do
    tfp <- getLinkTarget fp
    not <$> doesPathExist
      -- this drops 'symDir' if 'tfp' is absolute
      (takeDirectory fp </> tfp)
  else pure False


copyFile :: FilePath   -- ^ source file
         -> FilePath   -- ^ destination file
         -> Bool       -- ^ fail if file exists
         -> IO ()
copyFile = WS.copyFile

deleteFile :: FilePath -> IO ()
deleteFile = WS.deleteFile


install :: FilePath -> FilePath -> Bool -> IO ()
install = copyFile


moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = WS.moveFileEx from (Just to) 0


moveFilePortable :: FilePath -> FilePath -> IO ()
moveFilePortable = WS.moveFile


removeEmptyDirectory :: FilePath -> IO ()
removeEmptyDirectory = WS.removeDirectory


unfoldDirContents :: (S.MonadAsync m, MonadIO m, MonadCatch m, MonadMask m) => Unfold m FilePath (WS.FileAttributeOrFlag, FilePath)
unfoldDirContents = U.bracket alloc dealloc (Unfold step return)
 where
  {-# INLINE [0] step #-}
  step (_, False, _, _) = return D.Stop
  step (topdir, True, h, fd) = flip onException (liftIO $ WS.findClose h) $ do
    f <- liftIO $ WS.getFindDataFileName fd
    more <- liftIO $ WS.findNextFile h fd

    -- can't get file attribute from FindData yet (needs Win32 PR)
    fattr <- liftIO $ WS.getFileAttributes (topdir </> f)

    if | f == "." || f == ".." -> return $ D.Skip             (topdir, more, h, fd)
       | otherwise             -> return $ D.Yield (fattr, f) (topdir, more, h, fd)

  alloc topdir = do
    query <- liftIO $ furnishPath (topdir </> "*")
    (h, fd) <- liftIO $ WS.findFirstFile query
    pure (topdir, True, h, fd)

  dealloc (_, _, fd, _) = liftIO $ WS.findClose fd


getDirectoryContentsRecursiveDFSUnsafe :: (MonadCatch m, S.MonadAsync m, MonadMask m, S.IsStream t)
                                       => FilePath
                                       -> t m FilePath
getDirectoryContentsRecursiveDFSUnsafe fp = go ""
 where
  isDir attrs = attrs .&. WS.fILE_ATTRIBUTE_DIRECTORY /= 0

  go cd = flip S.concatMap (S.unfold unfoldDirContents (fp </> cd)) $ \(t, f) ->
    if | isDir t   -> go (cd </> f)
       | otherwise -> pure (cd </> f)


getDirectoryContentsRecursiveUnfold :: (MonadCatch m, S.MonadAsync m, MonadMask m) => Unfold m FilePath FilePath
getDirectoryContentsRecursiveUnfold = Unfold step init'
 where
  {-# INLINE [0] step #-}
  step (_, Nothing, []) = return D.Stop

  step (topdir, state@(Just (cdir, (h, findData, ref))), dirs) = flip onException (runIOFinalizer ref) $ do
    f <- liftIO $ WS.getFindDataFileName findData

    more <- liftIO $ WS.findNextFile h findData
    when (not more) $ runIOFinalizer ref
    let nextState = if more then state else Nothing

    -- can't get file attribute from FindData yet (needs Win32 PR)
    fattr <- liftIO $ WS.getFileAttributes (topdir </> cdir </> f)

    if | f == "." || f == ".." -> return $ D.Skip               (topdir, nextState, dirs)
       | isDir fattr           -> return $ D.Skip               (topdir, nextState, (cdir </> f):dirs)
       | otherwise             -> return $ D.Yield (cdir </> f) (topdir, nextState, dirs)

  step (topdir, Nothing, dir:dirs) = do
    (h, findData, ref) <- acquire (topdir </> dir)
    return $ D.Skip (topdir, Just (dir, (h, findData, ref)), dirs)

  init' topdir = do
    (h, findData, ref) <- acquire topdir
    return (topdir, Just ("", (h, findData, ref)), [])

  isDir attrs = attrs .&. WS.fILE_ATTRIBUTE_DIRECTORY /= 0

  acquire dir = do
    query <- liftIO $ furnishPath (dir </> "*")
    withRunInIO $ \run -> mask_ $ run $ do
        (h, findData) <- liftIO $ WS.findFirstFile query
        ref <- newIOFinalizer (liftIO $ WS.findClose h)
        return (h, findData, ref)


getDirectoryContentsRecursiveBFSUnsafe :: (MonadMask m, MonadIO m, S.MonadAsync m)
                                       => FilePath
                                       -> S.SerialT m FilePath
getDirectoryContentsRecursiveBFSUnsafe = S.unfold getDirectoryContentsRecursiveUnfold



    --------------------------------------
    --[ Inlined from directory package ]--
    --------------------------------------


furnishPath :: FilePath -> IO FilePath
furnishPath path =
  (toExtendedLengthPath <$> rawPrependCurrentDirectory path)
    `IOE.catchIOError` \ _ ->
      pure path


toExtendedLengthPath :: FilePath -> FilePath
toExtendedLengthPath path
  | isRelative path = simplifiedPath
  | otherwise =
      case simplifiedPath of
        '\\' : '?'  : '?' : '\\' : _ -> simplifiedPath
        '\\' : '\\' : '?' : '\\' : _ -> simplifiedPath
        '\\' : '\\' : '.' : '\\' : _ -> simplifiedPath
        '\\' : subpath@('\\' : _) -> "\\\\?\\UNC" <> subpath
        _ -> "\\\\?\\" <> simplifiedPath
  where simplifiedPath = simplify path


simplify :: FilePath -> FilePath
simplify = simplifyWindows

simplifyWindows :: FilePath -> FilePath
simplifyWindows "" = ""
simplifyWindows path =
  case drive' of
    "\\\\?\\" -> drive' <> subpath
    _ -> simplifiedPath
  where
    simplifiedPath = joinDrive drive' subpath'
    (drive, subpath) = splitDrive path
    drive' = upperDrive (normaliseTrailingSep (normalisePathSeps drive))
    subpath' = appendSep . avoidEmpty . prependSep . joinPath .
               stripPardirs . expandDots . skipSeps .
               splitDirectories $ subpath

    upperDrive d = case d of
      c : ':' : s | U.isAlpha c && all isPathSeparator s -> U.toUpper c : ':' : s
      _ -> d
    skipSeps = filter (not . (`elem` (pure <$> pathSeparators)))
    stripPardirs | pathIsAbsolute || subpathIsAbsolute = dropWhile (== "..")
                 | otherwise = id
    prependSep | subpathIsAbsolute = (pathSeparator :)
               | otherwise = id
    avoidEmpty | not pathIsAbsolute
                 && (null drive || hasTrailingPathSep) -- prefer "C:" over "C:."
                 = emptyToCurDir
               | otherwise = id
    appendSep p | hasTrailingPathSep
                  && not (pathIsAbsolute && null p)
                  = addTrailingPathSeparator p
                | otherwise = p
    pathIsAbsolute = not (isRelative path)
    subpathIsAbsolute = any isPathSeparator (take 1 subpath)
    hasTrailingPathSep = hasTrailingPathSeparator subpath

emptyToCurDir :: FilePath -> FilePath
emptyToCurDir ""   = "."
emptyToCurDir path = path

normaliseTrailingSep :: FilePath -> FilePath
normaliseTrailingSep path = do
  let path' = reverse path
  let (sep, path'') = span isPathSeparator path'
  let addSep = if null sep then id else (pathSeparator :)
  reverse (addSep path'')

normalisePathSeps :: FilePath -> FilePath
normalisePathSeps p = (\ c -> if isPathSeparator c then pathSeparator else c) <$> p

expandDots :: [FilePath] -> [FilePath]
expandDots = reverse . go []
  where
    go ys' xs' =
      case xs' of
        [] -> ys'
        x : xs ->
          case x of
            "." -> go ys' xs
            ".." ->
              case ys' of
                [] -> go (x : ys') xs
                ".." : _ -> go (x : ys') xs
                _ : ys -> go ys xs
            _ -> go (x : ys') xs

rawPrependCurrentDirectory :: FilePath -> IO FilePath
rawPrependCurrentDirectory path
  | isRelative path =
    ((`ioeAddLocation` "prependCurrentDirectory") .
     (`IOE.ioeSetFileName` path)) `IOE.modifyIOError` do
      getFullPathName path
  | otherwise = pure path

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = do
  IOE.ioeSetLocation e newLoc
  where
    newLoc = loc <> if null oldLoc then "" else ":" <> oldLoc
    oldLoc = IOE.ioeGetLocation e

getFullPathName :: FilePath -> IO FilePath
getFullPathName path =
  fromExtendedLengthPath <$> WS.getFullPathName (toExtendedLengthPath path)

fromExtendedLengthPath :: FilePath -> FilePath
fromExtendedLengthPath ePath =
  case ePath of
    '\\' : '\\' : '?' : '\\' : path ->
      case path of
        'U' : 'N' : 'C' : subpath@('\\' : _) -> "\\" <> subpath
        drive : ':' : subpath
          -- if the path is not "regular", then the prefix is necessary
          -- to ensure the path is interpreted literally
          | U.isAlpha drive && U.isAscii drive && isPathRegular subpath -> path
        _ -> ePath
    _ -> ePath
  where
    isPathRegular path =
      not ('/' `elem` path ||
           "." `elem` splitDirectories path ||
           ".." `elem` splitDirectories path)
