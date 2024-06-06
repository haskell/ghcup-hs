{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module GHCup.Prelude.File (
  mergeFileTree,
  copyFileE,
  findFilesDeep,
  getDirectoryContentsRecursive,
  getDirectoryContentsRecursiveUnsafe,
  recordedInstallationFile,
  module GHCup.Prelude.File.Search,

  chmod_755,
  isBrokenSymlink,
  copyFile,
  deleteFile,
  install,
  removeEmptyDirectory,
  removeDirIfEmptyOrIsSymlink,
  removeEmptyDirsRecursive,
  rmFileForce,
  createDirRecursive',
  recyclePathForcibly,
  rmDirectory,
  recycleFile,
  rmFile,
  rmDirectoryLink,
  moveFilePortable,
  moveFile,
  rmPathForcibly,

  exeExt,
  exeExt',
  getLinkTarget,
  pathIsLink,
  rmLink,
  createLink
) where

import GHCup.Utils.Dirs
import GHCup.Prelude.Logger.Internal (logInfo, logDebug)
import GHCup.Prelude.Internal
import GHCup.Prelude.File.Search
#if IS_WINDOWS
import GHCup.Prelude.File.Windows
import GHCup.Prelude.Windows
#else
import GHCup.Prelude.File.Posix
import GHCup.Prelude.Posix
#endif
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics

import           Text.Regex.Posix
import           Conduit
import qualified Data.Conduit.Combinators as C
import           Control.Exception.Safe
import           Control.Monad.Reader
import           Data.ByteString                ( ByteString )
import           Haskus.Utils.Variant.Excepts
import           System.FilePath
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import qualified Data.Text                     as T
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import GHC.IO.Exception
import System.IO.Error
import Control.Monad (when, forM_, filterM)


-- | Merge one file tree to another given a copy operation.
--
-- Records every successfully installed file into the destination
-- returned by 'recordedInstallationFile'.
--
-- If any copy operation fails, the record file is deleted, as well
-- as the partially installed files.
mergeFileTree :: ( MonadMask m
                 , MonadReader env m
                 , HasDirs env
                 , HasLog env
                 , MonadCatch m
                 , MonadIO m
                 , MonadUnliftIO m
                 )
              => GHCupPath                       -- ^ source base directory from which to install findFiles
              -> InstallDirResolved              -- ^ destination base dir
              -> Tool
              -> GHCTargetVersion
              -> (FilePath -> FilePath -> m ())  -- ^ file copy operation
              -> Excepts '[MergeFileTreeError] m ()
mergeFileTree _ (GHCupBinDir fp) _ _ _ =
  throwIO $ userError ("mergeFileTree: internal error, called on " <> fp)
mergeFileTree sourceBase destBase tool v' copyOp = do
  lift $ logInfo $ "Merging file tree from \""
       <> T.pack (fromGHCupPath sourceBase)
       <> "\" to \""
       <> T.pack (fromInstallDir destBase)
       <> "\""
  recFile <- recordedInstallationFile tool v'

  wrapInExcepts $ do
    -- These checks are not atomic, but we perform them to have
    -- the opportunity to abort before copying has started.
    --
    -- The actual copying might still fail.
    liftIO $ baseCheck (fromGHCupPath sourceBase)
    liftIO $ destCheck (fromInstallDir destBase)

    -- we only record for non-isolated installs
    when (isSafeDir destBase) $ do
      whenM (liftIO $ doesFileExist recFile)
        $ throwIO $ userError ("mergeFileTree: DB file " <> recFile <> " already exists!")
      liftIO $ createDirectoryIfMissing True (takeDirectory recFile)

  -- we want the cleanup action to leak through in case of exception
  onE_ (cleanupOnPartialInstall recFile) $ wrapInExcepts $ do
    logDebug "Starting merge"
    lift $ runConduitRes $ getDirectoryContentsRecursive sourceBase .| C.mapM_ (\f -> do
      lift $ copy f
      logDebug $ T.pack "Recording installed file: " <> T.pack f
      recordInstalledFile f recFile)

 where
  wrapInExcepts = handleIO (\e -> throwE $ MergeFileTreeError e (fromGHCupPath sourceBase) (fromInstallDir destBase))

  cleanupOnPartialInstall recFile = when (isSafeDir destBase) $ do
    (force -> !l) <- hideErrorDef [NoSuchThing] [] $ lines <$> liftIO
      (readFile recFile >>= evaluate)
    logDebug "Deleting recorded files due to partial install"
    forM_ l $ \f -> do
      let dest = fromInstallDir destBase </> dropDrive f
      logDebug $ "rm -f " <> T.pack f
      hideError NoSuchThing $ rmFile dest
      pure ()
    logDebug $ "rm -f " <> T.pack recFile
    hideError NoSuchThing $ rmFile recFile
    logDebug $ "rm -f " <> T.pack (fromInstallDir destBase)
    hideError UnsatisfiedConstraints $ hideError NoSuchThing $
      removeEmptyDirsRecursive (fromInstallDir destBase)


  recordInstalledFile f recFile = when (isSafeDir destBase) $
    liftIO $ appendFile recFile (f <> "\n")

  copy source = do
    let dest = fromInstallDir destBase </> source
        src  = fromGHCupPath sourceBase </> source

    when (isAbsolute source)
      $ throwIO $ userError ("mergeFileTree: source file " <> source <> " is not relative!")

    liftIO . createDirectoryIfMissing True . takeDirectory $ dest

    copyOp src dest


  baseCheck src = do
      when (isRelative src)
        $ throwIO $ userError ("mergeFileTree: source base directory " <> src <> " is not absolute!")
      whenM (not <$> doesDirectoryExist src)
        $ throwIO $ userError ("mergeFileTree: source base directory " <> src <> " does not exist!")
  destCheck dest = do
      when (isRelative dest)
        $ throwIO $ userError ("mergeFileTree: destination base directory " <> dest <> " is not absolute!")



copyFileE :: (CopyError :< xs, MonadCatch m, MonadIO m) => FilePath -> FilePath -> Bool -> Excepts xs m ()
copyFileE from to = handleIO (throwE . CopyError . show) . liftIO . copyFile from to


getDirectoryContentsRecursive :: (MonadResource m)
                              => GHCupPath
                              -> ConduitT i FilePath m ()
getDirectoryContentsRecursive (fromGHCupPath -> fp) = getDirectoryContentsRecursiveUnsafe fp

getDirectoryContentsRecursiveUnsafe :: (MonadResource m)
                                    => FilePath
                                    -> ConduitT i FilePath m ()
getDirectoryContentsRecursiveUnsafe = sourceDirectoryDeep'


findFilesDeep :: GHCupPath -> Regex -> IO [FilePath]
findFilesDeep path regex =
  runResourceT $ sourceToList $ getDirectoryContentsRecursive path .| C.filter (match regex)


recordedInstallationFile :: ( MonadReader env m
                            , HasDirs env
                            )
                         => Tool
                         -> GHCTargetVersion
                         -> m FilePath
recordedInstallationFile t v' = do
  Dirs {..}  <- getDirs
  pure (fromGHCupPath dbDir </> prettyShow t </> T.unpack (tVerToText v'))

removeDirIfEmptyOrIsSymlink :: (MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
removeDirIfEmptyOrIsSymlink filepath =
  hideError UnsatisfiedConstraints $
  handleIO' InappropriateType
        (handleIfSym filepath)
        (liftIO $ removeEmptyDirectory filepath)
  where
    handleIfSym fp e = do
      isSym <- liftIO $ pathIsSymbolicLink fp
      if isSym
      then rmFileForce fp
      else liftIO $ ioError e

removeEmptyDirsRecursive :: (MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
removeEmptyDirsRecursive = go
 where
  go fp = do
    cs <- liftIO $ listDirectory fp >>= filterM doesDirectoryExist . fmap (fp </>)
    forM_ cs go
    liftIO $ removeEmptyDirectory fp

rmFileForce :: (MonadMask m, MonadIO m) => FilePath -> m ()
rmFileForce filepath = do
  hideError doesNotExistErrorType
    $ hideError InappropriateType $ rmFile filepath

-- | More permissive version of 'createDirRecursive'. This doesn't
-- error when the destination is a symlink to a directory.
createDirRecursive' :: FilePath -> IO ()
createDirRecursive' p =
  handleIO (\e -> if isAlreadyExistsError e then isSymlinkDir e else throwIO e)
    . createDirectoryIfMissing True
    $ p

 where
  isSymlinkDir e = do
    ft <- pathIsSymbolicLink p
    case ft of
      True -> do
        rp <- canonicalizePath p
        rft <- doesDirectoryExist rp
        case rft of
          True -> pure ()
          _ -> throwIO e
      _ -> throwIO e


-- https://github.com/haskell/directory/issues/110
-- https://github.com/haskell/directory/issues/96
-- https://www.sqlite.org/src/info/89f1848d7f
recyclePathForcibly :: ( MonadIO m
                       , MonadReader env m
                       , HasDirs env
                       , MonadMask m
                       )
                    => GHCupPath
                    -> m ()
recyclePathForcibly fp
  | isWindows = do
      Dirs { recycleDir } <- getDirs
      tmp <- liftIO $ createTempGHCupDirectory recycleDir "recyclePathForcibly"
      let dest = tmp `appendGHCupPath` takeFileName (fromGHCupPath fp)
      liftIO (moveFile (fromGHCupPath fp) (fromGHCupPath dest))
          `catch`
          (\e -> if | isDoesNotExistError e -> pure ()
                    | isPermissionError e || ioeGetErrorType e == UnsupportedOperation {- EXDEV on windows -} -> recover (liftIO $ removePathForcibly fp)
                    | otherwise -> throwIO e)
          `finally`
            liftIO (handleIO (\_ -> pure ()) $ removePathForcibly tmp)
  | otherwise = liftIO $ removePathForcibly fp



rmDirectory :: (MonadIO m, MonadMask m)
            => GHCupPath
            -> m ()
rmDirectory fp
  | isWindows = recover (liftIO $ removeDirectory fp)
  | otherwise = liftIO $ removeDirectory fp


-- https://www.sqlite.org/src/info/89f1848d7f
-- https://github.com/haskell/directory/issues/96
recycleFile :: ( MonadIO m
               , MonadMask m
               , MonadReader env m
               , HasDirs env
               )
            => FilePath
            -> m ()
recycleFile fp
  | isWindows = do
      Dirs { recycleDir } <- getDirs
      liftIO $ whenM (doesDirectoryExist fp) $ ioError (IOError Nothing InappropriateType "recycleFile" "" Nothing (Just fp))
      tmp <- liftIO $ createTempGHCupDirectory recycleDir "recycleFile"
      let dest = fromGHCupPath tmp </> takeFileName fp
      liftIO (moveFile fp dest)
        `catch`
          (\e -> if isPermissionError e || ioeGetErrorType e == UnsupportedOperation {- EXDEV on windows -} then recover (liftIO $ rmFile fp) else throwIO e)
        `finally`
          liftIO (handleIO (\_ -> pure ()) $ removePathForcibly tmp)
  | otherwise = liftIO $ removeFile fp


rmFile :: ( MonadIO m
          , MonadMask m
          )
      => FilePath
      -> m ()
rmFile fp
  | isWindows = recover (liftIO $ removeFile fp)
  | otherwise = liftIO $ removeFile fp


rmDirectoryLink :: (MonadIO m, MonadMask m, MonadReader env m, HasDirs env)
                => FilePath
                -> m ()
rmDirectoryLink fp
  | isWindows = recover (liftIO $ removeDirectoryLink fp)
  | otherwise = liftIO $ removeDirectoryLink fp


rmPathForcibly :: ( MonadIO m
                  , MonadMask m
                  )
               => GHCupPath
               -> m ()
rmPathForcibly fp
  | isWindows = recover (liftIO $ removePathForcibly fp)
  | otherwise = liftIO $ removePathForcibly fp


-- | The file extension for executables.
exeExt :: String
exeExt
  | isWindows = ".exe"
  | otherwise = ""

-- | The file extension for executables.
exeExt' :: ByteString
exeExt'
  | isWindows = ".exe"
  | otherwise = ""


rmLink :: (MonadReader env m, HasDirs env, MonadIO m, MonadMask m) => FilePath -> m ()
rmLink fp
  | isWindows = do
      hideError doesNotExistErrorType . recycleFile $ fp
      hideError doesNotExistErrorType . recycleFile $ (dropExtension fp <.> "shim")
  | otherwise = hideError doesNotExistErrorType . recycleFile $ fp


-- | Creates a symbolic link on unix and a fake symlink on windows for
-- executables, which:
--     1. is a shim exe
--     2. has a corresponding .shim file in the same directory that
--        contains the target
--
-- This overwrites previously existing files.
--
-- On windows, this requires that 'ensureShimGen' was run beforehand.
createLink :: ( MonadMask m
              , MonadThrow m
              , HasLog env
              , MonadIO m
              , MonadReader env m
              , HasDirs env
              , MonadUnliftIO m
              , MonadFail m
              )
           => FilePath      -- ^ path to the target executable
           -> FilePath      -- ^ path to be created
           -> m ()
createLink link exe
  | isWindows = do
      dirs <- getDirs
      let shimGen = fromGHCupPath (cacheDir dirs) </> "gs.exe"

      let shim = dropExtension exe <.> "shim"
          -- For hardlinks, link needs to be absolute.
          -- If link is relative, it's relative to the target exe.
          -- Note that (</>) drops lhs when rhs is absolute.
          fullLink = takeDirectory exe </> link
          shimContents = "path = " <> fullLink

      logDebug $ "rm -f " <> T.pack exe
      rmLink exe

      logDebug $ "ln -s " <> T.pack fullLink <> " " <> T.pack exe
      liftIO $ copyFile shimGen exe False
      liftIO $ writeFile shim shimContents
  | otherwise = do
      logDebug $ "rm -f " <> T.pack exe
      hideError doesNotExistErrorType $ recycleFile exe

      logDebug $ "ln -s " <> T.pack link <> " " <> T.pack exe
      liftIO $ createFileLink link exe
