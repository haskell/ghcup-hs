{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module GHCup.Utils.File (
  mergeFileTree,
  mergeFileTreeAll,
  copyFileE,
  module GHCup.Utils.File.Common,
#if IS_WINDOWS
  module GHCup.Utils.File.Windows
#else
  module GHCup.Utils.File.Posix
#endif
) where

import GHCup.Utils.File.Common
#if IS_WINDOWS
import GHCup.Utils.File.Windows
#else
import GHCup.Utils.File.Posix
#endif
import           GHCup.Errors
import           GHCup.Utils.Prelude

import           GHC.IO                         ( evaluate )
import           Control.Exception.Safe
import           Haskus.Utils.Variant.Excepts
import           Control.Monad.Reader
import           System.Directory        hiding (findFiles, copyFile)
import           System.FilePath

import Data.List (nub)
import Data.Foldable (traverse_)
import Control.DeepSeq (force)


-- | Like 'mergeFileTree', except reads the entire source base dir to determine files to copy recursively.
mergeFileTreeAll :: MonadIO m
                 => FilePath                        -- ^ source base directory from which to install findFiles
                 -> FilePath                        -- ^ destination base dir
                 -> (FilePath -> FilePath -> m ()) -- ^ file copy operation
                 -> m [FilePath]
mergeFileTreeAll sourceBase destBase copyOp = do
  (force -> !sourceFiles) <- liftIO
    (getDirectoryContentsRecursive sourceBase >>= evaluate)
  mergeFileTree sourceBase sourceFiles destBase copyOp
  pure sourceFiles


mergeFileTree :: MonadIO m
              => FilePath                        -- ^ source base directory from which to install findFiles
              -> [FilePath]                      -- ^ relative filepaths from source base directory
              -> FilePath                        -- ^ destination base dir
              -> (FilePath -> FilePath -> m ()) -- ^ file copy operation
              -> m ()
mergeFileTree sourceBase sources destBase copyOp = do
  -- These checks are not atomic, but we perform them to have
  -- the opportunity to abort before copying has started.
  --
  -- The actual copying might still fail.
  liftIO baseCheck
  liftIO destCheck
  liftIO sourcesCheck

  -- finally copy
  copy

 where
  copy = do
    let dirs = map (destBase </>) . nub . fmap takeDirectory $ sources
    traverse_ (liftIO . createDirectoryIfMissing True) dirs

    forM_ sources $ \source -> do
      let dest = destBase </> source
          src  = sourceBase </> source
      copyOp src dest

  baseCheck = do
      when (isRelative sourceBase)
        $ throwIO $ userError ("mergeFileTree: source base directory " <> sourceBase <> " is not absolute!")
      whenM (not <$> doesDirectoryExist sourceBase)
        $ throwIO $ userError ("mergeFileTree: source base directory " <> sourceBase <> " does not exist!")
  destCheck = do
      when (isRelative destBase)
        $ throwIO $ userError ("mergeFileTree: destination base directory " <> destBase <> " is not absolute!")
      whenM (doesDirectoryExist destBase)
        $ throwIO $ userError ("mergeFileTree: destination base directory " <> destBase <> " does already exist!")
  sourcesCheck =
    forM_ sources $ \source -> do
      -- TODO: use Excepts or HPath
      when (isAbsolute source)
        $ throwIO $ userError ("mergeFileTree: source file " <> source <> " is not relative!")
      whenM (not <$> doesFileExist (sourceBase </> source))
        $ throwIO $ userError ("mergeFileTree: source file " <> (sourceBase </> source) <> " does not exist!")

copyFileE :: (CopyError :< xs, MonadCatch m, MonadIO m) => FilePath -> FilePath -> Bool -> Excepts xs m ()
copyFileE from to = handleIO (throwE . CopyError . show) . liftIO . copyFile from to
