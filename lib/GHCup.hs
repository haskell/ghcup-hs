{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

{-|
Module      : GHCup
Description : GHCup installation functions
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable

This module contains the main functions that correspond
to the command line interface, like installation, listing versions
and so on.

These are the entry points.
-}
module GHCup (
  module GHCup,
  module GHCup.Cabal,
  module GHCup.GHC,
  module GHCup.HLS,
  module GHCup.Stack,
  module GHCup.List
) where


import           GHCup.Cabal
import           GHCup.GHC             hiding ( GHCVer(..) )
import           GHCup.HLS             hiding ( HLSVer(..) )
import           GHCup.Stack
import           GHCup.List
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Prelude
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ
import           GHCup.Version

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Versions                hiding ( patch )
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , writeFile
                                                )
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           System.IO.Temp
import           Text.Regex.Posix

import qualified Data.Text                     as T
import qualified Streamly.Prelude              as S
import Text.PrettyPrint.HughesPJClass (prettyShow)




    ---------------------
    --[ Tool fetching ]--
    ---------------------


fetchToolBindist :: ( MonadFail m
                    , MonadMask m
                    , MonadCatch m
                    , MonadReader env m
                    , HasDirs env
                    , HasSettings env
                    , HasPlatformReq env
                    , HasGHCupInfo env
                    , HasLog env
                    , MonadResource m
                    , MonadIO m
                    , MonadUnliftIO m
                    )
                 => Version
                 -> Tool
                 -> Maybe FilePath
                 -> Excepts
                      '[ DigestError
                       , ContentLengthError
                       , GPGError
                       , DownloadFailed
                       , NoDownload
                       ]
                      m
                      FilePath
fetchToolBindist v t mfp = do
  dlinfo <- liftE $ getDownloadInfo t v
  liftE $ downloadCached' dlinfo Nothing mfp



    ------------
    --[ Nuke ]--
    ------------




rmTool :: ( MonadReader env m
          , HasDirs env
          , HasLog env
          , MonadFail m
          , MonadMask m
          , MonadUnliftIO m)
          => ListResult
          -> Excepts '[NotInstalled, UninstallFailed] m ()
rmTool ListResult {lVer, lTool, lCross} = do
  logInfo $ "removing " <> T.pack (show lTool) <> " version " <> prettyVer lVer
  case lTool of
    GHC ->
      let ghcTargetVersion = GHCTargetVersion lCross lVer
      in rmGHCVer ghcTargetVersion
    HLS -> rmHLSVer lVer
    Cabal -> liftE $ rmCabalVer lVer
    Stack -> liftE $ rmStackVer lVer
    GHCup -> lift rmGhcup


rmGhcupDirs :: ( MonadReader env m
               , HasDirs env
               , MonadIO m
               , HasLog env
               , MonadCatch m
               , MonadMask m )
            => m [FilePath]
rmGhcupDirs = do
  Dirs
    { baseDir
    , binDir
    , logsDir
    , cacheDir
    , recycleDir
    , dbDir
    , tmpDir
    } <- getDirs

  let envFilePath = fromGHCupPath baseDir </> "env"

  confFilePath <- getConfigFilePath

  handleRm $ rmEnvFile  envFilePath
  handleRm $ rmConfFile confFilePath

  -- for xdg dirs, the order matters here
  handleRm $ rmPathForcibly logsDir
  handleRm $ rmPathForcibly tmpDir
  handleRm $ rmPathForcibly cacheDir

  handleRm $ rmBinDir binDir
  handleRm $ rmPathForcibly recycleDir
  handleRm $ rmPathForcibly dbDir
  when isWindows $ do
    logInfo $ "removing " <> T.pack (fromGHCupPath baseDir </> "msys64")
    handleRm $ rmPathForcibly (baseDir `appendGHCupPath` "msys64")

  handleRm $ removeEmptyDirsRecursive (fromGHCupPath baseDir)

  -- report files in baseDir that are left-over after
  -- the standard location deletions above
  hideErrorDef [doesNotExistErrorType] [] $ reportRemainingFiles (fromGHCupPath baseDir)

  where
    handleRm :: (MonadReader env m, MonadCatch m, HasLog env, MonadIO m)  => m () -> m ()
    handleRm = handleIO (\e -> logDebug $ "Part of the cleanup action failed with error: " <> T.pack (displayException e) <> "\n"
                                <> "continuing regardless...")

    rmEnvFile :: (HasLog env, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmEnvFile enFilePath = do
      logInfo "Removing Ghcup Environment File"
      hideErrorDef [permissionErrorType] () $ rmFileForce enFilePath

    rmConfFile :: (HasLog env, MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmConfFile confFilePath = do
      logInfo "removing Ghcup Config File"
      hideErrorDef [permissionErrorType] () $ rmFileForce confFilePath

    rmBinDir :: (MonadReader env m, HasDirs env, MonadMask m, MonadIO m, MonadCatch m) => FilePath -> m ()
    rmBinDir binDir
      | isWindows = removeDirIfEmptyOrIsSymlink binDir
      | otherwise = do
          isXDGStyle <- liftIO useXDG
          when (not isXDGStyle) $
            removeDirIfEmptyOrIsSymlink binDir

    reportRemainingFiles :: (MonadMask m, MonadIO m) => FilePath -> m [FilePath]
    reportRemainingFiles dir = do
      remainingFiles <- liftIO $ S.toList (getDirectoryContentsRecursiveUnsafe dir)
      let normalizedFilePaths = fmap normalise remainingFiles
      let sortedByDepthRemainingFiles = sortBy (flip compareFn) normalizedFilePaths
      let remainingFilesAbsolute = fmap (dir </>) sortedByDepthRemainingFiles

      pure remainingFilesAbsolute

      where
        calcDepth :: FilePath -> Int
        calcDepth = length . filter isPathSeparator

        compareFn :: FilePath -> FilePath -> Ordering
        compareFn fp1 fp2 = compare (calcDepth fp1) (calcDepth fp2)




    ------------------
    --[ Debug info ]--
    ------------------


getDebugInfo :: ( Alternative m
                , MonadFail m
                , MonadReader env m
                , HasDirs env
                , HasLog env
                , MonadCatch m
                , MonadIO m
                )
             => Excepts
                  '[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]
                  m
                  DebugInfo
getDebugInfo = do
  Dirs {..} <- lift getDirs
  let diBaseDir  = fromGHCupPath baseDir
  let diBinDir   = binDir
  diGHCDir       <- fromGHCupPath <$> lift ghcupGHCBaseDir
  let diCacheDir = fromGHCupPath cacheDir
  diArch         <- lE getArchitecture
  diPlatform     <- liftE getPlatform
  pure $ DebugInfo { .. }




    -------------------------
    --[ GHCup upgrade etc ]--
    -------------------------


-- | Upgrade ghcup and place it in @~\/.ghcup\/bin\/ghcup@,
-- if no path is provided.
upgradeGHCup :: ( MonadMask m
                , MonadReader env m
                , HasDirs env
                , HasPlatformReq env
                , HasGHCupInfo env
                , HasSettings env
                , MonadCatch m
                , HasLog env
                , MonadThrow m
                , MonadFail m
                , MonadResource m
                , MonadIO m
                , MonadUnliftIO m
                )
             => Maybe FilePath    -- ^ full file destination to write ghcup into
             -> Bool              -- ^ whether to force update regardless
                                  --   of currently installed version
             -> Bool              -- ^ whether to throw an error if ghcup is shadowed
             -> Excepts
                  '[ CopyError
                   , DigestError
                   , ContentLengthError
                   , GPGError
                   , GPGError
                   , DownloadFailed
                   , NoDownload
                   , NoUpdate
                   , ToolShadowed
                   ]
                  m
                  Version
upgradeGHCup mtarget force' fatal = do
  Dirs {..} <- lift getDirs
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

  lift $ logInfo "Upgrading GHCup..."
  let latestVer = fst (fromJust (getLatest dls GHCup))
  (Just ghcupPVPVer) <- pure $ pvpToVersion ghcUpVer ""
  when (not force' && (latestVer <= ghcupPVPVer)) $ throwE NoUpdate
  dli   <- liftE $ getDownloadInfo GHCup latestVer
  tmp   <- fromGHCupPath <$> lift withGHCupTmpDir
  let fn = "ghcup" <> exeExt
  p <- liftE $ download (_dlUri dli) Nothing (Just (_dlHash dli)) (_dlCSize dli) tmp (Just fn) False
  let destDir = takeDirectory destFile
      destFile = fromMaybe (binDir </> fn) mtarget
  lift $ logDebug $ "mkdir -p " <> T.pack destDir
  liftIO $ createDirRecursive' destDir
  lift $ logDebug $ "rm -f " <> T.pack destFile
  lift $ hideError NoSuchThing $ recycleFile destFile
  lift $ logDebug $ "cp " <> T.pack p <> " " <> T.pack destFile
  copyFileE p destFile False
  lift $ chmod_755 destFile

  liftIO (isInPath destFile) >>= \b -> unless b $
    lift $ logWarn $ T.pack (takeFileName destFile) <> " is not in PATH! You have to add it in order to use ghcup."
  liftIO (isShadowed destFile) >>= \case
    Nothing -> pure ()
    Just pa
      | fatal -> throwE (ToolShadowed GHCup pa destFile latestVer)
      | otherwise ->
        lift $ logWarn $ T.pack $ prettyShow (ToolShadowed GHCup pa destFile latestVer)

  pure latestVer


-- assuming the current scheme of having just 1 ghcup bin, no version info is required.
rmGhcup :: ( MonadReader env m
           , HasDirs env
           , MonadIO m
           , MonadCatch m
           , HasLog env
           , MonadMask m
           , MonadUnliftIO m
           )
        => m ()
rmGhcup = do
  Dirs { .. } <- getDirs
  let ghcupFilename = "ghcup" <> exeExt
  let ghcupFilepath = binDir </> ghcupFilename

  currentRunningExecPath <- liftIO getExecutablePath

  -- if paths do no exist, warn user, and continue to compare them, as is,
  -- which should eventually fail and result in a non-standard install warning

  p1 <- handleIO' doesNotExistErrorType
                  (handlePathNotPresent currentRunningExecPath)
                  (liftIO $ canonicalizePath currentRunningExecPath)

  p2 <- handleIO' doesNotExistErrorType
                  (handlePathNotPresent ghcupFilepath)
                  (liftIO $ canonicalizePath ghcupFilepath)

  let areEqualPaths = equalFilePath p1 p2

  unless areEqualPaths $ logWarn $ nonStandardInstallLocationMsg currentRunningExecPath

  if isWindows
  then do
    -- since it doesn't seem possible to delete a running exe on windows
    -- we move it to system temp dir, to be deleted at next reboot
    tmp <- liftIO $ getCanonicalTemporaryDirectory >>= \t -> createTempDirectory t "ghcup"
    logDebug $ "mv " <> T.pack ghcupFilepath <> " " <> T.pack (tmp </> "ghcup")
    hideError UnsupportedOperation $
              liftIO $ hideError NoSuchThing $
              moveFile ghcupFilepath (tmp </> "ghcup")
  else
    -- delete it.
    hideError doesNotExistErrorType $ rmFile ghcupFilepath

  where
    handlePathNotPresent fp _err = do
      logDebug $ "Error: The path does not exist, " <> T.pack fp
      pure fp

    nonStandardInstallLocationMsg path = T.pack $
      "current ghcup is invoked from a non-standard location: \n"
      <> path <>
      "\n you may have to uninstall it manually."



    ---------------
    --[ Whereis ]--
    ---------------



-- | Reports the binary location of a given tool:
--
--   * for GHC, this reports: @~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--   * for cabal, this reports @~\/.ghcup\/bin\/cabal-\<ver\>@
--   * for hls, this reports @~\/.ghcup\/bin\/haskell-language-server-wrapper-\<ver\>@
--   * for stack, this reports @~\/.ghcup\/bin\/stack-\<ver\>@
--   * for ghcup, this reports the location of the currently running executable
whereIsTool :: ( MonadReader env m
               , HasDirs env
               , HasLog env
               , MonadThrow m
               , MonadFail m
               , MonadIO m
               , MonadCatch m
               , MonadMask m
               , MonadUnliftIO m
               )
            => Tool
            -> GHCTargetVersion
            -> Excepts '[NotInstalled] m FilePath
whereIsTool tool ver@GHCTargetVersion {..} = do
  dirs <- lift getDirs

  case tool of
    GHC -> do
      whenM (lift $ fmap not $ ghcInstalled ver)
        $ throwE (NotInstalled GHC ver)
      bdir <- fromGHCupPath <$> lift (ghcupGHCDir ver)
      pure (bdir </> "bin" </> ghcBinaryName ver)
    Cabal -> do
      whenM (lift $ fmap not $ cabalInstalled _tvVersion)
        $ throwE (NotInstalled Cabal (GHCTargetVersion Nothing _tvVersion))
      pure (binDir dirs </> "cabal-" <> T.unpack (prettyVer _tvVersion) <> exeExt)
    HLS -> do
      whenM (lift $ fmap not $ hlsInstalled _tvVersion)
        $ throwE (NotInstalled HLS (GHCTargetVersion Nothing _tvVersion))
      ifM (lift $ isLegacyHLS _tvVersion)
        (pure (binDir dirs </> "haskell-language-server-wrapper-" <> T.unpack (prettyVer _tvVersion) <> exeExt))
        $ do
          bdir <- fromGHCupPath <$> lift (ghcupHLSDir _tvVersion)
          pure (bdir </> "bin" </> "haskell-language-server-wrapper" <> exeExt)

    Stack -> do
      whenM (lift $ fmap not $ stackInstalled _tvVersion)
        $ throwE (NotInstalled Stack (GHCTargetVersion Nothing _tvVersion))
      pure (binDir dirs </> "stack-" <> T.unpack (prettyVer _tvVersion) <> exeExt)
    GHCup -> do
      currentRunningExecPath <- liftIO getExecutablePath
      liftIO $ canonicalizePath currentRunningExecPath


-- | Doesn't work for cross GHC.
checkIfToolInstalled :: ( MonadIO m
                        , MonadReader env m
                        , HasDirs env
                        , MonadCatch m) =>
                        Tool ->
                        Version ->
                        m Bool
checkIfToolInstalled tool ver = checkIfToolInstalled' tool (mkTVer ver)


checkIfToolInstalled' :: ( MonadIO m
                         , MonadReader env m
                         , HasDirs env
                         , MonadCatch m) =>
                        Tool ->
                        GHCTargetVersion ->
                        m Bool
checkIfToolInstalled' tool ver =
  case tool of
    Cabal -> cabalInstalled (_tvVersion ver)
    HLS   -> hlsInstalled (_tvVersion ver)
    Stack -> stackInstalled (_tvVersion ver)
    GHC   -> ghcInstalled ver
    _     -> pure False




    --------------------------
    --[ Garbage collection ]--
    --------------------------


rmOldGHC :: ( MonadReader env m
            , HasGHCupInfo env
            , HasDirs env
            , HasLog env
            , MonadIO m
            , MonadFail m
            , MonadMask m
            , MonadUnliftIO m
            )
         => Excepts '[NotInstalled, UninstallFailed] m ()
rmOldGHC = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  let oldGHCs = mkTVer <$> toListOf (ix GHC % getTagged Old % to fst) dls
  ghcs <- lift $ fmap rights getInstalledGHCs
  forM_ ghcs $ \ghc -> when (ghc `elem` oldGHCs) $ rmGHCVer ghc



rmProfilingLibs :: ( MonadReader env m
                   , HasDirs env
                   , HasLog env
                   , MonadIO m
                   , MonadFail m
                   , MonadMask m
                   , MonadUnliftIO m
                   )
                => m ()
rmProfilingLibs = do
  ghcs <- fmap rights getInstalledGHCs

  let regexes :: [ByteString]
      regexes = [[s|.*_p\.a$|], [s|.*\.p_hi$|]]

  forM_ regexes $ \regex ->
    forM_ ghcs $ \ghc -> do
      d <- ghcupGHCDir ghc
      -- TODO: audit findFilesDeep
      matches <- liftIO $ handleIO (\_ -> pure []) $ findFilesDeep
        d
        (makeRegexOpts compExtended
                       execBlank
                       regex
        )
      forM_ matches $ \m -> do
        let p = fromGHCupPath d </> m
        logDebug $ "rm " <> T.pack p
        rmFile p



rmShareDir :: ( MonadReader env m
              , HasDirs env
              , HasLog env
              , MonadIO m
              , MonadFail m
              , MonadMask m
              , MonadUnliftIO m
              )
           => m ()
rmShareDir = do
  ghcs <- fmap rights getInstalledGHCs
  forM_ ghcs $ \ghc -> do
    d <- ghcupGHCDir ghc
    let p = d `appendGHCupPath` "share"
    logDebug $ "rm -rf " <> T.pack (fromGHCupPath p)
    rmPathForcibly p


rmHLSNoGHC :: ( MonadReader env m
              , HasDirs env
              , HasLog env
              , MonadIO m
              , MonadMask m
              , MonadFail m
              , MonadUnliftIO m
              )
           => Excepts '[NotInstalled, UninstallFailed] m ()
rmHLSNoGHC = do
  Dirs {..} <- getDirs
  ghcs <- fmap rights getInstalledGHCs
  hlses <- fmap rights getInstalledHLSs
  forM_ hlses $ \hls -> do
    hlsGHCs <- fmap mkTVer <$> hlsGHCVersions' hls
    let candidates = filter (`notElem` ghcs) hlsGHCs
    if (length hlsGHCs - length candidates) <= 0
    then rmHLSVer hls
    else
      forM_ candidates $ \ghc -> do
        bins1 <- fmap (binDir </>) <$> hlsServerBinaries hls (Just $ _tvVersion ghc)
        bins2 <- ifM (isLegacyHLS hls) (pure []) $ do
          shs <- hlsInternalServerScripts hls (Just $ _tvVersion ghc)
          bins <- hlsInternalServerBinaries hls (Just $ _tvVersion ghc)
          libs <- hlsInternalServerLibs hls (_tvVersion ghc)
          pure (shs ++ bins ++ libs)
        forM_ (bins1 ++ bins2) $ \f -> do
          logDebug $ "rm " <> T.pack f
          rmFile f
    pure ()


rmCache :: ( MonadReader env m
           , HasDirs env
           , HasLog env
           , MonadIO m
           , MonadMask m
           )
        => m ()
rmCache = do
  Dirs {..} <- getDirs
  contents <- liftIO $ listDirectory (fromGHCupPath cacheDir)
  forM_ contents $ \f -> do
    let p = fromGHCupPath cacheDir </> f
    logDebug $ "rm " <> T.pack p
    rmFile p


rmTmp :: ( MonadReader env m
         , HasDirs env
         , HasLog env
         , MonadIO m
         , MonadMask m
         )
      => m ()
rmTmp = do
  ghcup_dirs <- liftIO getGHCupTmpDirs
  forM_ ghcup_dirs $ \f -> do
    logDebug $ "rm -rf " <> T.pack (fromGHCupPath f)
    rmPathForcibly f


