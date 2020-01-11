{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}

module GHCup where


import           GHCup.Download
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Utils.File
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ
import           GHCup.Utils.Version.QQ
import           GHCup.Version

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.ByteString                ( ByteString )
import           Data.List
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Versions
import           Data.Word8
import           GHC.IO.Exception
import           HPath
import           HPath.IO
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.IO.Error
import           System.Posix.FilePath          ( getSearchPath )
import           System.Posix.RawFilePath.Directory.Errors
                                                ( hideError )

import qualified Data.ByteString               as B
import qualified Data.Map.Strict               as Map
import qualified Data.Text.Encoding            as E



    -------------------------
    --[ Tool installation ]--
    -------------------------



installGHCBin :: ( MonadFail m
                 , MonadMask m
                 , MonadCatch m
                 , MonadReader Settings m
                 , MonadLogger m
                 , MonadResource m
                 , MonadIO m
                 )
              => GHCupDownloads
              -> Version
              -> Maybe PlatformRequest -- ^ if Nothing, looks up current host platform
              -> Excepts
                   '[ AlreadyInstalled
                    , BuildFailed
                    , DigestError
                    , DistroNotFound
                    , DownloadFailed
                    , NoCompatibleArch
                    , NoCompatiblePlatform
                    , NoDownload
                    , NotInstalled
                    , UnknownArchive
                    ]
                   m
                   ()
installGHCBin bDls ver mpfReq = do
  lift $ $(logDebug) [i|Requested to install GHC with #{ver}|]
  whenM (liftIO $ toolAlreadyInstalled GHC ver)
    $ (throwE $ AlreadyInstalled GHC ver)
  Settings {..} <- lift ask

  -- download (or use cached version)
  dlinfo        <- liftE $ getDownloadInfo bDls GHC ver mpfReq
  dl            <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack     <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl

  -- prepare paths
  ghcdir <- liftIO $ ghcupGHCDir ver

  -- the subdir of the archive where we do the work
  let archiveSubdir = maybe tmpUnpack (tmpUnpack </>) (view dlSubdir dlinfo)

  catchAllE
      (\es ->
        liftIO (hideError doesNotExistErrorType $ deleteDirRecursive ghcdir)
          >> throwE (BuildFailed archiveSubdir es)
      )
    $ installGHC' archiveSubdir ghcdir

  -- only clean up dir if the build succeeded
  liftIO $ deleteDirRecursive tmpUnpack

  liftE $ postGHCInstall ver

 where
    -- | Install an unpacked GHC distribution. This only deals with the GHC build system and nothing else.
  installGHC' :: (MonadLogger m, MonadIO m)
              => Path Abs      -- ^ Path to the unpacked GHC bindist (where the configure script resides)
              -> Path Abs      -- ^ Path to install to
              -> Excepts '[ProcessError] m ()
  installGHC' path inst = do
    lift $ $(logInfo) [s|Installing GHC (this may take a while)|]
    lEM $ liftIO $ execLogged [s|./configure|]
                              False
                              [[s|--prefix=|] <> toFilePath inst]
                              ([rel|ghc-configure.log|] :: Path Rel)
                              (Just path)
                              Nothing
    lEM $ liftIO $ execLogged [s|make|]
                              True
                              [[s|install|]]
                              ([rel|ghc-make.log|] :: Path Rel)
                              (Just path)
                              Nothing
    pure ()


installCabalBin :: ( MonadMask m
                   , MonadCatch m
                   , MonadReader Settings m
                   , MonadLogger m
                   , MonadResource m
                   , MonadIO m
                   )
                => GHCupDownloads
                -> Version
                -> Maybe PlatformRequest -- ^ if Nothing, looks up current host platform
                -> Excepts
                     '[ CopyError
                      , DigestError
                      , DistroNotFound
                      , DownloadFailed
                      , NoCompatibleArch
                      , NoCompatiblePlatform
                      , NoDownload
                      , UnknownArchive
                      ]
                     m
                     ()
installCabalBin bDls ver mpfReq = do
  lift $ $(logDebug) [i|Requested to install cabal version #{ver}|]
  Settings {..} <- lift ask

  -- download (or use cached version)
  dlinfo        <- liftE $ getDownloadInfo bDls Cabal ver mpfReq
  dl            <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack     <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl

  -- prepare paths
  bindir <- liftIO ghcupBinDir

  -- the subdir of the archive where we do the work
  let archiveSubdir = maybe tmpUnpack (tmpUnpack </>) (view dlSubdir dlinfo)

  liftE $ installCabal' archiveSubdir bindir
  pure ()

 where
    -- | Install an unpacked cabal distribution.
  installCabal' :: (MonadLogger m, MonadCatch m, MonadIO m)
                => Path Abs      -- ^ Path to the unpacked cabal bindist (where the executable resides)
                -> Path Abs      -- ^ Path to install to
                -> Excepts '[CopyError] m ()
  installCabal' path inst = do
    lift $ $(logInfo) [s|Installing cabal|]
    let cabalFile = [rel|cabal|] :: Path Rel
    liftIO $ createDirIfMissing newDirPerms inst
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile
      (path </> cabalFile)
      (inst </> cabalFile)
      Overwrite



    ---------------
    --[ Set GHC ]--
    ---------------



-- | Set GHC symlinks in ~/.ghcup/bin for the requested GHC version. The behavior depends
-- on `SetGHC`:
--
--   * SetGHCOnly: ~/.ghcup/bin/ghc -> ~/.ghcup/ghc/<ver>/bin/ghc
--   * SetGHC_XY: ~/.ghcup/bin/ghc-X.Y -> ~/.ghcup/ghc/<ver>/bin/ghc
--   * SetGHC_XYZ: ~/.ghcup/bin/ghc-<ver> -> ~/.ghcup/ghc/<ver>/bin/ghc
--
-- Additionally creates a ~/.ghcup/share -> ~/.ghcup/ghc/<ver>/share symlink
-- for `SetGHCOnly` constructor.
setGHC :: (MonadLogger m, MonadThrow m, MonadFail m, MonadIO m)
       => Version
       -> SetGHC
       -> Excepts '[NotInstalled] m ()
setGHC ver sghc = do
  let verBS = verToBS ver
  ghcdir <- liftIO $ ghcupGHCDir ver

  -- symlink destination
  bindir <- liftIO $ ghcupBinDir
  liftIO $ hideError AlreadyExists $ createDirRecursive newDirPerms bindir

  -- first delete the old symlinks (this fixes compatibility issues
  -- with old ghcup)
  case sghc of
    SetGHCOnly -> liftE $ rmPlain ver
    SetGHC_XY  -> lift $ rmMajorSymlinks ver
    SetGHC_XYZ -> lift $ rmMinorSymlinks ver

  -- for ghc tools (ghc, ghci, haddock, ...)
  verfiles <- ghcToolFiles ver
  forM_ verfiles $ \file -> do
    liftIO $ hideError doesNotExistErrorType $ deleteFile (bindir </> file)
    targetFile <- case sghc of
      SetGHCOnly -> pure file
      SetGHC_XY  -> do
        major' <-
          (\(mj, mi) -> E.encodeUtf8 $ intToText mj <> [s|.|] <> intToText mi)
            <$> getGHCMajor ver
        parseRel (toFilePath file <> B.singleton _hyphen <> major')
      SetGHC_XYZ -> parseRel (toFilePath file <> B.singleton _hyphen <> verBS)

    -- create symlink
    let fullF = bindir </> targetFile
    let destL = ghcLinkDestination (toFilePath file) ver
    lift $ $(logDebug) [i|ln -s #{destL} #{toFilePath fullF}|]
    liftIO $ createSymlink fullF destL

  -- create symlink for share dir
  lift $ symlinkShareDir ghcdir verBS

  pure ()

 where

  symlinkShareDir :: (MonadIO m, MonadLogger m)
                  => Path Abs
                  -> ByteString
                  -> m ()
  symlinkShareDir ghcdir verBS = do
    destdir <- liftIO $ ghcupBaseDir
    case sghc of
      SetGHCOnly -> do
        let sharedir     = [rel|share|] :: Path Rel
        let fullsharedir = ghcdir </> sharedir
        whenM (liftIO $ doesDirectoryExist fullsharedir) $ do
          let fullF   = destdir </> sharedir
          let targetF = [s|./ghc/|] <> verBS <> [s|/|] <> toFilePath sharedir
          $(logDebug) [i|rm -f #{fullF}|]
          liftIO $ hideError doesNotExistErrorType $ deleteFile fullF
          $(logDebug) [i|ln -s #{targetF} #{fullF}|]
          liftIO $ createSymlink fullF targetF
      _ -> pure ()




    ------------------
    --[ List tools ]--
    ------------------


data ListCriteria = ListInstalled
                  | ListSet
                  deriving Show

data ListResult = ListResult
  { lTool      :: Tool
  , lVer       :: Version
  , lTag       :: [Tag]
  , lInstalled :: Bool
  , lSet       :: Bool
  , fromSrc    :: Bool
  }
  deriving Show


availableToolVersions :: GHCupDownloads -> Tool -> [(Version, [Tag])]
availableToolVersions av tool = toListOf
  (ix tool % to (fmap (\(v, vi) -> (v, (_viTags vi))) . Map.toList) % folded)
  av


listVersions :: GHCupDownloads
             -> Maybe Tool
             -> Maybe ListCriteria
             -> IO [ListResult]
listVersions av lt criteria = case lt of
  Just t -> do
    filter' <$> forM (availableToolVersions av t) (toListResult t)
  Nothing -> do
    ghcvers   <- listVersions av (Just GHC) criteria
    cabalvers <- listVersions av (Just Cabal) criteria
    ghcupvers <- listVersions av (Just GHCup) criteria
    pure (ghcvers <> cabalvers <> ghcupvers)

 where
  toListResult :: Tool -> (Version, [Tag]) -> IO ListResult
  toListResult t (v, tags) = case t of
    GHC -> do
      lSet       <- fmap (maybe False (== v)) $ ghcSet
      lInstalled <- ghcInstalled v
      fromSrc    <- ghcSrcInstalled v
      pure ListResult { lVer = v, lTag = tags, lTool = t, .. }
    Cabal -> do
      lSet       <- fmap (== v) $ cabalSet
      lInstalled <- cabalInstalled v
      pure ListResult { lVer = v, lTag = tags, lTool = t, fromSrc = False, .. }
    GHCup -> do
      let lSet       = prettyPVP ghcUpVer == prettyVer v
      let lInstalled = True
      pure ListResult { lVer = v, lTag = tags, lTool = t, fromSrc = False, .. }


  filter' :: [ListResult] -> [ListResult]
  filter' lr = case criteria of
    Nothing            -> lr
    Just ListInstalled -> filter (\ListResult {..} -> lInstalled) lr
    Just ListSet       -> filter (\ListResult {..} -> lSet) lr



    --------------
    --[ GHC rm ]--
    --------------


-- | This function may throw and crash in various ways.
rmGHCVer :: (MonadThrow m, MonadLogger m, MonadIO m, MonadFail m)
         => Version
         -> Excepts '[NotInstalled] m ()
rmGHCVer ver = do
  isSetGHC <- fmap (maybe False (== ver)) $ ghcSet
  dir      <- liftIO $ ghcupGHCDir ver
  let d' = toFilePath dir
  exists <- liftIO $ doesDirectoryExist dir


  if exists
    then do
      -- this isn't atomic, order matters
      lift $ $(logInfo) [i|Removing directory recursively: #{d'}|]
      liftIO $ deleteDirRecursive dir

      lift $ $(logInfo) [i|Removing ghc-x.y.z symlinks|]
      lift $ rmMinorSymlinks ver

      lift $ $(logInfo) [i|Removing/rewiring ghc-x.y symlinks|]
      -- first remove
      lift $ rmMajorSymlinks ver
      -- then fix them (e.g. with an earlier version)
      (mj, mi) <- getGHCMajor ver
      getGHCForMajor mj mi >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY)


      when isSetGHC $ do
        lift $ $(logInfo) [i|Removing ghc symlinks|]
        liftE $ rmPlain ver

      liftIO
        $   ghcupBaseDir
        >>= hideError doesNotExistErrorType
        .   deleteFile
        .   (</> ([rel|share|] :: Path Rel))
    else throwE (NotInstalled GHC ver)




    ------------------
    --[ Debug info ]--
    ------------------


getDebugInfo :: (MonadLogger m, MonadCatch m, MonadReader Settings m, MonadIO m)
             => Excepts
                  '[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]
                  m
                  DebugInfo
getDebugInfo = do
  diBaseDir   <- liftIO $ ghcupBaseDir
  diBinDir    <- liftIO $ ghcupBinDir
  diGHCDir    <- liftIO $ ghcupGHCBaseDir
  diCacheDir  <- liftIO $ ghcupCacheDir
  diURLSource <- lift $ getUrlSource
  diArch      <- lE getArchitecture
  diPlatform  <- liftE $ getPlatform
  pure $ DebugInfo { .. }




    ---------------
    --[ Compile ]--
    ---------------


compileGHC :: ( MonadMask m
              , MonadReader Settings m
              , MonadThrow m
              , MonadResource m
              , MonadLogger m
              , MonadIO m
              , MonadFail m
              )
           => GHCupDownloads
           -> Version          -- ^ version to install
           -> Version          -- ^ version to bootstrap with
           -> Maybe Int        -- ^ jobs
           -> Maybe (Path Abs) -- ^ build config
           -> Excepts
                '[ AlreadyInstalled
                 , BuildFailed
                 , DigestError
                 , DownloadFailed
                 , GHCupSetError
                 , NoDownload
                 , UnknownArchive
                 ]
                m
                ()
compileGHC dls tver bver jobs mbuildConfig = do
  lift $ $(logDebug) [i|Requested to compile: #{tver} with #{bver}|]
  whenM (liftIO $ toolAlreadyInstalled GHC tver)
        (throwE $ AlreadyInstalled GHC tver)

  -- download source tarball
  dlInfo    <- preview (ix GHC % ix tver % viSourceDL % _Just) dls ?? NoDownload
  dl        <- liftE $ downloadCached dlInfo Nothing

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl

  bghc <- parseRel ([s|ghc-|] <> verToBS bver)
  let workdir = maybe id (flip (</>)) (view dlSubdir dlInfo) $ tmpUnpack
  ghcdir <- liftIO $ ghcupGHCDir tver

  catchAllE
      (\es ->
        liftIO (hideError doesNotExistErrorType $ deleteDirRecursive ghcdir)
          >> throwE (BuildFailed workdir es)
      )
    $ compile bghc ghcdir workdir
  markSrcBuilt ghcdir workdir

  -- only clean up dir if the build succeeded
  liftIO $ deleteDirRecursive tmpUnpack

  reThrowAll GHCupSetError $ postGHCInstall tver
  pure ()

 where
  defaultConf = [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = YES
GhcWithLlvmCodeGen = YES|]

  compile :: (MonadCatch m, MonadLogger m, MonadIO m)
          => Path Rel
          -> Path Abs
          -> Path Abs
          -> Excepts
               '[NoDownload , FileDoesNotExistError , ProcessError]
               m
               ()
  compile bghc ghcdir workdir = do
    lift $ $(logInfo) [i|configuring build|]
    if
      | tver >= [vver|8.8.0|] -> do
        spaths   <- catMaybes . fmap parseAbs <$> liftIO getSearchPath
        bghcPath <- (liftIO $ searchPath spaths bghc) !? NoDownload
        newEnv   <- addToCurrentEnv [([s|GHC|], toFilePath bghcPath)]
        lEM $ liftIO $ execLogged [s|./configure|]
                                  False
                                  [[s|--prefix=|] <> toFilePath ghcdir]
                                  ([rel|ghc-configure.log|] :: Path Rel)
                                  (Just workdir)
                                  (Just newEnv)
      | otherwise -> do
        lEM $ liftIO $ execLogged
          [s|./configure|]
          False
          [ [s|--prefix=|] <> toFilePath ghcdir
          , [s|--with-ghc=|] <> toFilePath bghc
          ]
          ([rel|ghc-configure.log|] :: Path Rel)
          (Just workdir)
          Nothing

    case mbuildConfig of
      Just bc -> liftIOException
        doesNotExistErrorType
        (FileDoesNotExistError $ toFilePath bc)
        (liftIO $ copyFile bc (build_mk workdir) Overwrite)
      Nothing ->
        liftIO $ writeFile (build_mk workdir) (Just newFilePerms) defaultConf

    lift
      $ $(logInfo)
          [i|Building (this may take a while)... Run 'tail -f ~/.ghcup/logs/ghc-make.log' to see the progress.|]
    lEM $ liftIO $ execLogged [s|make|]
                              True
                              (maybe [] (\j -> [[s|-j|] <> fS (show j)]) jobs)
                              ([rel|ghc-make.log|] :: Path Rel)
                              (Just workdir)
                              Nothing

    lift $ $(logInfo) [i|Installing...|]
    lEM $ liftIO $ execLogged [s|make|]
                              True
                              [[s|install|]]
                              ([rel|ghc-make.log|] :: Path Rel)
                              (Just workdir)
                              Nothing

  markSrcBuilt ghcdir workdir = do
    let dest = (ghcdir </> ghcUpSrcBuiltFile)
    liftIO $ copyFile (build_mk workdir) dest Overwrite

  build_mk workdir = workdir </> ([rel|mk/build.mk|] :: Path Rel)


compileCabal :: ( MonadReader Settings m
                , MonadResource m
                , MonadMask m
                , MonadLogger m
                , MonadIO m
                )
             => GHCupDownloads
             -> Version          -- ^ version to install
             -> Version          -- ^ GHC version to build with
             -> Maybe Int
             -> Excepts
                  '[ BuildFailed
                   , DigestError
                   , DownloadFailed
                   , NoDownload
                   , UnknownArchive
                   ]
                  m
                  ()
compileCabal dls tver bver jobs = do
  lift $ $(logDebug) [i|Requested to compile: #{tver} with ghc-#{bver}|]

  -- download source tarball
  dlInfo <- preview (ix Cabal % ix tver % viSourceDL % _Just) dls ?? NoDownload
  dl <- liftE $ downloadCached dlInfo Nothing

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl

  let workdir = maybe id (flip (</>)) (view dlSubdir dlInfo) $ tmpUnpack

  reThrowAll (BuildFailed workdir) $ compile workdir

  -- only clean up dir if the build succeeded
  liftIO $ deleteDirRecursive tmpUnpack

  pure ()

 where
  compile :: (MonadLogger m, MonadIO m)
          => Path Abs
          -> Excepts '[ProcessError] m ()
  compile workdir = do
    lift
      $ $(logInfo)
          [i|Building (this may take a while)... Run 'tail -f ~/.ghcup/logs/cabal-bootstrap.log' to see the progress.|]

    let v' = verToBS bver
    cabal_bin <- liftIO $ ghcupBinDir
    newEnv    <- lift $ addToCurrentEnv
      [ ([s|GHC|]    , [s|ghc-|] <> v')
      , ([s|GHC_PKG|], [s|ghc-pkg-|] <> v')
      , ([s|GHC_VER|], v')
      , ([s|PREFIX|] , toFilePath cabal_bin)
      ]

    lEM $ liftIO $ execLogged [s|./bootstrap.sh|]
                              False
                              (maybe [] (\j -> [[s|-j|], fS (show j)]) jobs)
                              ([rel|cabal-bootstrap.log|] :: Path Rel)
                              (Just workdir)
                              (Just newEnv)




    ---------------------
    --[ Upgrade GHCup ]--
    ---------------------


upgradeGHCup :: ( MonadMask m
                , MonadReader Settings m
                , MonadCatch m
                , MonadLogger m
                , MonadThrow m
                , MonadResource m
                , MonadIO m
                )
             => GHCupDownloads
             -> Maybe (Path Abs)  -- ^ full file destination to write ghcup into
             -> Excepts
                  '[ CopyError
                   , DigestError
                   , DistroNotFound
                   , DownloadFailed
                   , NoCompatibleArch
                   , NoCompatiblePlatform
                   , NoDownload
                   ]
                  m
                  Version
upgradeGHCup dls mtarget = do
  lift $ $(logInfo) [i|Upgrading GHCup...|]
  let latestVer = head $ getTagged dls GHCup Latest
  dli <- liftE $ getDownloadInfo dls GHCup latestVer Nothing
  tmp <- lift withGHCupTmpDir
  let fn = [rel|ghcup|] :: Path Rel
  p <- liftE $ download dli tmp (Just fn)
  case mtarget of
    Nothing -> do
      dest <- liftIO $ ghcupBinDir
      handleIO (throwE . CopyError . show) $ liftIO $ copyFile p
                                                               (dest </> fn)
                                                               Overwrite
    Just fullDest -> liftIO $ copyFile p fullDest Overwrite
  pure latestVer



    -------------
    --[ Other ]--
    -------------


-- | Creates ghc-x.y.z and ghc-x.y symlinks. This is used for
-- both installing from source and bindist.
postGHCInstall :: (MonadLogger m, MonadThrow m, MonadFail m, MonadIO m)
               => Version
               -> Excepts '[NotInstalled] m ()
postGHCInstall ver = do
  liftE $ setGHC ver SetGHC_XYZ

  -- Create ghc-x.y symlinks. This may not be the current
  -- version, create it regardless.
  (mj, mi) <- liftIO $ getGHCMajor ver
  getGHCForMajor mj mi >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY)
