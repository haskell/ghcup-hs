{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

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

#if !defined(TAR)
import           Codec.Archive                  ( ArchiveResult )
#endif
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
                                         hiding ( throwM )
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Text                      ( Text )
import           Data.Versions
import           Data.Word8
import           GHC.IO.Exception
import           HPath
import           HPath.IO                hiding ( hideError )
import           Haskus.Utils.Variant.Excepts
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           Safe                    hiding ( at )
import           System.IO.Error
import           System.Posix.Env.ByteString    ( getEnvironment )
import           System.Posix.FilePath          ( getSearchPath )
import           System.Posix.Files.ByteString

import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E



    -------------------------
    --[ Tool installation ]--
    -------------------------



installGHCBindist :: ( MonadFail m
                 , MonadMask m
                 , MonadCatch m
                 , MonadReader Settings m
                 , MonadLogger m
                 , MonadResource m
                 , MonadIO m
                 )
              => DownloadInfo
              -> Version
              -> PlatformRequest
              -> Excepts
                   '[ AlreadyInstalled
                    , BuildFailed
                    , DigestError
                    , DownloadFailed
                    , NoDownload
                    , NotInstalled
                    , UnknownArchive
#if !defined(TAR)
                    , ArchiveResult
#endif
                    ]
                   m
                   ()
installGHCBindist dlinfo ver (PlatformRequest {..}) = do
  let tver = (mkTVer ver)
  lift $ $(logDebug) [i|Requested to install GHC with #{ver}|]
  whenM (liftIO $ ghcInstalled tver)
    $ (throwE $ AlreadyInstalled GHC ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack                    <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ liftIO $ darwinNotarization _rPlatform tmpUnpack

  -- prepare paths
  ghcdir <- liftIO $ ghcupGHCDir tver

  -- the subdir of the archive where we do the work
  let workdir = maybe tmpUnpack (tmpUnpack </>) (view dlSubdir dlinfo)

  liftE $ runBuildAction tmpUnpack (Just ghcdir) (installGHC' workdir ghcdir)

  liftE $ postGHCInstall tver

 where
  -- | Install an unpacked GHC distribution. This only deals with the GHC build system and nothing else.
  installGHC' :: (MonadReader Settings m, MonadThrow m, MonadLogger m, MonadIO m)
              => Path Abs      -- ^ Path to the unpacked GHC bindist (where the configure script resides)
              -> Path Abs      -- ^ Path to install to
              -> Excepts '[ProcessError] m ()
  installGHC' path inst = do
    lift $ $(logInfo) "Installing GHC (this may take a while)"
    lEM $ execLogged "./configure"
                              False
                              (["--prefix=" <> toFilePath inst] ++ alpineArgs)
                              [rel|ghc-configure|]
                              (Just path)
                              Nothing
    lEM $ make ["install"] (Just path)
    pure ()

  alpineArgs
    | ver >= [vver|8.2.2|]
    , Linux Alpine <- _rPlatform = ["--disable-ld-override"]
    | otherwise = []


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
              -> PlatformRequest
              -> Excepts
                   '[ AlreadyInstalled
                    , BuildFailed
                    , DigestError
                    , DownloadFailed
                    , NoDownload
                    , NotInstalled
                    , UnknownArchive
#if !defined(TAR)
                    , ArchiveResult
#endif
                    ]
                   m
                   ()
installGHCBin bDls ver pfreq = do
  dlinfo <- lE $ getDownloadInfo GHC ver pfreq bDls
  installGHCBindist dlinfo ver pfreq


installCabalBindist :: ( MonadMask m
                       , MonadCatch m
                       , MonadReader Settings m
                       , MonadLogger m
                       , MonadResource m
                       , MonadIO m
                       , MonadFail m
                       )
                    => DownloadInfo
                    -> Version
                    -> PlatformRequest
                    -> Excepts
                         '[ AlreadyInstalled
                          , CopyError
                          , DigestError
                          , DownloadFailed
                          , NoDownload
                          , NotInstalled
                          , UnknownArchive
#if !defined(TAR)
                          , ArchiveResult
#endif
                          ]
                         m
                         ()
installCabalBindist dlinfo ver (PlatformRequest {..}) = do
  lift $ $(logDebug) [i|Requested to install cabal version #{ver}|]

  bindir <- liftIO ghcupBinDir

  whenM
      (liftIO $ cabalInstalled ver >>= \a ->
        handleIO (\_ -> pure False)
          $ fmap (\x -> a && isSymbolicLink x)
          -- ignore when the installation is a legacy cabal (binary, not symlink)
          $ getSymbolicLinkStatus (toFilePath (bindir </> [rel|cabal|]))
      )
    $ (throwE $ AlreadyInstalled Cabal ver)

  -- download (or use cached version)
  dl                           <- liftE $ downloadCached dlinfo Nothing

  -- unpack
  tmpUnpack                    <- lift withGHCupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ liftIO $ darwinNotarization _rPlatform tmpUnpack

  -- the subdir of the archive where we do the work
  let workdir = maybe tmpUnpack (tmpUnpack </>) (view dlSubdir dlinfo)

  liftE $ installCabal' workdir bindir

  -- create symlink if this is the latest version
  cVers <- liftIO $ fmap rights $ getInstalledCabals
  let lInstCabal = headMay . reverse . sort $ cVers
  when (maybe True (ver >=) lInstCabal) $ liftE $ setCabal ver

  pure ()

 where
  -- | Install an unpacked cabal distribution.
  installCabal' :: (MonadLogger m, MonadCatch m, MonadIO m)
                => Path Abs      -- ^ Path to the unpacked cabal bindist (where the executable resides)
                -> Path Abs      -- ^ Path to install to
                -> Excepts '[CopyError] m ()
  installCabal' path inst = do
    lift $ $(logInfo) "Installing cabal"
    let cabalFile = [rel|cabal|]
    liftIO $ createDirIfMissing newDirPerms inst
    destFileName <- lift $ parseRel (toFilePath cabalFile <> "-" <> verToBS ver)
    handleIO (throwE . CopyError . show) $ liftIO $ copyFile
      (path </> cabalFile)
      (inst </> destFileName)
      Overwrite


installCabalBin :: ( MonadMask m
                   , MonadCatch m
                   , MonadReader Settings m
                   , MonadLogger m
                   , MonadResource m
                   , MonadIO m
                   , MonadFail m
                   )
                => GHCupDownloads
                -> Version
                -> PlatformRequest
                -> Excepts
                     '[ AlreadyInstalled
                      , CopyError
                      , DigestError
                      , DownloadFailed
                      , NoDownload
                      , NotInstalled
                      , UnknownArchive
#if !defined(TAR)
                      , ArchiveResult
#endif
                      ]
                     m
                     ()
installCabalBin bDls ver pfreq@(PlatformRequest {..}) = do
  dlinfo <- lE $ getDownloadInfo GHC ver pfreq bDls
  installCabalBindist dlinfo ver pfreq




    ---------------------
    --[ Set GHC/cabal ]--
    ---------------------



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
       => GHCTargetVersion
       -> SetGHC
       -> Excepts '[NotInstalled] m GHCTargetVersion
setGHC ver sghc = do
  let verBS = verToBS (_tvVersion ver)
  ghcdir <- liftIO $ ghcupGHCDir ver

  -- symlink destination
  bindir <- liftIO $ ghcupBinDir
  liftIO $ hideError AlreadyExists $ createDirRecursive newDirPerms bindir

  -- first delete the old symlinks (this fixes compatibility issues
  -- with old ghcup)
  case sghc of
    SetGHCOnly -> liftE $ rmPlain (_tvTarget ver)
    SetGHC_XY  -> lift $ rmMajorSymlinks ver
    SetGHC_XYZ -> lift $ rmMinorSymlinks ver

  -- for ghc tools (ghc, ghci, haddock, ...)
  verfiles <- ghcToolFiles ver
  forM_ verfiles $ \file -> do
    targetFile <- case sghc of
      SetGHCOnly -> pure file
      SetGHC_XY  -> do
        major' <- (\(mj, mi) -> E.encodeUtf8 $ intToText mj <> "." <> intToText mi)
                     <$> getMajorMinorV (_tvVersion ver)
        parseRel (toFilePath file <> B.singleton _hyphen <> major')
      SetGHC_XYZ -> parseRel (toFilePath file <> B.singleton _hyphen <> verBS)

    -- create symlink
    let fullF = bindir </> targetFile
    let destL = ghcLinkDestination (toFilePath file) ver
    lift $ $(logDebug) [i|ln -s #{destL} #{toFilePath fullF}|]
    liftIO $ createSymlink fullF destL

  -- create symlink for share dir
  when (isNothing . _tvTarget $ ver) $ lift $ symlinkShareDir ghcdir verBS

  pure ver

 where

  symlinkShareDir :: (MonadIO m, MonadLogger m)
                  => Path Abs
                  -> ByteString
                  -> m ()
  symlinkShareDir ghcdir verBS = do
    destdir <- liftIO $ ghcupBaseDir
    case sghc of
      SetGHCOnly -> do
        let sharedir     = [rel|share|]
        let fullsharedir = ghcdir </> sharedir
        whenM (liftIO $ doesDirectoryExist fullsharedir) $ do
          let fullF   = destdir </> sharedir
          let targetF = "./ghc/" <> verBS <> "/" <> toFilePath sharedir
          $(logDebug) [i|rm -f #{fullF}|]
          liftIO $ hideError doesNotExistErrorType $ deleteFile fullF
          $(logDebug) [i|ln -s #{targetF} #{fullF}|]
          liftIO $ createSymlink fullF targetF
      _ -> pure ()



-- | Set the ~/.ghcup/bin/cabal symlink.
setCabal :: (MonadLogger m, MonadThrow m, MonadFail m, MonadIO m)
         => Version
         -> Excepts '[NotInstalled] m ()
setCabal ver = do
  let verBS = verToBS ver
  targetFile <- parseRel ("cabal-" <> verBS)

  -- symlink destination
  bindir     <- liftIO $ ghcupBinDir
  liftIO $ hideError AlreadyExists $ createDirRecursive newDirPerms bindir

  whenM (liftIO $ fmap not $ doesFileExist (bindir </> targetFile))
    $ throwE
    $ NotInstalled Cabal (prettyVer ver)

  let cabalbin = bindir </> [rel|cabal|]

  -- delete old file (may be binary or symlink)
  lift $ $(logDebug) [i|rm -f #{toFilePath cabalbin}|]
  liftIO $ hideError doesNotExistErrorType $ deleteFile
    cabalbin

  -- create symlink
  let destL = toFilePath targetFile
  lift $ $(logDebug) [i|ln -s #{destL} #{toFilePath cabalbin}|]
  liftIO $ createSymlink cabalbin destL

  pure ()






    ------------------
    --[ List tools ]--
    ------------------


data ListCriteria = ListInstalled
                  | ListSet
                  deriving Show

data ListResult = ListResult
  { lTool      :: Tool
  , lVer       :: Version
  , lCross     :: Maybe Text -- ^ currently only for GHC
  , lTag       :: [Tag]
  , lInstalled :: Bool
  , lSet       :: Bool -- ^ currently active version
  , fromSrc    :: Bool -- ^ compiled from source
  , lStray     :: Bool -- ^ not in download info
  , lNoBindist :: Bool -- ^ whether the version is available for this platform/arch
  }
  deriving (Eq, Ord, Show)


availableToolVersions :: GHCupDownloads -> Tool -> Map.Map Version [Tag]
availableToolVersions av tool = view
  (at tool % non Map.empty % to (fmap (_viTags)))
  av


-- | List all versions from the download info, as well as stray
-- versions.
listVersions :: ( MonadCatch m
                , MonadLogger m
                , MonadThrow m
                , MonadLogger m
                , MonadIO m
                )
             => GHCupDownloads
             -> Maybe Tool
             -> Maybe ListCriteria
             -> PlatformRequest
             -> m [ListResult]
listVersions av lt criteria pfreq = do
  case lt of
    Just t -> do
      -- get versions from GHCupDownloads
      let avTools = availableToolVersions av t
      lr <- filter' <$> forM (Map.toList avTools) (liftIO . toListResult t)

      case t of
        -- append stray GHCs
        GHC -> do
          slr <- strayGHCs avTools
          pure $ (sort (slr ++ lr))
        _ -> pure lr
    Nothing -> do
      ghcvers   <- listVersions av (Just GHC) criteria pfreq
      cabalvers <- listVersions av (Just Cabal) criteria pfreq
      ghcupvers <- listVersions av (Just GHCup) criteria pfreq
      pure (ghcvers <> cabalvers <> ghcupvers)

 where
  strayGHCs :: (MonadThrow m, MonadLogger m, MonadIO m)
            => Map.Map Version [Tag]
            -> m [ListResult]
  strayGHCs avTools = do
    ghcs <- getInstalledGHCs
    fmap catMaybes $ forM ghcs $ \case
      Right tver@GHCTargetVersion{ _tvTarget = Nothing, .. } -> do
        case Map.lookup _tvVersion avTools of
          Just _  -> pure Nothing
          Nothing -> do
            lSet    <- fmap (maybe False (\(GHCTargetVersion _ v ) -> v == _tvVersion)) $ ghcSet Nothing
            fromSrc <- liftIO $ ghcSrcInstalled tver
            pure $ Just $ ListResult
              { lTool      = GHC
              , lVer       = _tvVersion
              , lCross     = Nothing
              , lTag       = []
              , lInstalled = True
              , lStray     = maybe True (const False) (Map.lookup _tvVersion avTools)
              , lNoBindist = False
              , ..
              }
      Right tver@GHCTargetVersion{ .. } -> do
        lSet    <- fmap (maybe False (\(GHCTargetVersion _ v ) -> v == _tvVersion)) $ ghcSet _tvTarget
        fromSrc <- liftIO $ ghcSrcInstalled tver
        pure $ Just $ ListResult
          { lTool      = GHC
          , lVer       = _tvVersion
          , lCross     = _tvTarget
          , lTag       = []
          , lInstalled = True
          , lStray     = True -- NOTE: cross currently cannot be installed via bindist
          , lNoBindist = False
          , ..
          }
      Left e -> do
        $(logWarn)
          [i|Could not parse version of stray directory #{toFilePath e}|]
        pure Nothing

  -- NOTE: this are not cross ones, because no bindists
  toListResult :: Tool -> (Version, [Tag]) -> IO ListResult
  toListResult t (v, tags) = case t of
    GHC -> do
      let lNoBindist = isLeft $ getDownloadInfo GHC v pfreq av
      let tver = mkTVer v
      lSet       <- fmap (maybe False (\(GHCTargetVersion _ v') -> v' == v)) $ ghcSet Nothing
      lInstalled <- ghcInstalled tver
      fromSrc    <- ghcSrcInstalled tver
      pure ListResult { lVer = v, lCross = Nothing , lTag = tags, lTool = t, lStray = False, .. }
    Cabal -> do
      let lNoBindist = isLeft $ getDownloadInfo Cabal v pfreq av
      lSet <- fmap (maybe False (== v)) $ cabalSet
      lInstalled <- cabalInstalled v
      pure ListResult { lVer    = v
                      , lCross  = Nothing
                      , lTag    = tags
                      , lTool   = t
                      , fromSrc = False
                      , lStray  = False
                      , ..
                      }
    GHCup -> do
      let lSet       = prettyPVP ghcUpVer == prettyVer v
      let lInstalled = lSet
      pure ListResult { lVer    = v
                      , lTag    = tags
                      , lCross  = Nothing
                      , lTool   = t
                      , fromSrc = False
                      , lStray  = False
                      , lNoBindist = False
                      , ..
                      }


  filter' :: [ListResult] -> [ListResult]
  filter' lr = case criteria of
    Nothing            -> lr
    Just ListInstalled -> filter (\ListResult {..} -> lInstalled) lr
    Just ListSet       -> filter (\ListResult {..} -> lSet) lr



    --------------------
    --[ GHC/cabal rm ]--
    --------------------


-- | This function may throw and crash in various ways.
rmGHCVer :: (MonadThrow m, MonadLogger m, MonadIO m, MonadFail m)
         => GHCTargetVersion
         -> Excepts '[NotInstalled] m ()
rmGHCVer ver = do
  isSetGHC <- fmap (maybe False (== ver)) $ ghcSet (_tvTarget ver)
  dir      <- liftIO $ ghcupGHCDir ver
  let d' = toFilePath dir
  exists <- liftIO $ doesDirectoryExist dir


  if exists
    then do
      -- this isn't atomic, order matters
      when isSetGHC $ do
        lift $ $(logInfo) [i|Removing ghc symlinks|]
        liftE $ rmPlain (_tvTarget ver)

      lift $ $(logInfo) [i|Removing directory recursively: #{d'}|]
      liftIO $ deleteDirRecursive dir

      lift $ $(logInfo) [i|Removing ghc-x.y.z symlinks|]
      lift $ rmMinorSymlinks ver

      lift $ $(logInfo) [i|Removing/rewiring ghc-x.y symlinks|]
      -- first remove
      lift $ rmMajorSymlinks ver
      -- then fix them (e.g. with an earlier version)
      (mj, mi) <- getMajorMinorV (_tvVersion ver)
      getGHCForMajor mj mi (_tvTarget ver) >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY)

      liftIO
        $   ghcupBaseDir
        >>= hideError doesNotExistErrorType
        .   deleteFile
        .   (</> [rel|share|])
    else throwE (NotInstalled GHC (ver ^. tvVersion % to prettyVer))


-- | This function may throw and crash in various ways.
rmCabalVer :: (MonadThrow m, MonadLogger m, MonadIO m, MonadFail m)
           => Version
           -> Excepts '[NotInstalled] m ()
rmCabalVer ver = do
  whenM (fmap not $ liftIO $ cabalInstalled ver) $ throwE (NotInstalled GHC (prettyVer ver))

  cSet      <- liftIO cabalSet

  bindir    <- liftIO ghcupBinDir
  cabalFile <- lift $ parseRel ("cabal-" <> verToBS ver)
  liftIO $ hideError doesNotExistErrorType $ deleteFile (bindir </> cabalFile)

  when (maybe False (== ver) cSet) $ do
    cVers <- liftIO $ fmap rights $ getInstalledCabals
    case headMay . reverse . sort $ cVers of
      Just latestver -> setCabal latestver
      Nothing        -> liftIO $ hideError doesNotExistErrorType $ deleteFile
        (bindir </> [rel|cabal|])



    ------------------
    --[ Debug info ]--
    ------------------


getDebugInfo :: (MonadLogger m, MonadCatch m, MonadIO m)
             => Excepts
                  '[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]
                  m
                  DebugInfo
getDebugInfo = do
  diBaseDir  <- liftIO $ ghcupBaseDir
  diBinDir   <- liftIO $ ghcupBinDir
  diGHCDir   <- liftIO $ ghcupGHCBaseDir
  diCacheDir <- liftIO $ ghcupCacheDir
  diArch     <- lE getArchitecture
  diPlatform <- liftE $ getPlatform
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
           -> GHCTargetVersion           -- ^ version to install
           -> Either Version (Path Abs)  -- ^ version to bootstrap with
           -> Maybe Int                  -- ^ jobs
           -> Maybe (Path Abs)           -- ^ build config
           -> Maybe (Path Abs)           -- ^ patch directory
           -> [Text]                     -- ^ additional args to ./configure
           -> PlatformRequest
           -> Excepts
                '[ AlreadyInstalled
                 , BuildFailed
                 , DigestError
                 , DownloadFailed
                 , GHCupSetError
                 , NoDownload
                 , NotFoundInPATH
                 , PatchFailed
                 , UnknownArchive
#if !defined(TAR)
                 , ArchiveResult
#endif
                 ]
                m
                ()
compileGHC dls tver bstrap jobs mbuildConfig patchdir aargs PlatformRequest {..} = do
  lift $ $(logDebug) [i|Requested to compile: #{tver} with #{bstrap}|]
  whenM (liftIO $ ghcInstalled tver)
        (throwE $ AlreadyInstalled GHC (tver ^. tvVersion))

  -- download source tarball
  dlInfo <-
    preview (ix GHC % ix (tver ^. tvVersion) % viSourceDL % _Just) dls
      ?? NoDownload
  dl        <- liftE $ downloadCached dlInfo Nothing

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ liftIO $ darwinNotarization _rPlatform tmpUnpack

  bghc <- case bstrap of
    Right g    -> pure $ Right g
    Left  bver -> Left <$> parseRel ("ghc-" <> verToBS bver)
  let workdir = maybe id (flip (</>)) (view dlSubdir dlInfo) $ tmpUnpack
  ghcdir <- liftIO $ ghcupGHCDir tver

  liftE $ runBuildAction
    tmpUnpack
    (Just ghcdir)
    (compile bghc ghcdir workdir >> markSrcBuilt ghcdir workdir)

  reThrowAll GHCupSetError $ postGHCInstall tver
  pure ()

 where
  defaultConf = case _tvTarget tver of
                  Nothing -> [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = YES|]
                  Just _ -> [s|
V=0
BUILD_MAN = NO
BUILD_SPHINX_HTML = NO
BUILD_SPHINX_PDF = NO
HADDOCK_DOCS = NO
Stage1Only = YES|]

  compile :: (MonadReader Settings m, MonadThrow m, MonadCatch m, MonadLogger m, MonadIO m)
          => Either (Path Rel) (Path Abs)
          -> Path Abs
          -> Path Abs
          -> Excepts
               '[ FileDoesNotExistError
                , InvalidBuildConfig
                , PatchFailed
                , ProcessError
                , NotFoundInPATH
                ]
               m
               ()
  compile bghc ghcdir workdir = do
    lift $ $(logInfo) [i|configuring build|]
    liftE $ checkBuildConfig

    forM_ patchdir $ \dir -> liftE $ applyPatches dir workdir

    cEnv <- liftIO $ getEnvironment

    if
      | (_tvVersion tver) >= [vver|8.8.0|] -> do
        bghcPath <- case bghc of
          Right ghc' -> pure ghc'
          Left  bver -> do
            spaths <- catMaybes . fmap parseAbs <$> liftIO getSearchPath
            (liftIO $ searchPath spaths bver) !? NotFoundInPATH bver
        lEM $ execLogged
          "./configure"
          False
          (  ["--prefix=" <> toFilePath ghcdir]
          ++ (maybe mempty
                    (\x -> ["--target=" <> E.encodeUtf8 x])
                    (_tvTarget tver)
             )
          ++ fmap E.encodeUtf8 aargs
          )
          [rel|ghc-conf|]
          (Just workdir)
          (Just (("GHC", toFilePath bghcPath) : cEnv))
      | otherwise -> do
        lEM $ execLogged
          "./configure"
          False
          (  [ "--prefix=" <> toFilePath ghcdir
             , "--with-ghc=" <> either toFilePath toFilePath bghc
             ]
          ++ (maybe mempty
                    (\x -> ["--target=" <> E.encodeUtf8 x])
                    (_tvTarget tver)
             )
          ++ fmap E.encodeUtf8 aargs
          )
          [rel|ghc-conf|]
          (Just workdir)
          (Just cEnv)

    case mbuildConfig of
      Just bc -> liftIOException
        doesNotExistErrorType
        (FileDoesNotExistError $ toFilePath bc)
        (liftIO $ copyFile bc (build_mk workdir) Overwrite)
      Nothing ->
        liftIO $ writeFile (build_mk workdir) (Just newFilePerms) defaultConf

    lift $ $(logInfo) [i|Building (this may take a while)...|]
    lEM $ make (maybe [] (\j -> ["-j" <> fS (show j)]) jobs)
                        (Just workdir)

    lift $ $(logInfo) [i|Installing...|]
    lEM $ make ["install"] (Just workdir)

  markSrcBuilt ghcdir workdir = do
    let dest = (ghcdir </> ghcUpSrcBuiltFile)
    liftIO $ copyFile (build_mk workdir) dest Overwrite

  build_mk workdir = workdir </> [rel|mk/build.mk|]

  checkBuildConfig :: (MonadCatch m, MonadIO m)
                   => Excepts
                        '[FileDoesNotExistError , InvalidBuildConfig]
                        m
                        ()
  checkBuildConfig = do
    c <- case mbuildConfig of
      Just bc -> do
        BL.toStrict <$> liftIOException doesNotExistErrorType
                                        (FileDoesNotExistError $ toFilePath bc)
                                        (liftIO $ readFile bc)
      Nothing -> pure defaultConf
    let lines' = fmap T.strip . T.lines $ decUTF8Safe c

   -- for cross, we need Stage1Only
    case _tvTarget tver of
      Just _ -> when (not $ elem "Stage1Only = YES" lines') $ throwE
        (InvalidBuildConfig
          [s|Cross compiling needs to be a Stage1 build, add "Stage1Only = YES" to your config!|]
        )
      Nothing -> pure ()



compileCabal :: ( MonadReader Settings m
                , MonadResource m
                , MonadMask m
                , MonadLogger m
                , MonadIO m
                , MonadFail m
                )
             => GHCupDownloads
             -> Version                    -- ^ version to install
             -> Either Version (Path Abs)  -- ^ version to bootstrap with
             -> Maybe Int
             -> Maybe (Path Abs)
             -> PlatformRequest
             -> Excepts
                  '[ AlreadyInstalled
                   , BuildFailed
                   , CopyError
                   , DigestError
                   , DownloadFailed
                   , NoDownload
                   , NotInstalled
                   , PatchFailed
                   , UnknownArchive
#if !defined(TAR)
                   , ArchiveResult
#endif
                   ]
                  m
                  ()
compileCabal dls tver bghc jobs patchdir PlatformRequest{..} = do
  lift $ $(logDebug) [i|Requested to compile: #{tver} with ghc-#{bghc}|]

  bindir <- liftIO ghcupBinDir

  whenM
      (liftIO $ cabalInstalled tver >>= \a ->
        handleIO (\_ -> pure False)
          $ fmap (\x -> a && isSymbolicLink x)
          -- ignore when the installation is a legacy cabal (binary, not symlink)
          $ getSymbolicLinkStatus (toFilePath (bindir </> [rel|cabal|]))
      )
    $ (throwE $ AlreadyInstalled Cabal tver)

  -- download source tarball
  dlInfo <- preview (ix Cabal % ix tver % viSourceDL % _Just) dls ?? NoDownload
  dl <- liftE $ downloadCached dlInfo Nothing

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ unpackToDir tmpUnpack dl
  void $ liftIO $ darwinNotarization _rPlatform tmpUnpack

  let workdir = maybe id (flip (</>)) (view dlSubdir dlInfo) $ tmpUnpack

  cbin         <- liftE $ runBuildAction tmpUnpack Nothing (compile workdir)

  destFileName <- lift $ parseRel ("cabal-" <> verToBS tver)
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile
    cbin
    (bindir </> destFileName)
    Overwrite

  -- create symlink if this is the latest version
  cVers <- liftIO $ fmap rights $ getInstalledCabals
  let lInstCabal = headMay . reverse . sort $ cVers
  when (maybe True (tver >=) lInstCabal) $ liftE $ setCabal tver

  pure ()

 where
  compile :: (MonadReader Settings m, MonadThrow m, MonadLogger m, MonadIO m, MonadResource m)
          => Path Abs
          -> Excepts '[ProcessError , PatchFailed] m (Path Abs)
  compile workdir = do
    lift $ $(logInfo) [i|Building (this may take a while)...|]

    forM_ patchdir $ \dir -> liftE $ applyPatches dir workdir

    ghcEnv <- case bghc of
      Right path -> do
        -- recover the version from /foo/ghc-6.5.4
        bn <- basename path
        let dn  = toFilePath $ dirname path
        let ver = snd . B.break (== _hyphen) . toFilePath $ bn

        pure
          [ ("GHC"    , toFilePath path)
          , ("GHC_PKG", dn <> "/" <> "ghc-pkg" <> ver)
          , ("HADDOCK", dn <> "/" <> "haddock" <> ver)
          ]
      Left bver -> do
        let v' = verToBS bver
        pure
          [ ("GHC"    , "ghc-" <> v')
          , ("GHC_PKG", "ghc-pkg-" <> v')
          , ("HADDOCK", "haddock-" <> v')
          ]

    tmp <- lift withGHCupTmpDir
    liftIO $ createDirRecursive newDirPerms (tmp </> [rel|bin|])
    newEnv <- lift $ addToCurrentEnv (("PREFIX", toFilePath tmp) : ghcEnv)
    lift $ $(logDebug) [i|Environment: #{newEnv}|]

    lEM $ execLogged "./bootstrap.sh"
                              False
                              (maybe [] (\j -> ["-j", fS (show j)]) jobs)
                              [rel|cabal-bootstrap|]
                              (Just workdir)
                              (Just newEnv)
    pure $ (tmp </> [rel|bin/cabal|])




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
             -> Bool              -- ^ whether to force update regardless
                                  --   of currently installed version
             -> PlatformRequest
             -> Excepts
                  '[ CopyError
                   , DigestError
                   , DownloadFailed
                   , NoDownload
                   , NoUpdate
                   ]
                  m
                  Version
upgradeGHCup dls mtarget force pfreq = do
  lift $ $(logInfo) [i|Upgrading GHCup...|]
  let latestVer = fromJust $ getLatest dls GHCup
  when (not force && (latestVer <= pvpToVersion ghcUpVer)) $ throwE NoUpdate
  dli   <- lE $ getDownloadInfo GHCup latestVer pfreq dls
  tmp   <- lift withGHCupTmpDir
  let fn = [rel|ghcup|]
  p <- liftE $ download dli tmp (Just fn)
  let fileMode' =
        newFilePerms
          `unionFileModes` ownerExecuteMode
          `unionFileModes` groupExecuteMode
          `unionFileModes` otherExecuteMode
  binDir <- liftIO $ ghcupBinDir
  let fullDest = fromMaybe (binDir </> fn) mtarget
  liftIO $ hideError NoSuchThing $ deleteFile fullDest
  handleIO (throwE . CopyError . show) $ liftIO $ copyFile p
                                                           fullDest
                                                           Overwrite
  liftIO $ setFileMode (toFilePath fullDest) fileMode'
  pure latestVer



    -------------
    --[ Other ]--
    -------------


-- | Creates ghc-x.y.z and ghc-x.y symlinks. This is used for
-- both installing from source and bindist.
postGHCInstall :: (MonadLogger m, MonadThrow m, MonadFail m, MonadIO m)
               => GHCTargetVersion
               -> Excepts '[NotInstalled] m ()
postGHCInstall ver@GHCTargetVersion{..} = do
  void $ liftE $ setGHC ver SetGHC_XYZ

  -- Create ghc-x.y symlinks. This may not be the current
  -- version, create it regardless.
  (mj, mi) <- getMajorMinorV _tvVersion
  getGHCForMajor mj mi _tvTarget >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY)
