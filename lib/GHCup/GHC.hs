{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

{-|
Module      : GHCup.GHC
Description : GHCup installation functions for GHC
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.GHC where


import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Prelude
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process
import           GHCup.Prelude.String.QQ
import           GHCup.Prelude.Version.QQ
import           GHCup.Prelude.MegaParsec

import           Codec.Archive                  ( ArchiveResult )
import           Control.Applicative
import           Control.Concurrent             ( threadDelay )
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
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import           Data.Time.Clock
import           Data.Time.Format.ISO8601
import           Data.Versions                hiding ( patch )
import           GHC.IO.Exception
import           Haskus.Utils.Variant.Excepts
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( Quasi(qAddDependentFile) )
import           Optics
import           Prelude                 hiding ( abs
                                                , writeFile
                                                )
import           System.Environment
import           System.FilePath
import           System.IO.Error
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           Text.Regex.Posix
import           URI.ByteString

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP


data GHCVer = SourceDist Version
            | GitDist GitBranch
            | RemoteDist URI



    --------------------
    --[ Tool testing ]--
    --------------------



testGHCVer :: ( MonadFail m
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
           => GHCTargetVersion
           -> [T.Text]
           -> Excepts
                '[ DigestError
                 , ContentLengthError
                 , GPGError
                 , DownloadFailed
                 , NoDownload
                 , ArchiveResult
                 , TarDirDoesNotExist
                 , UnknownArchive
                 , TestFailed
                 ]
                m
                ()
testGHCVer ver addMakeArgs = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

  dlInfo <-
    preview (ix GHC % ix ver % viTestDL % _Just) dls
      ?? NoDownload

  liftE $ testGHCBindist dlInfo ver addMakeArgs



testGHCBindist :: ( MonadFail m
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
               => DownloadInfo
               -> GHCTargetVersion
               -> [T.Text]
               -> Excepts
                    '[ DigestError
                     , ContentLengthError
                     , GPGError
                     , DownloadFailed
                     , NoDownload
                     , ArchiveResult
                     , TarDirDoesNotExist
                     , UnknownArchive
                     , TestFailed
                     ]
                    m
                    ()
testGHCBindist dlinfo ver addMakeArgs = do
  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing

  liftE $ testPackedGHC dl (view dlSubdir dlinfo) ver addMakeArgs


testPackedGHC :: ( MonadMask m
                 , MonadCatch m
                 , MonadReader env m
                 , HasDirs env
                 , HasPlatformReq env
                 , HasSettings env
                 , MonadThrow m
                 , HasLog env
                 , MonadIO m
                 , MonadUnliftIO m
                 , MonadFail m
                 , MonadResource m
                 )
              => FilePath          -- ^ Path to the packed GHC bindist
              -> Maybe TarDir      -- ^ Subdir of the archive
              -> GHCTargetVersion  -- ^ The GHC version
              -> [T.Text]          -- ^ additional make args
              -> Excepts
                   '[ ArchiveResult, UnknownArchive, TarDirDoesNotExist, TestFailed ] m ()
testPackedGHC dl msubdir ver addMakeArgs = do
  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack)
                   (liftE . intoSubdir tmpUnpack)
                   msubdir

  reThrowAll @_ @'[ArchiveResult, UnknownArchive, TarDirDoesNotExist, ProcessError]
    (TestFailed (fromGHCupPath workdir)) $ liftE $ runBuildAction tmpUnpack
                         (testUnpackedGHC workdir ver addMakeArgs)

testUnpackedGHC :: ( MonadReader env m
                   , HasDirs env
                   , HasSettings env
                   , MonadThrow m
                   , HasLog env
                   , MonadIO m
                   )
                => GHCupPath         -- ^ Path to the unpacked GHC bindist (where the make file resides)
                -> GHCTargetVersion  -- ^ The GHC version
                -> [T.Text]          -- ^ additional configure args for bindist
                -> Excepts '[ProcessError] m ()
testUnpackedGHC path tver addMakeArgs = do
  lift $ logInfo $ "Testing GHC version " <> tVerToText tver <> "!"
  ghcDir <- lift $ ghcupGHCDir tver
  let ghcBinDir = fromGHCupPath ghcDir </> "bin"
  env <- liftIO $ addToPath ghcBinDir False

  lEM $ make' (fmap T.unpack addMakeArgs)
              (Just $ fromGHCupPath path)
              "ghc-test"
              (Just $ ("STAGE1_GHC", maybe "" (T.unpack . (<> "-")) (_tvTarget tver)
                                     <> "ghc-"
                                     <> T.unpack (prettyVer $ _tvVersion tver)) : env)
  pure ()


    ---------------------
    --[ Tool fetching ]--
    ---------------------



fetchGHCSrc :: ( MonadFail m
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
            => GHCTargetVersion
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
fetchGHCSrc v mfp = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
  dlInfo <-
    preview (ix GHC % ix v % viSourceDL % _Just) dls
      ?? NoDownload
  liftE $ downloadCached' dlInfo Nothing mfp



    -------------------------
    --[ Tool installation ]--
    -------------------------


-- | Like 'installGHCBin', except takes the 'DownloadInfo' as
-- argument instead of looking it up from 'GHCupDownloads'.
installGHCBindist :: ( MonadFail m
                     , MonadMask m
                     , MonadCatch m
                     , MonadReader env m
                     , HasDirs env
                     , HasSettings env
                     , HasPlatformReq env
                     , HasLog env
                     , MonadResource m
                     , MonadIO m
                     , MonadUnliftIO m
                     )
                  => DownloadInfo    -- ^ where/how to download
                  -> GHCTargetVersion -- ^ the version to install
                  -> InstallDir
                  -> Bool            -- ^ Force install
                  -> [T.Text]        -- ^ additional configure args for bindist
                  -> Excepts
                       '[ AlreadyInstalled
                        , BuildFailed
                        , DigestError
                        , ContentLengthError
                        , GPGError
                        , DownloadFailed
                        , NoDownload
                        , NotInstalled
                        , UnknownArchive
                        , TarDirDoesNotExist
                        , DirNotEmpty
                        , ArchiveResult
                        , ProcessError
                        , UninstallFailed
                        , MergeFileTreeError
                        ]
                       m
                       ()
installGHCBindist dlinfo tver installDir forceInstall addConfArgs = do
  lift $ logDebug $ "Requested to install GHC with " <> tVerToText tver

  regularGHCInstalled <- lift $ ghcInstalled tver

  if
    | not forceInstall
    , regularGHCInstalled
    , GHCupInternal <- installDir -> do
        throwE $ AlreadyInstalled GHC (_tvVersion tver)

    | forceInstall
    , regularGHCInstalled
    , GHCupInternal <- installDir -> do
        lift $ logInfo "Removing the currently installed GHC version first!"
        liftE $ rmGHCVer tver

    | otherwise -> pure ()

  -- download (or use cached version)
  dl <- liftE $ downloadCached dlinfo Nothing


  toolchainSanityChecks

  case installDir of
    IsolateDir isoDir -> do                        -- isolated install
      lift $ logInfo $ "isolated installing GHC to " <> T.pack isoDir
      liftE $ installPackedGHC dl (view dlSubdir dlinfo) (IsolateDirResolved isoDir) tver forceInstall addConfArgs
    GHCupInternal -> do                            -- regular install
      -- prepare paths
      ghcdir <- lift $ ghcupGHCDir tver

      liftE $ installPackedGHC dl (view dlSubdir dlinfo) (GHCupDir ghcdir) tver forceInstall addConfArgs

      -- make symlinks & stuff when regular install,
      liftE $ postGHCInstall tver

 where
  toolchainSanityChecks = do
    r <- forM ["CC", "LD"] (liftIO . lookupEnv)
    case catMaybes r of
      [] -> pure ()
      _ -> do
        lift $ logWarn $ "CC/LD environment variable is set. This will change the compiler/linker"
         <> "\n" <> "GHC uses internally and can cause defunct GHC in some cases (e.g. in Anaconda"
         <> "\n" <> "environments). If you encounter problems, unset CC and LD and reinstall."


-- | Install a packed GHC distribution. This only deals with unpacking and the GHC
-- build system and nothing else.
installPackedGHC :: ( MonadMask m
                    , MonadCatch m
                    , MonadReader env m
                    , HasDirs env
                    , HasPlatformReq env
                    , HasSettings env
                    , MonadThrow m
                    , HasLog env
                    , MonadIO m
                    , MonadUnliftIO m
                    , MonadFail m
                    , MonadResource m
                    )
                 => FilePath          -- ^ Path to the packed GHC bindist
                 -> Maybe TarDir      -- ^ Subdir of the archive
                 -> InstallDirResolved
                 -> GHCTargetVersion  -- ^ The GHC version
                 -> Bool              -- ^ Force install
                 -> [T.Text]          -- ^ additional configure args for bindist
                 -> Excepts
                      '[ BuildFailed
                       , UnknownArchive
                       , TarDirDoesNotExist
                       , DirNotEmpty
                       , ArchiveResult
                       , ProcessError
                       , MergeFileTreeError
                       ] m ()
installPackedGHC dl msubdir inst ver forceInstall addConfArgs = do
  PlatformRequest {..} <- lift getPlatformReq

  unless forceInstall
    (liftE $ installDestSanityCheck inst)

  -- unpack
  tmpUnpack <- lift mkGhcupTmpDir
  liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
  liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)

  -- the subdir of the archive where we do the work
  workdir <- maybe (pure tmpUnpack)
                   (liftE . intoSubdir tmpUnpack)
                   msubdir

  liftE $ runBuildAction tmpUnpack
                         (installUnpackedGHC workdir inst ver forceInstall addConfArgs)


-- | Install an unpacked GHC distribution. This only deals with the GHC
-- build system and nothing else.
installUnpackedGHC :: ( MonadReader env m
                      , HasPlatformReq env
                      , HasDirs env
                      , HasSettings env
                      , MonadThrow m
                      , HasLog env
                      , MonadIO m
                      , MonadUnliftIO m
                      , MonadMask m
                      , MonadResource m
                      , MonadFail m
                      )
                   => GHCupPath           -- ^ Path to the unpacked GHC bindist (where the configure script resides)
                   -> InstallDirResolved  -- ^ Path to install to
                   -> GHCTargetVersion    -- ^ The GHC version
                   -> Bool                -- ^ Force install
                   -> [T.Text]          -- ^ additional configure args for bindist
                   -> Excepts '[ProcessError, MergeFileTreeError] m ()
installUnpackedGHC path inst tver forceInstall addConfArgs
  | isWindows = do
      lift $ logInfo "Installing GHC (this may take a while)"
      -- Windows bindists are relocatable and don't need
      -- to run configure.
      -- We also must make sure to preserve mtime to not confuse ghc-pkg.
      liftE $ mergeGHCFileTree path inst tver forceInstall
  | otherwise = do
      PlatformRequest {..} <- lift getPlatformReq

      let ldOverride
           | _tvVersion tver >= [vver|8.2.2|]
           , _rPlatform `elem` [Linux Alpine, Darwin]
           = ["--disable-ld-override"]
           | otherwise
           = []

      lift $ logInfo "Installing GHC (this may take a while)"
      lEM $ execLogged "sh"
                       ("./configure" : ("--prefix=" <> fromInstallDir inst)
                        : (maybe mempty (\x -> ["--target=" <> T.unpack x]) (_tvTarget tver) <> ldOverride <> (T.unpack <$> addConfArgs))
                       )
                       (Just $ fromGHCupPath path)
                       "ghc-configure"
                       Nothing
      tmpInstallDest <- lift withGHCupTmpDir
      lEM $ make ["DESTDIR=" <> fromGHCupPath tmpInstallDest, "install"] (Just $ fromGHCupPath path)
      liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpInstallDest)
      liftE $ mergeGHCFileTree (tmpInstallDest `appendGHCupPath` dropDrive (fromInstallDir inst)) inst tver forceInstall
      pure ()


mergeGHCFileTree :: ( MonadReader env m
                    , HasPlatformReq env
                    , HasDirs env
                    , HasSettings env
                    , MonadThrow m
                    , HasLog env
                    , MonadIO m
                    , MonadUnliftIO m
                    , MonadMask m
                    , MonadResource m
                    , MonadFail m
                    )
                 => GHCupPath           -- ^ Path to the root of the tree
                 -> InstallDirResolved  -- ^ Path to install to
                 -> GHCTargetVersion    -- ^ The GHC version
                 -> Bool                -- ^ Force install
                 -> Excepts '[MergeFileTreeError] m ()
mergeGHCFileTree root inst tver forceInstall
  | isWindows = do
      liftE $ mergeFileTree root inst GHC tver $ \source dest -> do
        mtime <- liftIO $ ifM (pathIsSymbolicLink source) (pure Nothing) (Just <$> getModificationTime source)
        when forceInstall $ hideError doesNotExistErrorType $ hideError InappropriateType $ recycleFile dest
        liftIO $ moveFilePortable source dest
        forM_ mtime $ liftIO . setModificationTime dest
  | otherwise = do
      liftE $ mergeFileTree root
        inst
        GHC
        tver
        (\f t -> liftIO $ do
            mtime <- ifM (pathIsSymbolicLink f) (pure Nothing) (Just <$> getModificationTime f)
            install f t (not forceInstall)
            forM_ mtime $ setModificationTime t)


-- | Installs GHC into @~\/.ghcup\/ghc/\<ver\>@ and places the
-- following symlinks in @~\/.ghcup\/bin@:
--
--   * @ghc-x.y.z -> ..\/ghc\/x.y.z\/bin/ghc@
--   * @ghc-x.y   -> ..\/ghc\/x.y.z\/bin/ghc@ (if x.y.z is the latest x.y version)
installGHCBin :: ( MonadFail m
                 , MonadMask m
                 , MonadCatch m
                 , MonadReader env m
                 , HasPlatformReq env
                 , HasGHCupInfo env
                 , HasDirs env
                 , HasSettings env
                 , HasLog env
                 , MonadResource m
                 , MonadIO m
                 , MonadUnliftIO m
                 )
              => GHCTargetVersion -- ^ the version to install
              -> InstallDir
              -> Bool            -- ^ force install
              -> [T.Text]        -- ^ additional configure args for bindist
              -> Excepts
                   '[ AlreadyInstalled
                    , BuildFailed
                    , DigestError
                    , ContentLengthError
                    , GPGError
                    , DownloadFailed
                    , NoDownload
                    , NotInstalled
                    , UnknownArchive
                    , TarDirDoesNotExist
                    , DirNotEmpty
                    , ArchiveResult
                    , ProcessError
                    , UninstallFailed
                    , MergeFileTreeError
                    ]
                   m
                   ()
installGHCBin tver installDir forceInstall addConfArgs = do
  dlinfo <- liftE $ getDownloadInfo' GHC tver
  liftE $ installGHCBindist dlinfo tver installDir forceInstall addConfArgs





    ---------------
    --[ Set GHC ]--
    ---------------



-- | Set GHC symlinks in @~\/.ghcup\/bin@ for the requested GHC version. The behavior depends
-- on `SetGHC`:
--
--   * SetGHCOnly: @~\/.ghcup\/bin\/ghc -> ~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--   * SetGHC_XY: @~\/.ghcup\/bin\/ghc-X.Y -> ~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--   * SetGHC_XYZ: @~\/.ghcup\/bin\/ghc-\<ver\> -> ~\/.ghcup\/ghc\/\<ver\>\/bin\/ghc@
--
-- Additionally creates a @~\/.ghcup\/share -> ~\/.ghcup\/ghc\/\<ver\>\/share symlink@
-- for 'SetGHCOnly' constructor.
setGHC :: ( MonadReader env m
          , HasDirs env
          , HasLog env
          , MonadThrow m
          , MonadFail m
          , MonadIO m
          , MonadCatch m
          , MonadMask m
          , MonadUnliftIO m
          )
       => GHCTargetVersion
       -> SetGHC
       -> Maybe FilePath  -- if set, signals that we're not operating in ~/.ghcup/bin
                          -- and don't want mess with other versions
       -> Excepts '[NotInstalled] m GHCTargetVersion
setGHC ver sghc mBinDir = do
  let verS = T.unpack $ prettyVer (_tvVersion ver)
  ghcdir                        <- lift $ ghcupGHCDir ver

  whenM (lift $ not <$> ghcInstalled ver) (throwE (NotInstalled GHC ver))

  -- symlink destination
  binDir <- case mBinDir of
    Just x -> pure x
    Nothing -> do
      Dirs {binDir = f} <- lift getDirs
      pure f

  -- first delete the old symlinks (this fixes compatibility issues
  -- with old ghcup)
  when (isNothing mBinDir) $
    case sghc of
      SetGHCOnly -> liftE $ rmPlainGHC (_tvTarget ver)
      SetGHC_XY  -> liftE $ rmMajorGHCSymlinks ver
      SetGHC_XYZ -> liftE $ rmMinorGHCSymlinks ver

  -- for ghc tools (ghc, ghci, haddock, ...)
  verfiles <- ghcToolFiles ver
  forM_ verfiles $ \file -> do
    mTargetFile <- case sghc of
      SetGHCOnly -> pure $ Just file
      SetGHC_XY  -> do
        handle
            (\(e :: ParseError) -> lift $ logWarn (T.pack $ displayException e) >> pure Nothing)
          $ do
            (mj, mi) <- getMajorMinorV (_tvVersion ver)
            let major' = intToText mj <> "." <> intToText mi
            pure $ Just (file <> "-" <> T.unpack major')
      SetGHC_XYZ ->
        pure $ Just (file <> "-" <> verS)

    -- create symlink
    forM_ mTargetFile $ \targetFile -> do
      bindir <- ghcInternalBinDir ver
      let fullF = binDir </> targetFile  <> exeExt
          fileWithExt = bindir </> file <> exeExt
      destL <- binarySymLinkDestination binDir fileWithExt
      lift $ createLink destL fullF

      when (targetFile == "ghc") $
        liftIO (isShadowed fullF) >>= \case
          Nothing -> pure ()
          Just pa -> lift $ logWarn $ T.pack $ prettyHFError (ToolShadowed GHC pa fullF (_tvVersion ver))

  when (isNothing mBinDir) $ do
    -- create symlink for share dir
    when (isNothing . _tvTarget $ ver) $ lift $ symlinkShareDir (fromGHCupPath ghcdir) verS

    when (sghc == SetGHCOnly) $ lift warnAboutHlsCompatibility

  pure ver

 where

  symlinkShareDir :: ( MonadReader env m
                     , HasDirs env
                     , MonadIO m
                     , HasLog env
                     , MonadCatch m
                     , MonadMask m
                     )
                  => FilePath
                  -> String
                  -> m ()
  symlinkShareDir ghcdir ver' = do
    Dirs {..} <- getDirs
    let destdir = fromGHCupPath baseDir
    case sghc of
      SetGHCOnly -> do
        let sharedir     = "share"
        let fullsharedir = ghcdir </> sharedir
        logDebug $ "Checking for sharedir existence: " <> T.pack fullsharedir
        whenM (liftIO $ doesDirectoryExist fullsharedir) $ do
          let fullF   = destdir </> sharedir
          let targetF = "." </> "ghc" </> ver' </> sharedir
          logDebug $ "rm -f " <> T.pack fullF
          hideError doesNotExistErrorType $ rmDirectoryLink fullF
          logDebug $ "ln -s " <> T.pack targetF <> " " <> T.pack fullF

          if isWindows
          then liftIO
                 -- On windows we need to be more permissive
                 -- in case symlinks can't be created, be just
                 -- give up here. This symlink isn't strictly necessary.
                 $ hideError permissionErrorType
                 $ hideError illegalOperationErrorType
                 $ createDirectoryLink targetF fullF
          else liftIO
                 $ createDirectoryLink targetF fullF
      _ -> pure ()

unsetGHC :: ( MonadReader env m
            , HasDirs env
            , HasLog env
            , MonadThrow m
            , MonadFail m
            , MonadIO m
            , MonadMask m
            )
         => Maybe Text
         -> Excepts '[NotInstalled] m ()
unsetGHC = rmPlainGHC





    --------------
    --[ GHC rm ]--
    --------------


-- | Delete a ghc version and all its symlinks.
--
-- This may leave GHCup without a "set" version.
-- Will try to fix the ghc-x.y symlink after removal (e.g. to an
-- older version).
rmGHCVer :: ( MonadReader env m
            , HasDirs env
            , MonadThrow m
            , HasLog env
            , MonadIO m
            , MonadFail m
            , MonadCatch m
            , MonadMask m
            , MonadUnliftIO m
            )
         => GHCTargetVersion
         -> Excepts '[NotInstalled, UninstallFailed] m ()
rmGHCVer ver = do
  isSetGHC <- lift $ fmap (== Just ver) $ ghcSet (_tvTarget ver)

  whenM (lift $ fmap not $ ghcInstalled ver) (throwE (NotInstalled GHC ver))

  -- this isn't atomic, order matters
  when isSetGHC $ do
    lift $ logInfo "Removing ghc symlinks"
    liftE $ rmPlainGHC (_tvTarget ver)

  lift $ logInfo "Removing ghc-x.y.z symlinks"
  liftE $ rmMinorGHCSymlinks ver

  lift $ logInfo "Removing/rewiring ghc-x.y symlinks"
  -- first remove
  handle (\(_ :: ParseError) -> pure ()) $ liftE $ rmMajorGHCSymlinks ver
  -- then fix them (e.g. with an earlier version)

  dir' <- lift $ ghcupGHCDir ver
  let dir = fromGHCupPath dir'
  lift (getInstalledFiles GHC ver) >>= \case
    Just files -> do
      lift $ logInfo $ "Removing files safely from: " <> T.pack dir
      forM_ files (lift . hideError NoSuchThing . recycleFile . (\f -> dir </> dropDrive f))
      hideError UnsatisfiedConstraints $ removeEmptyDirsRecursive dir
      survivors <- liftIO $ hideErrorDef [doesNotExistErrorType] [] $ listDirectory dir
      f <- recordedInstallationFile GHC ver
      lift $ recycleFile f
      when (not (null survivors)) $ throwE $ UninstallFailed dir survivors
    Nothing -> do
      isDir <- liftIO $ doesDirectoryExist dir
      isSyml <- liftIO $ handleIO (\_ -> pure False) $ pathIsSymbolicLink dir
      when (isDir && not isSyml) $ do
        lift $ logInfo $ "Removing legacy directory recursively: " <> T.pack dir
        recyclePathForcibly dir'

  v' <-
    handle
      (\(e :: ParseError) -> lift $ logWarn (T.pack $ displayException e) >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV (_tvVersion ver)
  forM_ v' $ \(mj, mi) -> lift (getGHCForPVP (PVP (fromIntegral mj :| [fromIntegral mi])) (_tvTarget ver))
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY Nothing)

  Dirs {..} <- lift getDirs

  lift $ hideError doesNotExistErrorType $ rmDirectoryLink (fromGHCupPath baseDir </> "share")




    ---------------
    --[ Compile ]--
    ---------------


-- | Compile a GHC from source. This behaves wrt symlinks and installation
-- the same as 'installGHCBin'.
compileGHC :: ( MonadMask m
              , MonadReader env m
              , HasDirs env
              , HasPlatformReq env
              , HasGHCupInfo env
              , HasSettings env
              , MonadThrow m
              , MonadResource m
              , HasLog env
              , MonadIO m
              , MonadUnliftIO m
              , MonadFail m
              )
           => GHCVer
           -> Maybe Text               -- ^ cross target
           -> Maybe Version            -- ^ overwrite version
           -> Either Version FilePath  -- ^ version to bootstrap with
           -> Maybe Int                -- ^ jobs
           -> Maybe FilePath           -- ^ build config
           -> Maybe (Either FilePath [URI])  -- ^ patches
           -> [Text]                   -- ^ additional args to ./configure
           -> Maybe String             -- ^ build flavour
           -> Maybe BuildSystem
           -> InstallDir
           -> Excepts
                '[ AlreadyInstalled
                 , BuildFailed
                 , DigestError
                 , ContentLengthError
                 , GPGError
                 , DownloadFailed
                 , GHCupSetError
                 , NoDownload
                 , NotFoundInPATH
                 , PatchFailed
                 , UnknownArchive
                 , TarDirDoesNotExist
                 , NotInstalled
                 , DirNotEmpty
                 , ArchiveResult
                 , FileDoesNotExistError
                 , HadrianNotFound
                 , InvalidBuildConfig
                 , ProcessError
                 , CopyError
                 , BuildFailed
                 , UninstallFailed
                 , MergeFileTreeError
                 ]
                m
                GHCTargetVersion
compileGHC targetGhc crossTarget ov bstrap jobs mbuildConfig patches aargs buildFlavour buildSystem installDir
  = do
    PlatformRequest { .. } <- lift getPlatformReq
    GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

    (workdir, tmpUnpack, tver) <- case targetGhc of
      -- unpack from version tarball
      SourceDist ver -> do
        lift $ logDebug $ "Requested to compile: " <> prettyVer ver <> " with " <> either prettyVer T.pack bstrap

        -- download source tarball
        dlInfo <-
          preview (ix GHC % ix (mkTVer ver) % viSourceDL % _Just) dls
            ?? NoDownload
        dl <- liftE $ downloadCached dlInfo Nothing

        -- unpack
        tmpUnpack <- lift mkGhcupTmpDir
        liftE $ cleanUpOnError tmpUnpack (unpackToDir (fromGHCupPath tmpUnpack) dl)
        liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform $ fromGHCupPath tmpUnpack

        workdir <- maybe (pure tmpUnpack)
                         (liftE . intoSubdir tmpUnpack)
                         (view dlSubdir dlInfo)
        liftE $ applyAnyPatch patches (fromGHCupPath workdir)

        pure (workdir, tmpUnpack, Just (GHCTargetVersion crossTarget ver))

      RemoteDist uri -> do
        lift $ logDebug $ "Requested to compile (from uri): " <> T.pack (show uri)

        -- download source tarball
        tmpDownload <- lift withGHCupTmpDir
        tmpUnpack <- lift mkGhcupTmpDir
        tar <- liftE $ download uri Nothing Nothing Nothing (fromGHCupPath tmpDownload) Nothing False
        (bf, tver) <- liftE $ cleanUpOnError @'[UnknownArchive, ArchiveResult, ProcessError] tmpUnpack $ do
          liftE $ unpackToDir (fromGHCupPath tmpUnpack) tar
          let regex = [s|^(.*/)*boot$|] :: B.ByteString
          [bootFile] <- liftIO $ findFilesDeep
            tmpUnpack
            (makeRegexOpts compExtended
                           execBlank
                           regex
            )
          tver <- liftE $ catchAllE @_ @'[ProcessError, ParseError] @'[] (\_ -> pure Nothing) $ fmap Just $ getGHCVer
            (appendGHCupPath tmpUnpack (takeDirectory bootFile))
          pure (bootFile, tver)

        let workdir = appendGHCupPath tmpUnpack (takeDirectory bf)

        pure (workdir, tmpUnpack, GHCTargetVersion crossTarget <$> tver)

      -- clone from git
      GitDist GitBranch{..} -> do
        tmpUnpack <- lift mkGhcupTmpDir
        let git args = execLogged "git" ("--no-pager":args) (Just $ fromGHCupPath tmpUnpack) "git" Nothing
        tver <- reThrowAll @_ @'[PatchFailed, ProcessError, NotFoundInPATH, DigestError, ContentLengthError, DownloadFailed, GPGError] DownloadFailed $ do
          let rep = fromMaybe "https://gitlab.haskell.org/ghc/ghc.git" repo
          lift $ logInfo $ "Fetching git repo " <> T.pack rep <> " at ref " <> T.pack ref <> " (this may take a while)"
          lEM $ git [ "init" ]
          lEM $ git [ "remote"
                    , "add"
                    , "origin"
                    , fromString rep ]

          -- figure out if we can do a shallow clone
          remoteBranches <- catchE @ProcessError @'[PatchFailed, ProcessError, NotFoundInPATH, DigestError, ContentLengthError, DownloadFailed, GPGError] @'[PatchFailed, NotFoundInPATH, DigestError, DownloadFailed, GPGError] (\(_ :: ProcessError) -> pure [])
              $ fmap processBranches $ gitOut ["ls-remote", "--heads", "origin"] (fromGHCupPath tmpUnpack)
          let shallow_clone
                | isCommitHash ref                     = True
                | fromString ref `elem` remoteBranches = True
                | otherwise                            = False
          lift $ logDebug $ "Shallow clone: " <> T.pack (show shallow_clone)

          -- fetch
          let fetch_args
                | shallow_clone = ["fetch", "--depth", "1", "--quiet", "origin", fromString ref]
                | otherwise     = ["fetch", "--tags",       "--quiet", "origin"                ]
          lEM $ git fetch_args

          -- initial checkout
          lEM $ git [ "checkout", fromString ref ]

          -- gather some info
          git_describe <- if shallow_clone
                          then pure Nothing
                          else fmap Just $ liftE $ gitOut ["describe", "--tags"] (fromGHCupPath tmpUnpack)
          chash <- liftE $ gitOut ["rev-parse", "HEAD" ] (fromGHCupPath tmpUnpack)

          -- clone submodules
          lEM $ git [ "submodule", "update", "--init", "--depth", "1" ]

          -- apply patches
          liftE $ applyAnyPatch patches (fromGHCupPath tmpUnpack)

          -- bootstrap
          tver <- liftE $ catchAllE @_ @'[ProcessError, ParseError] @'[] (\_ -> pure Nothing) $ fmap Just $ getGHCVer
            tmpUnpack
          liftE $ catchWarn $ lEM @_ @'[ProcessError] $ darwinNotarization _rPlatform (fromGHCupPath tmpUnpack)
          lift $ logInfo $ "Examining git ref " <> T.pack ref <> "\n  " <>
                           "GHC version (from Makefile): " <> T.pack (show (prettyVer <$> tver)) <>
                           (if not shallow_clone then "\n  " <> "'git describe' output: " <> fromJust git_describe else mempty) <>
                           (if isCommitHash ref then mempty else "\n  " <> "commit hash: " <> chash)
          liftIO $ threadDelay 5000000 -- give the user a sec to intervene

          pure tver

        pure (tmpUnpack, tmpUnpack, GHCTargetVersion crossTarget <$> tver)
    -- the version that's installed may differ from the
    -- compiled version, so the user can overwrite it
    installVer <- if | Just ov'   <- ov   -> pure (GHCTargetVersion crossTarget ov')
                     | Just tver' <- tver -> pure tver'
                     | otherwise          -> fail "No GHC version given and couldn't detect version. Giving up..."

    alreadyInstalled <- lift $ ghcInstalled installVer
    alreadySet <- fmap (== Just installVer) $ lift $ ghcSet (_tvTarget installVer)

    when alreadyInstalled $ do
      case installDir of
        IsolateDir isoDir ->
          lift $ logWarn $ "GHC " <> T.pack (prettyShow installVer) <> " already installed. Isolate installing to " <> T.pack isoDir
        GHCupInternal ->
          lift $ logWarn $ "GHC " <> T.pack (prettyShow installVer) <> " already installed. Will overwrite existing version."
      lift $ logWarn
        "...waiting for 10 seconds before continuing, you can still abort..."
      liftIO $ threadDelay 10000000 -- give the user a sec to intervene

    ghcdir <- case installDir of
      IsolateDir isoDir -> pure $ IsolateDirResolved isoDir
      GHCupInternal -> GHCupDir <$> lift (ghcupGHCDir installVer)

    mBindist <- liftE $ runBuildAction
      tmpUnpack
      (do
        -- prefer 'tver', because the real version carries out compatibility checks
        -- we don't want the user to do funny things with it
        let doHadrian = compileHadrianBindist (fromMaybe installVer tver) (fromGHCupPath workdir) ghcdir
            doMake    = compileMakeBindist (fromMaybe installVer tver) (fromGHCupPath workdir) ghcdir
        case buildSystem of
          Just Hadrian -> do
            lift $ logInfo "Requested to use Hadrian"
            liftE doHadrian
          Just Make -> do
            lift $ logInfo "Requested to use Make"
            doMake
          Nothing -> do
            supportsHadrian <- liftE $ catchE @HadrianNotFound @'[HadrianNotFound] @'[] (\_ -> return False)
                                 $ fmap (const True)
                                 $ findHadrianFile (fromGHCupPath workdir)
            if supportsHadrian
            then do
              lift $ logInfo "Detected Hadrian"
              liftE doHadrian
            else do
              lift $ logInfo "Detected Make"
              doMake
      )

    case installDir of
      GHCupInternal ->
        -- only remove old ghc in regular installs
        when alreadyInstalled $ do
          lift $ logInfo "Deleting existing installation"
          liftE $ rmGHCVer installVer

      _ -> pure ()

    forM_ mBindist $ \bindist -> do
      liftE $ installPackedGHC bindist
                               (Just $ RegexDir "ghc-.*")
                               ghcdir
                               installVer
                               False       -- not a force install, since we already overwrite when compiling.
                               []

    case installDir of
      -- set and make symlinks for regular (non-isolated) installs
      GHCupInternal -> do
        reThrowAll GHCupSetError $ postGHCInstall installVer
        -- restore
        when alreadySet $ liftE $ void $ setGHC installVer SetGHCOnly Nothing

      _ -> pure ()

    pure installVer

 where
  getGHCVer :: ( MonadReader env m
               , HasSettings env
               , HasDirs env
               , HasLog env
               , MonadIO m
               , MonadThrow m
               )
            => GHCupPath
            -> Excepts '[ProcessError, ParseError] m Version
  getGHCVer tmpUnpack = do
    lEM $ execLogged "python3" ["./boot"] (Just $ fromGHCupPath tmpUnpack) "ghc-bootstrap" Nothing
    lEM $ configureWithGhcBoot Nothing [] (Just $ fromGHCupPath tmpUnpack) "ghc-bootstrap"
    let versionFile = fromGHCupPath tmpUnpack </> "VERSION"
    hasVersionFile <- liftIO $ doesFileExist versionFile
    if hasVersionFile
    then do
      lift $ logDebug "Detected VERSION file, trying to extract"
      contents <- liftIO $ readFile versionFile
      either (throwE . ParseError . show) pure . MP.parse version' "" . T.pack . stripNewlineEnd $ contents
    else do
      lift $ logDebug "Didn't detect VERSION file, trying to extract via legacy 'make'"
      CapturedProcess {..} <- lift $ makeOut
        ["show!", "--quiet", "VALUE=ProjectVersion" ] (Just $ fromGHCupPath tmpUnpack)
      case _exitCode of
        ExitSuccess -> either (throwE . ParseError . show) pure . MP.parse ghcProjectVersion "" . T.pack . stripNewlineEnd . T.unpack . decUTF8Safe' $ _stdOut
        ExitFailure c -> throwE $ NonZeroExit c "make" ["show!", "--quiet", "VALUE=ProjectVersion" ]

  defaultConf =
    let cross_mk = $(LitE . StringL <$> (qAddDependentFile "data/build_mk/cross" >> runIO (readFile "data/build_mk/cross")))
        default_mk = $(LitE . StringL <$> (qAddDependentFile "data/build_mk/default" >> runIO (readFile "data/build_mk/default")))
    in case crossTarget of
         Just _ -> cross_mk
         _      -> default_mk

  compileHadrianBindist :: ( MonadReader env m
                           , HasDirs env
                           , HasSettings env
                           , HasPlatformReq env
                           , MonadThrow m
                           , MonadCatch m
                           , HasLog env
                           , MonadIO m
                           , MonadFail m
                           )
                        => GHCTargetVersion
                        -> FilePath
                        -> InstallDirResolved
                        -> Excepts
                             '[ FileDoesNotExistError
                              , HadrianNotFound
                              , InvalidBuildConfig
                              , PatchFailed
                              , ProcessError
                              , NotFoundInPATH
                              , CopyError]
                             m
                             (Maybe FilePath)  -- ^ output path of bindist, None for cross
  compileHadrianBindist tver workdir ghcdir = do
    liftE $ configureBindist tver workdir ghcdir

    lift $ logInfo "Building (this may take a while)..."
    hadrian_build <- liftE $ findHadrianFile workdir
    lEM $ execLogged hadrian_build
                          ( maybe [] (\j  -> ["-j" <> show j]         ) jobs
                         ++ maybe [] (\bf -> ["--flavour=" <> bf]) buildFlavour
                         ++ ["binary-dist"]
                          )
                          (Just workdir) "ghc-make"
                          Nothing
    [tar] <- liftIO $ findFiles
      (workdir </> "_build" </> "bindist")
      (makeRegexOpts compExtended
                     execBlank
                     ([s|^ghc-.*\.tar\..*$|] :: ByteString)
      )
    liftE $ fmap Just $ copyBindist tver tar (workdir </> "_build" </> "bindist")

  findHadrianFile :: (MonadIO m)
                  => FilePath
                  -> Excepts
                       '[HadrianNotFound]
                       m
                       FilePath
  findHadrianFile workdir = do
    let possible_files = if isWindows
                         then ((workdir </> "hadrian") </>) <$> ["build.bat"]
                         else ((workdir </> "hadrian") </>) <$> ["build", "build.sh"]
    exsists <- forM possible_files (\f -> liftIO (doesFileExist f) <&> (,f))
    case filter fst exsists of
      [] -> throwE HadrianNotFound
      ((_, x):_) -> pure x

  compileMakeBindist :: ( MonadReader env m
                        , HasDirs env
                        , HasSettings env
                        , HasPlatformReq env
                        , MonadThrow m
                        , MonadCatch m
                        , HasLog env
                        , MonadIO m
                        , MonadFail m
                        , MonadMask m
                        , MonadUnliftIO m
                        , MonadResource m
                        )
                     => GHCTargetVersion
                     -> FilePath
                     -> InstallDirResolved
                     -> Excepts
                          '[ FileDoesNotExistError
                           , HadrianNotFound
                           , InvalidBuildConfig
                           , PatchFailed
                           , ProcessError
                           , NotFoundInPATH
                           , MergeFileTreeError
                           , CopyError]
                          m
                       (Maybe FilePath)  -- ^ output path of bindist, None for cross
  compileMakeBindist tver workdir ghcdir = do
    liftE $ configureBindist tver workdir ghcdir

    case mbuildConfig of
      Just bc -> liftIOException
        doesNotExistErrorType
        (FileDoesNotExistError bc)
        (liftIO $ copyFile bc (build_mk workdir) False)
      Nothing ->
        liftIO $ T.writeFile (build_mk workdir) (addBuildFlavourToConf defaultConf)

    liftE $ checkBuildConfig (build_mk workdir)

    lift $ logInfo "Building (this may take a while)..."
    lEM $ make (maybe [] (\j -> ["-j" <> fS (show j)]) jobs) (Just workdir)

    if | isCross tver -> do
          lift $ logInfo "Installing cross toolchain..."
          tmpInstallDest <- lift withGHCupTmpDir
          lEM $ make ["DESTDIR=" <> fromGHCupPath tmpInstallDest, "install"] (Just workdir)
          liftE $ mergeGHCFileTree (tmpInstallDest `appendGHCupPath` dropDrive (fromInstallDir ghcdir)) ghcdir tver True
          pure Nothing
       | otherwise -> do
          lift $ logInfo "Creating bindist..."
          lEM $ make ["binary-dist"] (Just workdir)
          [tar] <- liftIO $ findFiles
            workdir
            (makeRegexOpts compExtended
                           execBlank
                           ([s|^ghc-.*\.tar\..*$|] :: ByteString)
            )
          liftE $ fmap Just $ copyBindist tver tar workdir

  build_mk workdir = workdir </> "mk" </> "build.mk"

  copyBindist :: ( MonadReader env m
                 , HasDirs env
                 , HasSettings env
                 , HasPlatformReq env
                 , MonadIO m
                 , MonadThrow m
                 , MonadCatch m
                 , HasLog env
                 )
              => GHCTargetVersion
              -> FilePath           -- ^ tar file
              -> FilePath           -- ^ workdir
              -> Excepts
                   '[CopyError]
                   m
                   FilePath
  copyBindist tver tar workdir = do
    Dirs {..} <- lift getDirs
    pfreq <- lift getPlatformReq
    c       <- liftIO $ BL.readFile (workdir </> tar)
    cDigest <-
      fmap (T.take 8)
      . lift
      . throwEither
      . E.decodeUtf8'
      . B16.encode
      . SHA256.hashlazy
      $ c
    cTime <- liftIO getCurrentTime
    let tarName = makeValid ("ghc-"
                            <> T.unpack (tVerToText tver)
                            <> "-"
                            <> pfReqToString pfreq
                            <> "-"
                            <> iso8601Show cTime
                            <> "-"
                            <> T.unpack cDigest
                            <> ".tar"
                            <> takeExtension tar)
    let tarPath = fromGHCupPath cacheDir </> tarName
    copyFileE (workdir </> tar) tarPath False
    lift $ logInfo $ "Copied bindist to " <> T.pack tarPath
    pure tarPath

  checkBuildConfig :: (MonadReader env m, MonadCatch m, MonadIO m, HasLog env)
                   => FilePath
                   -> Excepts
                        '[FileDoesNotExistError, InvalidBuildConfig]
                        m
                        ()
  checkBuildConfig bc = do
    c <- liftIOException
           doesNotExistErrorType
           (FileDoesNotExistError bc)
           (liftIO $ B.readFile bc)
    let lines' = fmap T.strip . T.lines $ decUTF8Safe c

   -- for cross, we need Stage1Only
    case crossTarget of
      Just _ -> when ("Stage1Only = YES" `notElem` lines') $ throwE
        (InvalidBuildConfig
          [s|Cross compiling needs to be a Stage1 build, add "Stage1Only = YES" to your config!|]
        )
      _ -> pure ()

    forM_ buildFlavour $ \bf ->
      when (T.pack ("BuildFlavour = " <> bf) `notElem` lines') $ do
        lift $ logWarn $ "Customly specified build config overwrites --flavour=" <> T.pack bf <> " switch! Waiting 5 seconds..."
        liftIO $ threadDelay 5000000

  addBuildFlavourToConf bc = case buildFlavour of
    Just bf -> "BuildFlavour = " <> T.pack bf <> "\n" <> bc
    Nothing -> bc

  isCross :: GHCTargetVersion -> Bool
  isCross = isJust . _tvTarget


  configureBindist :: ( MonadReader env m
                      , HasDirs env
                      , HasSettings env
                      , HasPlatformReq env
                      , MonadThrow m
                      , MonadCatch m
                      , HasLog env
                      , MonadIO m
                      , MonadFail m
                      )
                   => GHCTargetVersion
                   -> FilePath
                   -> InstallDirResolved
                   -> Excepts
                        '[ FileDoesNotExistError
                         , InvalidBuildConfig
                         , PatchFailed
                         , ProcessError
                         , NotFoundInPATH
                         , CopyError
                         ]
                        m
                        ()
  configureBindist tver workdir (fromInstallDir -> ghcdir) = do
    lift $ logInfo [s|configuring build|]
    lEM $ configureWithGhcBoot (Just tver)
      (maybe mempty
                (\x -> ["--target=" <> T.unpack x])
                (_tvTarget tver)
      ++ ["--prefix=" <> ghcdir]
      ++ (if isWindows then ["--enable-tarballs-autodownload"] else [])
      ++ fmap T.unpack aargs
      )
      (Just workdir)
      "ghc-conf"
    pure ()

  configureWithGhcBoot :: ( MonadReader env m
                          , HasSettings env
                          , HasDirs env
                          , HasLog env
                          , MonadIO m
                          , MonadThrow m)
                       => Maybe GHCTargetVersion
                       -> [String]         -- ^ args for configure
                       -> Maybe FilePath   -- ^ optionally chdir into this
                       -> FilePath         -- ^ log filename (opened in append mode)
                       -> m (Either ProcessError ())
  configureWithGhcBoot mtver args dir logf = do
    let execNew = execLogged
                    "sh"
                    ("./configure" : ("GHC=" <> bghc) : args)
                    dir
                    logf
                    Nothing
        execOld = execLogged
                   "sh"
                   ("./configure" : ("--with-ghc=" <> bghc) : args)
                   dir
                   logf
                   Nothing
    if | Just tver <- mtver
       , _tvVersion tver >= [vver|8.8.0|] -> execNew
       | Nothing   <- mtver               -> execNew -- need some default for git checkouts where we don't know yet
       | otherwise                        -> execOld

  bghc = case bstrap of
           Right g    -> g
           Left  bver -> ("ghc-" <> (T.unpack . prettyVer $ bver) <> exeExt)




    -------------
    --[ Other ]--
    -------------



-- | Creates @ghc-x.y.z@ and @ghc-x.y@ symlinks. This is used for
-- both installing from source and bindist.
postGHCInstall :: ( MonadReader env m
                  , HasDirs env
                  , HasLog env
                  , MonadThrow m
                  , MonadFail m
                  , MonadIO m
                  , MonadCatch m
                  , MonadMask m
                  , MonadUnliftIO m
                  )
               => GHCTargetVersion
               -> Excepts '[NotInstalled] m ()
postGHCInstall ver@GHCTargetVersion {..} = do
  void $ liftE $ setGHC ver SetGHC_XYZ Nothing

  -- Create ghc-x.y symlinks. This may not be the current
  -- version, create it regardless.
  v' <-
    handle (\(e :: ParseError) -> lift $ logWarn (T.pack $ displayException e) >> pure Nothing)
    $ fmap Just
    $ getMajorMinorV _tvVersion
  forM_ v' $ \(mj, mi) -> lift (getGHCForPVP (PVP (fromIntegral mj :| [fromIntegral mi])) _tvTarget)
    >>= mapM_ (\v -> liftE $ setGHC v SetGHC_XY Nothing)

