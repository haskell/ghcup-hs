{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Command.Test.GHC
Description : GHCup installation functions for GHC
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Command.Test.GHC where


import GHCup.Builder
import GHCup.Command.Install.LowLevel
import GHCup.Download
import GHCup.Errors
import GHCup.Prelude
import GHCup.Query.GHCupDirs
import GHCup.Types
import GHCup.Types.Optics
import GHCup.Unpack

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Resource hiding ( throwM )
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions                hiding ( patch )
import Optics
import Prelude                      hiding ( abs, writeFile )
import System.Environment
import System.FilePath
import URI.ByteString

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T


data GHCVer
  = SourceDist Version
  | GitDist GitBranch
  | RemoteDist URI
  deriving (Eq, Show)



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
                 , URIParseError
                 ]
                m
                ()
testGHCVer ver addMakeArgs = do
  GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo

  dlInfo <-
    preview (ix ghc % toolVersions % ix ver % viTestDL % _Just) dls
      ?? NoDownload ver ghc Nothing

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
                     , URIParseError
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
                   , HasLog env
                   , MonadIOish m
                   )
                => GHCupPath         -- ^ Path to the unpacked GHC bindist (where the make file resides)
                -> GHCTargetVersion  -- ^ The GHC version
                -> [T.Text]          -- ^ additional configure args for bindist
                -> Excepts '[ProcessError] m ()
testUnpackedGHC path tver addMakeArgs = do
  lift $ logInfo $ "Testing GHC version " <> tVerToText tver <> "!"
  ghcDir <- lift $ ghcupGHCDir tver
  let ghcBinDir = fromGHCupPath ghcDir </> "bin"
  env <- liftIO $ addToPath [ghcBinDir] False
  let pathVar = if isWindows then "Path" else "PATH"
  forM_ (Map.lookup pathVar . Map.fromList $ env) $ liftIO . setEnv pathVar

  liftE $ makeWithWrapper (fmap T.unpack addMakeArgs)
              (Just $ fromGHCupPath path)
              "ghc-test"
              (Just $ ("STAGE1_GHC", maybe "" (T.unpack . (<> "-")) (_tvTarget tver)
                                     <> "ghc-"
                                     <> T.unpack (prettyVer $ _tvVersion tver)) : env)
  pure ()
