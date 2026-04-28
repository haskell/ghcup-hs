{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Setup where

import GHCup.Download
import GHCup.Errors
import GHCup.Hardcoded.URLs
import GHCup.Prelude
import GHCup.Query.GHCupDirs
import GHCup.Types
import GHCup.Types.Optics

import Control.Monad        ( void )
import Control.Monad.Reader ( MonadReader )
import Control.Monad.Trans  ( lift )
import Data.Variant.Excepts
import System.FilePath      ( (</>) )
import System.IO.Error      ( doesNotExistErrorType )
import URI.ByteString       ( serializeURIRef' )

import qualified Data.Text as T

ensureShimGen :: ( HasLog env
                 , MonadReader env m
                 , HasDirs env
                 , HasSettings env
                 , HasGHCupInfo env
                 , MonadIOish m
                 )
              => Excepts '[URIParseError, GPGError, DigestError, ContentLengthError, DownloadFailed, NoDownload] m ()
ensureShimGen
  | isWindows = do
      dirs <- lift getDirs
      let shimDownload = DownloadInfo { _dlUri = decUTF8Safe . serializeURIRef' $ shimGenURL, _dlSubdir = Nothing, _dlHash = shimGenSHA, _dlCSize = Nothing, _dlOutput = Nothing, _dlTag = Nothing, _dlInstallSpec = Nothing }
      let dl = downloadCached' shimDownload (Just "gs.exe") Nothing
      void $ (\DigestError{} -> do
          lift $ logWarn "Digest doesn't match, redownloading gs.exe..."
          lift $ logDebug ("rm -f " <> T.pack (fromGHCupPath (cacheDir dirs) </> "gs.exe"))
          lift $ hideError doesNotExistErrorType $ recycleFile (fromGHCupPath (cacheDir dirs) </> "gs.exe")
          liftE @'[URIParseError, GPGError, DigestError, ContentLengthError, DownloadFailed] $ dl
        ) `catchE` liftE @'[URIParseError, GPGError, DigestError, ContentLengthError, DownloadFailed] dl
  | otherwise = pure ()


-- | Ensure ghcup directory structure exists.
ensureDirectories :: Dirs -> IO ()
ensureDirectories (Dirs baseDir binDir cacheDir logsDir confDir trashDir dbDir tmpDir _) = do
  createDirRecursive' (fromGHCupPath baseDir)
  createDirRecursive' (fromGHCupPath baseDir </> "ghc")
  createDirRecursive' (fromGHCupPath baseDir </> "hls")
  createDirRecursive' binDir
  createDirRecursive' (fromGHCupPath cacheDir)
  createDirRecursive' (fromGHCupPath logsDir)
  createDirRecursive' (fromGHCupPath confDir)
  createDirRecursive' (fromGHCupPath trashDir)
  createDirRecursive' (fromGHCupPath dbDir)
  createDirRecursive' (fromGHCupPath tmpDir)
  pure ()
