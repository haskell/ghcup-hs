{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ViewPatterns          #-}

{-|
Module      : GHCup.Utils.Dirs
Description : Definition of GHCup directories
Copyright   : (c) Julian Ospald, 2020
License     : GPL-3
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX
-}
module GHCup.Utils.Dirs where


import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Utils.MegaParsec
import           GHCup.Utils.Prelude

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Maybe
import           HPath
import           HPath.IO
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )
import           System.Posix.Env.ByteString    ( getEnv
                                                , getEnvDefault
                                                )
import           System.Posix.Temp.ByteString   ( mkdtemp )

import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Text.Encoding            as E
import qualified System.Posix.FilePath         as FP
import qualified System.Posix.User             as PU
import qualified Text.Megaparsec               as MP



    -------------------------
    --[ GHCup directories ]--
    -------------------------


-- | ~/.ghcup by default
ghcupBaseDir :: IO (Path Abs)
ghcupBaseDir = do
  bdir <- getEnv "GHCUP_INSTALL_BASE_PREFIX" >>= \case
    Just r  -> parseAbs r
    Nothing -> liftIO getHomeDirectory
  pure (bdir </> [rel|.ghcup|])


-- | ~/.ghcup/ghc by default.
ghcupGHCBaseDir :: IO (Path Abs)
ghcupGHCBaseDir = ghcupBaseDir <&> (</> [rel|ghc|])


-- | Gets '~/.ghcup/ghc/<ghcupGHCDir>'.
-- The dir may be of the form
--   * armv7-unknown-linux-gnueabihf-8.8.3
--   * 8.8.4
ghcupGHCDir :: GHCTargetVersion -> IO (Path Abs)
ghcupGHCDir ver = do
  ghcbasedir <- ghcupGHCBaseDir
  verdir     <- parseRel $ E.encodeUtf8 (prettyTVer ver)
  pure (ghcbasedir </> verdir)


-- | See 'ghcupToolParser'.
parseGHCupGHCDir :: MonadThrow m => Path Rel -> m GHCTargetVersion
parseGHCupGHCDir (toFilePath -> f) = do
  fp <- throwEither $ E.decodeUtf8' f
  throwEither $ MP.parse ghcTargetVerP "" fp


ghcupBinDir :: IO (Path Abs)
ghcupBinDir = ghcupBaseDir <&> (</> [rel|bin|])

ghcupCacheDir :: IO (Path Abs)
ghcupCacheDir = ghcupBaseDir <&> (</> [rel|cache|])

ghcupLogsDir :: IO (Path Abs)
ghcupLogsDir = ghcupBaseDir <&> (</> [rel|logs|])


mkGhcupTmpDir :: (MonadThrow m, MonadIO m) => m (Path Abs)
mkGhcupTmpDir = do
  tmpdir <- liftIO $ getEnvDefault "TMPDIR" "/tmp"
  tmp    <- liftIO $ mkdtemp $ (tmpdir FP.</> "ghcup-")
  parseAbs tmp


withGHCupTmpDir :: (MonadResource m, MonadThrow m, MonadIO m) => m (Path Abs)
withGHCupTmpDir = snd <$> allocate mkGhcupTmpDir deleteDirRecursive


    --------------
    --[ Others ]--
    --------------


getHomeDirectory :: IO (Path Abs)
getHomeDirectory = do
  e <- getEnv "HOME"
  case e of
    Just fp -> parseAbs fp
    Nothing -> do
      h <- PU.homeDirectory <$> (PU.getEffectiveUserID >>= PU.getUserEntryForID)
      parseAbs $ UTF8.fromString h -- this is a guess
