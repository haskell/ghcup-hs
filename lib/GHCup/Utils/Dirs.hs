{-# LANGUAGE QuasiQuotes           #-}

module GHCup.Utils.Dirs where


import           GHCup.Types.JSON               ( )
import           GHCup.Utils.Prelude
import           GHCup.Utils.String.QQ

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Maybe
import           Data.Versions
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
import qualified System.Posix.FilePath         as FP
import qualified System.Posix.User             as PU



    -------------------------
    --[ GHCup directories ]--
    -------------------------


ghcupBaseDir :: IO (Path Abs)
ghcupBaseDir = do
  getEnv [s|GHCUP_INSTALL_BASE_PREFIX|] >>= \case
    Just r  -> parseAbs r
    Nothing -> do
      home <- liftIO getHomeDirectory
      pure (home </> ([rel|.ghcup|] :: Path Rel))

ghcupGHCBaseDir :: IO (Path Abs)
ghcupGHCBaseDir = ghcupBaseDir <&> (</> ([rel|ghc|] :: Path Rel))

ghcupGHCDir :: Version -> IO (Path Abs)
ghcupGHCDir ver = do
  ghcbasedir <- ghcupGHCBaseDir
  verdir     <- parseRel (verToBS ver)
  pure (ghcbasedir </> verdir)


ghcupBinDir :: IO (Path Abs)
ghcupBinDir = ghcupBaseDir <&> (</> ([rel|bin|] :: Path Rel))

ghcupCacheDir :: IO (Path Abs)
ghcupCacheDir = ghcupBaseDir <&> (</> ([rel|cache|] :: Path Rel))

ghcupLogsDir :: IO (Path Abs)
ghcupLogsDir = ghcupBaseDir <&> (</> ([rel|logs|] :: Path Rel))


mkGhcupTmpDir :: (MonadThrow m, MonadIO m) => m (Path Abs)
mkGhcupTmpDir = do
  tmpdir <- liftIO $ getEnvDefault [s|TMPDIR|] [s|/tmp|]
  tmp    <- liftIO $ mkdtemp $ (tmpdir FP.</> [s|ghcup-|])
  parseAbs tmp


withGHCupTmpDir :: (MonadResource m, MonadThrow m, MonadIO m) => m (Path Abs)
withGHCupTmpDir = snd <$> allocate mkGhcupTmpDir deleteDirRecursive


    --------------
    --[ Others ]--
    --------------


getHomeDirectory :: IO (Path Abs)
getHomeDirectory = do
  e <- getEnv [s|HOME|]
  case e of
    Just fp -> parseAbs fp
    Nothing -> do
      h <- PU.homeDirectory <$> (PU.getEffectiveUserID >>= PU.getUserEntryForID)
      parseAbs $ UTF8.fromString h -- this is a guess
