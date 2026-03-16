{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GHCup.System.Cmd where

import GHCup.Download
import GHCup.Errors
import GHCup.Prelude
import GHCup.Prelude.Process
import GHCup.Prelude.String.QQ
import GHCup.Query.GHCupDirs
import GHCup.Types
import GHCup.Types.Optics

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource ( MonadResource )
import Data.ByteString              ( ByteString )
import Data.Char                    ( isHexDigit )
import Data.List                    ( isPrefixOf, sort, stripPrefix )
import Data.Maybe                   ( catMaybes, isJust )
import Data.Variant.Excepts
import GHC.IO.Exception             ( ExitCode (ExitFailure, ExitSuccess) )
import Safe                         ( atMay )
import System.FilePath              ( getSearchPath, (</>) )
import System.IO.Error              ( isDoesNotExistError, isPermissionError )
import Text.Regex.Posix
    ( RegexMaker (makeRegexOpts), compExtended, execBlank )
import URI.ByteString               ( URI )

import qualified Data.Text as T


-- | Calls gmake if it exists in PATH, otherwise make.
make :: ( MonadIOish m
        , MonadReader env m
        , HasDirs env
        , HasLog env
        , HasSettings env
        )
     => [String]
     -> Maybe FilePath
     -> m (Either ProcessError ())
make args workdir = make' args workdir "ghc-make" Nothing


-- | Calls gmake if it exists in PATH, otherwise make.
make' :: ( MonadThrow m
         , MonadIO m
         , MonadReader env m
         , HasDirs env
         , HasLog env
         , HasSettings env
         )
      => [String]
      -> Maybe FilePath
      -> FilePath         -- ^ log filename (opened in append mode)
      -> Maybe [(String, String)] -- ^ optional environment
      -> m (Either ProcessError ())
make' args workdir logfile menv = do
  mymake <- liftIO getBestMake
  execLogged mymake args workdir logfile menv

getBestMake :: IO FilePath
getBestMake = do
  spaths    <- liftIO getSearchPath
  has_gmake <- isJust <$> liftIO (searchPath spaths "gmake")
  let mymake = if has_gmake then "gmake" else "make"
  pure mymake


makeOut :: (MonadReader env m, HasDirs env, MonadIO m)
        => [String]
        -> Maybe FilePath
        -> m CapturedProcess
makeOut args workdir = do
  spaths    <- liftIO getSearchPath
  has_gmake <- isJust <$> liftIO (searchPath spaths "gmake")
  let mymake = if has_gmake then "gmake" else "make"
  executeOut mymake args workdir


-- | Try to apply patches in order. The order is determined by
-- a quilt series file (in the patch directory) if one exists,
-- else the patches are applied in lexicographical order.
-- Fails with 'PatchFailed' on first failure.
applyPatches :: (MonadReader env m, HasDirs env, HasLog env, MonadIO m)
             => FilePath   -- ^ dir containing patches
             -> FilePath   -- ^ dir to apply patches in
             -> Excepts '[PatchFailed] m ()
applyPatches pdir ddir = do
  let lexicographical = (fmap . fmap) (pdir </>) $ sort <$> findFiles
        pdir
        (makeRegexOpts compExtended
                       execBlank
                       ([s|.+\.(patch|diff)$|] :: ByteString)
        )
  let quilt = map (pdir </>) . lines <$> readFile (pdir </> "series")

  patches <- liftIO $ quilt `catchIO` (\e ->
    if isDoesNotExistError e || isPermissionError e then
      lexicographical
    else throwIO e)
  forM_ patches $ \patch' -> applyPatch patch' ddir


applyPatch :: (MonadReader env m, HasDirs env, HasLog env, MonadIO m)
           => FilePath   -- ^ Patch
           -> FilePath   -- ^ dir to apply patches in
           -> Excepts '[PatchFailed] m ()
applyPatch patch ddir = do
  lift $ logInfo $ "Applying patch " <> T.pack patch
  fmap (either (const Nothing) Just)
       (exec
         "patch"
         ["-p1", "-s", "-f", "-i", patch]
         (Just ddir)
         Nothing)
    !? PatchFailed


applyAnyPatch :: ( MonadReader env m
                 , HasDirs env
                 , HasLog env
                 , HasSettings env
                 , MonadResource m
                 , MonadIOish m
                 )
              => Maybe (Either FilePath [URI])
              -> FilePath
              -> Excepts '[PatchFailed, DownloadFailed, DigestError, ContentLengthError, GPGError] m ()
applyAnyPatch Nothing _                   = pure ()
applyAnyPatch (Just (Left pdir)) workdir  = liftE $ applyPatches pdir workdir
applyAnyPatch (Just (Right uris)) workdir = do
  tmpUnpack <- fromGHCupPath <$> lift withGHCupTmpDir
  forM_ uris $ \uri -> do
    patch <- liftE $ download uri Nothing Nothing Nothing tmpUnpack Nothing False
    liftE $ applyPatch patch workdir


-- | https://gitlab.haskell.org/ghc/ghc/-/issues/17353
darwinNotarization :: (MonadReader env m, HasDirs env, MonadIO m)
                   => Platform
                   -> FilePath
                   -> m (Either ProcessError ())
darwinNotarization Darwin path = exec
  "/usr/bin/xattr"
  ["-r", "-d", "com.apple.quarantine", path]
  Nothing
  Nothing
darwinNotarization _ _ = pure $ Right ()

gitOut :: (MonadReader env m, HasLog env, MonadIO m) => [String] -> FilePath -> Excepts '[ProcessError] m T.Text
gitOut args dir = do
  CapturedProcess {..} <- lift $ executeOut "git" args (Just dir)
  case _exitCode of
    ExitSuccess   -> pure $ T.pack $ stripNewlineEnd $ T.unpack $ decUTF8Safe' _stdOut
    ExitFailure c -> do
      let pe = NonZeroExit c "git" args
      lift $ logDebug $ T.pack (prettyHFError pe)
      throwE pe

processBranches :: T.Text -> [String]
processBranches str' = let lines'   = lines (T.unpack str')
                           words'   = fmap words lines'
                           refs     = catMaybes $ fmap (`atMay` 1) words'
                           branches = catMaybes $ fmap (stripPrefix "refs/heads/") $ filter (isPrefixOf "refs/heads/") refs
                       in branches

isCommitHash :: String -> Bool
isCommitHash str' = let hex = all isHexDigit str'
                        len = length str'
                    in hex && len == 40
