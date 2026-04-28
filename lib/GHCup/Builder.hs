{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module GHCup.Builder where

import GHCup.Errors
import GHCup.Prelude
import GHCup.Query.GHCupDirs
import GHCup.Types
import GHCup.Types.Optics

import Control.Exception.Safe
    ( Exception (displayException), handleIO, onException )
import Control.Monad
import Control.Monad.IO.Unlift ( MonadUnliftIO (withRunInIO) )
import Control.Monad.Reader    ( MonadIO (..), MonadReader, MonadTrans (lift) )
import Data.Foldable           ( foldlM )
import Data.List               ( sort )
import Data.Variant.Excepts    ( Excepts, onE_, throwE )
import System.FilePath         ( pathSeparators )
import System.IO.Error         ( doesNotExistErrorType )
import Text.Regex.Posix
    ( RegexMaker (makeRegexOpts), compIgnoreCase, execBlank )

import qualified Data.Text              as T
import           GHCup.System.Directory


-- | Execute a build action while potentially cleaning up:
--
--   1. the build directory, depending on the KeepDirs setting
runBuildAction :: ( MonadReader env m
                  , HasDirs env
                  , HasSettings env
                  , MonadIOish m
                  , HasLog env
                  )
               => GHCupPath        -- ^ build directory (cleaned up depending on Settings)
               -> Excepts e m a
               -> Excepts e m a
runBuildAction bdir action = do
  Settings {..} <- lift getSettings
  let exAction = do
        when (keepDirs == Never)
          $ rmBDir bdir
  v <-
    flip onException (lift exAction)
    $ onE_ exAction action
  when (keepDirs == Never || keepDirs == Errors) $ lift $ rmBDir bdir
  pure v

-- | Clean up the given directory if the action fails,
-- depending on the Settings.
cleanUpOnError :: forall e m a env .
                  ( MonadReader env m
                  , HasDirs env
                  , HasSettings env
                  , MonadIOish m
                  , HasLog env
                  )
               => GHCupPath        -- ^ build directory (cleaned up depending on Settings)
               -> Excepts e m a
               -> Excepts e m a
cleanUpOnError bdir action = do
  Settings {..} <- lift getSettings
  let exAction = when (keepDirs == Never) $ rmBDir bdir
  flip onException (lift exAction) $ onE_ exAction action


-- | Remove a build directory, ignoring if it doesn't exist and gracefully
-- printing other errors without crashing.
rmBDir :: (MonadReader env m, HasLog env, MonadIOish m) => GHCupPath -> m ()
rmBDir dir = withRunInIO (\run -> run $
           liftIO $ handleIO (\e -> run $ logWarn $
               "Couldn't remove build dir " <> T.pack (fromGHCupPath dir) <> ", error was: " <> T.pack (displayException e))
           $ hideError doesNotExistErrorType
           $ rmPathForcibly dir)




intoSubdir :: (MonadReader env m, HasLog env, MonadIOish m)
           => GHCupPath       -- ^ unpacked tar dir
           -> TarDir         -- ^ how to descend
           -> Excepts '[TarDirDoesNotExist] m GHCupPath
intoSubdir bdir tardir = case tardir of
  RealDir pr -> do
    whenM (fmap not . liftIO . doesDirectoryExist $ fromGHCupPath (bdir `appendGHCupPath` pr))
          (throwE $ TarDirDoesNotExist tardir)
    pure (bdir `appendGHCupPath` pr)
  RegexDir r -> do
    let rs = split (`elem` pathSeparators) r
    foldlM
      (\y x ->
        (handleIO (\_ -> pure []) . liftIO . findFiles (fromGHCupPath y) . regex $ x) >>= (\case
          []      -> throwE $ TarDirDoesNotExist tardir
          (p : _) -> pure (y `appendGHCupPath` p)) . sort
      )
      bdir
      rs
    where regex = makeRegexOpts compIgnoreCase execBlank

