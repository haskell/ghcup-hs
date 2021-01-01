{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Validate where

import           GHCup
import           GHCup.Download
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils.Dirs
import           GHCup.Utils.Logger
import           GHCup.Utils.Version.QQ

import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Control.Monad.Trans.Resource   ( runResourceT
                                                , MonadUnliftIO
                                                )
import           Data.Containers.ListUtils      ( nubOrd )
import           Data.IORef
import           Data.List
import           Data.String.Interpolate
import           Data.Versions
import           Haskus.Utils.Variant.Excepts
import           Optics
import           System.Exit
import           System.IO
import           Text.ParserCombinators.ReadP
import           URI.ByteString

import qualified Data.ByteString               as B
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Version                  as V


data ValidationError = InternalError String
  deriving Show

instance Exception ValidationError


addError :: (MonadReader (IORef Int) m, MonadIO m, Monad m) => m ()
addError = do
  ref <- ask
  liftIO $ modifyIORef ref (+ 1)


validate :: (Monad m, MonadLogger m, MonadThrow m, MonadIO m, MonadUnliftIO m)
         => GHCupDownloads
         -> m ExitCode
validate dls = do
  ref <- liftIO $ newIORef 0

  -- verify binary downloads --
  flip runReaderT ref $ do
    -- unique tags
    forM_ (M.toList dls) $ \(t, _) -> checkUniqueTags t

    -- required platforms
    forM_ (M.toList dls) $ \(t, versions) ->
      forM_ (M.toList versions) $ \(v, vi) ->
        forM_ (M.toList $ _viArch vi) $ \(arch, pspecs) -> do
          checkHasRequiredPlatforms t v arch (M.keys pspecs)

    checkGHCVerIsValid
    forM_ (M.toList dls) $ \(t, _) -> checkMandatoryTags t
    _ <- checkGHCHasBaseVersion

    -- exit
    e <- liftIO $ readIORef ref
    if e > 0
      then pure $ ExitFailure e
      else do
        lift $ $(logInfo) [i|All good|]
        pure ExitSuccess
 where
  checkHasRequiredPlatforms t v arch pspecs = do
    let v' = prettyVer v
    when (not $ any (== Linux UnknownLinux) pspecs) $ do
      lift $ $(logError)
        [i|Linux UnknownLinux missing for for #{t} #{v'} #{arch}|]
      addError
    when ((not $ any (== Darwin) pspecs) && arch == A_64) $ do
      lift $ $(logError) [i|Darwin missing for #{t} #{v'} #{arch}|]
      addError
    when ((not $ any (== FreeBSD) pspecs) && arch == A_64) $ lift $ $(logWarn)
      [i|FreeBSD missing for #{t} #{v'} #{arch}|]

    -- alpine needs to be set explicitly, because
    -- we cannot assume that "Linux UnknownLinux" runs on Alpine
    -- (although it could be static)
    when (not $ any (== Linux Alpine) pspecs) $
      case t of
        GHCup -> (lift $ $(logError) [i|Linux Alpine missing for #{t} #{v'} #{arch}|]) >> addError
        Cabal | v > [vver|2.4.1.0|] -> (lift $ $(logError) [i|Linux Alpine missing for #{t} #{v'} #{arch}|]) >> addError
        _     -> lift $ $(logWarn) [i|Linux Alpine missing for #{t} #{v'} #{arch}|]

  checkUniqueTags tool = do
    let allTags = join $ M.elems $ availableToolVersions dls tool
    let nonUnique =
          fmap fst
            .   filter (\(_, b) -> not b)
            <$> ( mapM
                    (\case
                      [] -> throwM $ InternalError "empty inner list"
                      (t : ts) ->
                        pure $ (t, ) $ if isUniqueTag t then ts == [] else True
                    )
                . group
                . sort
                $ allTags
                )
    case join nonUnique of
      [] -> pure ()
      xs -> do
        lift $ $(logError) [i|Tags not unique for #{tool}: #{xs}|]
        addError
   where
    isUniqueTag Latest         = True
    isUniqueTag Recommended    = True
    isUniqueTag Old            = False
    isUniqueTag Prerelease     = False
    isUniqueTag (Base       _) = False
    isUniqueTag (UnknownTag _) = False

  checkGHCVerIsValid = do
    let ghcVers = toListOf (ix GHC % to M.keys % folded) dls
    forM_ ghcVers $ \v ->
      case [ x | (x,"") <- readP_to_S V.parseVersion (T.unpack . prettyVer $ v) ] of
        [_] -> pure ()
        _   -> do
          lift $ $(logError) [i|GHC version #{v} is not valid |]
          addError

  -- a tool must have at least one of each mandatory tags
  checkMandatoryTags tool = do
    let allTags = join $ M.elems $ availableToolVersions dls tool
    forM_ [Latest, Recommended] $ \t -> case elem t allTags of
      False -> do
        lift $ $(logError) [i|Tag #{t} missing from #{tool}|]
        addError
      True -> pure ()

  -- all GHC versions must have a base tag
  checkGHCHasBaseVersion = do
    let allTags = M.toList $ availableToolVersions dls GHC
    forM allTags $ \(ver, tags) -> case any isBase tags of
      False -> do
        lift $ $(logError) [i|Base tag missing from GHC ver #{ver}|]
        addError
      True -> pure ()

  isBase (Base _) = True
  isBase _        = False

validateTarballs :: ( Monad m
                    , MonadLogger m
                    , MonadThrow m
                    , MonadIO m
                    , MonadUnliftIO m
                    , MonadMask m
                    )
                 => Maybe T.Text
                 -> GHCupDownloads
                 -> m ExitCode
validateTarballs urlSubstr dls = do
  ref <- liftIO $ newIORef 0

  flip runReaderT ref $ do
     -- download/verify all tarballs
    let dlis = nubOrd . filter matchingUrl $
          dls ^.. each % each % (viSourceDL % _Just `summing` viArch % each % each % each)
        matchingUrl dli = case urlSubstr of
          Nothing -> True
          Just sub -> E.encodeUtf8 sub `B.isInfixOf` serializeURIRef' (_dlUri dli)
    forM_ dlis $ downloadAll

    -- exit
    e <- liftIO $ readIORef ref
    if e > 0
      then pure $ ExitFailure e
      else do
        lift $ $(logInfo) [i|All good|]
        pure ExitSuccess

 where
  downloadAll dli = do
    dirs <- liftIO getDirs
    let settings = AppState (Settings True False Never Curl False GHCupURL) dirs defaultKeyBindings
    let runLogger = myLoggerT LoggerConfig { lcPrintDebug = True
                                           , colorOutter  = B.hPut stderr
                                           , rawOutter    = (\_ -> pure ())
                                           }

    r <-
      runLogger
      . flip runReaderT settings
      . runResourceT
      . runE
      $ downloadCached dli Nothing
    case r of
      VRight _ -> pure ()
      VLeft  e -> do
        lift $ $(logError)
          [i|Could not download (or verify hash) of #{dli}, Error was: #{e}|]
        addError
