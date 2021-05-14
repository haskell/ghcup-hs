{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Validate where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Utils.Logger
import           GHCup.Utils.Version.QQ

#if defined(TAR)
import qualified Codec.Archive.Tar             as Tar
#else
import           Codec.Archive
#endif
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
import           System.FilePath
import           System.Exit
import           System.IO
import           Text.ParserCombinators.ReadP
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           Text.Regex.Posix

import qualified Data.ByteString               as B
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
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
          checkHasRequiredPlatforms t v (_viTags vi) arch (M.keys pspecs)

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
  checkHasRequiredPlatforms t v tags arch pspecs = do
    let v' = prettyVer v
        arch' = prettyShow arch
    when (notElem (Linux UnknownLinux) pspecs) $ do
      lift $ $(logError)
        [i|Linux UnknownLinux missing for for #{t} #{v'} #{arch'}|]
      addError
    when ((notElem Darwin pspecs) && arch == A_64) $ do
      lift $ $(logError) [i|Darwin missing for #{t} #{v'} #{arch'}|]
      addError
    when ((notElem FreeBSD pspecs) && arch == A_64) $ lift $ $(logWarn)
      [i|FreeBSD missing for #{t} #{v'} #{arch'}|]
    when (notElem Windows pspecs && arch == A_64) $ do
      lift $ $(logError)
        [i|Windows missing for for #{t} #{v'} #{arch'}|]
      addError

    -- alpine needs to be set explicitly, because
    -- we cannot assume that "Linux UnknownLinux" runs on Alpine
    -- (although it could be static)
    when (notElem (Linux Alpine) pspecs) $
      case t of
        GHCup | arch `elem` [A_64, A_32] -> lift ($(logError) [i|Linux Alpine missing for #{t} #{v'} #{arch}|]) >> addError
        Cabal | v > [vver|2.4.1.0|]
              , arch `elem` [A_64, A_32] -> lift ($(logError) [i|Linux Alpine missing for #{t} #{v'} #{arch'}|]) >> addError
        GHC | Latest `elem` tags || Recommended `elem` tags
            , arch `elem` [A_64, A_32] -> lift ($(logError) [i|Linux Alpine missing for #{t} #{v'} #{arch'}|])
        _ -> lift $ $(logWarn) [i|Linux Alpine missing for #{t} #{v'} #{arch'}|]

  checkUniqueTags tool = do
    let allTags = join $ M.elems $ availableToolVersions dls tool
    let nonUnique =
          fmap fst
            .   filter (\(_, b) -> not b)
            <$> ( mapM
                    (\case
                      [] -> throwM $ InternalError "empty inner list"
                      (t : ts) ->
                        pure $ (t, ) (not (isUniqueTag t) || null ts)
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

data TarballFilter = TarballFilter
  { tfTool    :: Maybe Tool
  , tfVersion :: Regex
  }

validateTarballs :: ( Monad m
                    , MonadLogger m
                    , MonadThrow m
                    , MonadIO m
                    , MonadUnliftIO m
                    , MonadMask m
                    )
                 => TarballFilter
                 -> GHCupDownloads
                 -> m ExitCode
validateTarballs (TarballFilter tool versionRegex) dls = do
  ref <- liftIO $ newIORef 0

  flip runReaderT ref $ do
     -- download/verify all tarballs
    let dlis = nubOrd $ dls ^.. each
          %& indices (maybe (const True) (==) tool) %> each
          %& indices (matchTest versionRegex . T.unpack . prettyVer)
          % (viSourceDL % _Just `summing` viArch % each % each % each)
    when (null dlis) $ $(logError) [i|no tarballs selected by filter|] *> addError

    forM_ dlis downloadAll

    -- exit
    e <- liftIO $ readIORef ref
    if e > 0
      then pure $ ExitFailure e
      else do
        lift $ $(logInfo) [i|All good|]
        pure ExitSuccess

 where
  runLogger = myLoggerT LoggerConfig { lcPrintDebug = True
                                     , colorOutter  = B.hPut stderr
                                     , rawOutter    = \_ -> pure ()
                                     }
  downloadAll dli = do
    dirs <- liftIO getDirs
    let settings = AppState (Settings True False Never Curl False GHCupURL) dirs defaultKeyBindings

    r <-
      runLogger
      . flip runReaderT settings
      . runResourceT
      . runE @'[DigestError
               , DownloadFailed
               , UnknownArchive
#if defined(TAR)
               , Tar.FormatError
#else
               , ArchiveResult
#endif
               ]
      $ do
        case tool of
          Just GHCup -> do
            let fn = "ghcup"
            dir <- liftIO ghcupCacheDir
            p <- liftE $ download dli dir (Just fn)
            liftE $ checkDigest dli p
            pure Nothing
          _ -> do
            p <- liftE $ downloadCached dli Nothing
            fmap (Just . head . splitDirectories . head)
              . liftE
              . getArchiveFiles
              $ p
    case r of
      VRight (Just basePath) -> do
        case _dlSubdir dli of
          Just (RealDir prel) -> do
            lift $ $(logInfo)
              [i|verifying subdir: #{prel}|]
            when (basePath /= prel) $ do
              lift $ $(logError)
                [i|Subdir doesn't match: expected "#{prel}", got "#{basePath}"|]
              addError
          Just (RegexDir regexString) -> do
            lift $ $(logInfo)
              [i|verifying subdir (regex): #{regexString}|]
            let regex = makeRegexOpts
                  compIgnoreCase
                  execBlank
                  regexString
            when (not (match regex basePath)) $ do
              lift $ $(logError)
                [i|Subdir doesn't match: expected regex "#{regexString}", got "#{basePath}"|]
              addError
          Nothing -> pure ()
      VRight Nothing -> pure ()
      VLeft  e -> do
        lift $ $(logError)
          [i|Could not download (or verify hash) of #{dli}, Error was: #{prettyShow e}|]
        addError
