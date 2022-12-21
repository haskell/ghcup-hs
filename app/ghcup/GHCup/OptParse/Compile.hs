{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module GHCup.OptParse.Compile where


import           GHCup
import qualified GHCup.GHC as GHC
import qualified GHCup.HLS as HLS
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ
import           GHCup.OptParse.Common

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Codec.Archive                  ( ArchiveResult )
import           Control.Concurrent (threadDelay)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Functor
import           Data.Maybe
import           Data.Versions                  ( Version, prettyVer, version, pvp )
import qualified Data.Versions as V
import           Data.Text                      ( Text )
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

import           URI.ByteString          hiding ( uriParser )
import qualified Data.Text                     as T
import Control.Exception.Safe (MonadMask, displayException)
import System.FilePath (isPathSeparator)
import Text.Read (readEither)




    ----------------
    --[ Commands ]--
    ----------------


data CompileCommand = CompileGHC GHCCompileOptions
                    | CompileHLS HLSCompileOptions



    ---------------
    --[ Options ]--
    ---------------


data GHCCompileOptions = GHCCompileOptions
  { targetGhc    :: GHC.GHCVer Version
  , bootstrapGhc :: Either Version FilePath
  , jobs         :: Maybe Int
  , buildConfig  :: Maybe FilePath
  , patches      :: Maybe (Either FilePath [URI])
  , crossTarget  :: Maybe Text
  , addConfArgs  :: [Text]
  , setCompile   :: Bool
  , ovewrwiteVer :: Maybe Version
  , buildFlavour :: Maybe String
  , hadrian      :: Bool
  , isolateDir   :: Maybe FilePath
  }


data HLSCompileOptions = HLSCompileOptions
  { targetHLS    :: HLS.HLSVer
  , jobs         :: Maybe Int
  , setCompile   :: Bool
  , updateCabal  :: Bool
  , ovewrwiteVer :: Either Bool Version
  , isolateDir   :: Maybe FilePath
  , cabalProject :: Maybe (Either FilePath URI)
  , cabalProjectLocal :: Maybe URI
  , patches      :: Maybe (Either FilePath [URI])
  , targetGHCs   :: [ToolVersion]
  , cabalArgs    :: [Text]
  }




    ---------------
    --[ Parsers ]--
    ---------------


compileP :: Parser CompileCommand
compileP = subparser
  (  command
      "ghc"
      (   CompileGHC
      <$> info
            (ghcCompileOpts <**> helper)
            (  progDesc "Compile GHC from source"
            <> footerDoc (Just $ text compileFooter)
            )
      )
  <>  command
      "hls"
      (   CompileHLS
      <$> info
            (hlsCompileOpts <**> helper)
            (  progDesc "Compile HLS from source"
            <> footerDoc (Just $ text compileHLSFooter)
            )
      )
  )
 where
  compileFooter = [s|Discussion:
  Compiles and installs the specified GHC version into
  a self-contained "~/.ghcup/ghc/<ghcver>" directory
  and symlinks the ghc binaries to "~/.ghcup/bin/<binary>-<ghcver>".

  This also allows building a cross-compiler. Consult the documentation
  first: <https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling#configuring-the-build>

ENV variables:
  Various toolchain variables will be passed onto the ghc build system,
  such as: CC, LD, OBJDUMP, NM, AR, RANLIB.

Examples:
  # compile from known version
  ghcup compile ghc -j 4 -v 8.4.2 -b 8.2.2
  # compile from git commit/reference
  ghcup compile ghc -j 4 -g master -b 8.2.2
  # specify path to bootstrap ghc
  ghcup compile ghc -j 4 -v 8.4.2 -b /usr/bin/ghc-8.2.2
  # build cross compiler
  ghcup compile ghc -j 4 -v 8.4.2 -b 8.2.2 -x armv7-unknown-linux-gnueabihf --config $(pwd)/build.mk -- --enable-unregisterised|]

  compileHLSFooter = [s|Discussion:
  Compiles and installs the specified HLS version.
  The --ghc arguments are necessary to specify which GHC version to build for/against.
  These need to be available in PATH prior to compilation.

Examples:
  # compile 1.7.0.0 from hackage for 8.10.7, running 'cabal update' before the build
  ghcup compile hls --version 1.7.0.0 --ghc 8.10.7 --cabal-update
  # compile from master for ghc 9.2.3 using 'git describe' to name the binary and ignore the pinned index state
  ghcup compile hls -g master --git-describe-version --ghc 9.2.3 -- --index-state=@(date '+%s')
  # compile a specific commit for ghc 9.2.3 and set a specifc version for the binary name
  ghcup compile hls -g a32db0b -o 1.7.0.0-p1 --ghc 9.2.3|]


ghcCompileOpts :: Parser GHCCompileOptions
ghcCompileOpts =
  GHCCompileOptions
    <$> ((GHC.SourceDist <$> option
          (eitherReader
            (first (const "Not a valid version") . version . T.pack)
          )
          (short 'v' <> long "version" <> metavar "VERSION" <> help
            "The tool version to compile"
            <> (completer $ versionCompleter Nothing GHC)
          )
          ) <|>
          (GHC.GitDist <$> (GitBranch <$> option
          str
          (short 'g' <> long "git-ref" <> metavar "GIT_REFERENCE" <> help
            "The git commit/branch/ref to build from"
          ) <*>
          optional (option str (
            short 'r' <> long "repository" <> metavar "GIT_REPOSITORY" <> help "The git repository to build from (defaults to GHC upstream)"
            <> completer (gitFileUri ["https://gitlab.haskell.org/ghc/ghc.git"])
            ))
          ))
          <|>
          (
           GHC.RemoteDist <$> (option
            (eitherReader uriParser)
            (long "remote-source-dist" <> metavar "URI" <> help
              "URI (https/http/file) to a GHC source distribution"
              <> completer fileUri
            )
          )
          )
          )
    <*> option
          (eitherReader
            (\x ->
              (bimap (const "Not a valid version") Left . version . T.pack $ x) <|> (if isPathSeparator (head x) then pure $ Right x else Left "Not an absolute Path")
            )
          )
          (  short 'b'
          <> long "bootstrap-ghc"
          <> metavar "BOOTSTRAP_GHC"
          <> help
               "The GHC version (or full path) to bootstrap with (must be installed)"
          <> (completer $ versionCompleter Nothing GHC)
          )
    <*> optional
          (option
            (eitherReader (readEither @Int))
            (short 'j' <> long "jobs" <> metavar "JOBS" <> help
              "How many jobs to use for make"
              <> (completer $ listCompleter $ fmap show ([1..12] :: [Int]))
            )
          )
    <*> optional
          (option
            str
            (short 'c' <> long "config" <> metavar "CONFIG" <> help
              "Absolute path to build config file"
             <> completer (bashCompleter "file")
            )
          )
    <*> (optional
          (
            (fmap Right $ many $ option
              (eitherReader uriParser)
              (long "patch" <> metavar "PATCH_URI" <> help
                "URI to a patch (https/http/file)"
               <> completer fileUri
              )
            )
            <|>
            (fmap Left $ option
              str
              (short 'p' <> long "patchdir" <> metavar "PATCH_DIR" <> help
                "Absolute path to patch directory (applies all .patch and .diff files in order using -p1. This order is determined by a quilt series file if it exists, or the patches are lexicographically ordered)"
               <> completer (bashCompleter "directory")
              )
            )
          )
        )
    <*> optional
          (option
            str
            (short 'x' <> long "cross-target" <> metavar "CROSS_TARGET" <> help
              "Build cross-compiler for this platform"
            )
          )
    <*> many (argument str (metavar "CONFIGURE_ARGS" <> help "Additional arguments to compile configure, prefix with '-- ' (longopts)"))
    <*> fmap (fromMaybe False) (invertableSwitch "set" Nothing False (help "Set as active version after install"))
    <*> optional
          (option
            (eitherReader
              (first (const "Not a valid version") . version . T.pack)
            )
            (short 'o' <> long "overwrite-version" <> metavar "OVERWRITE_VERSION" <> help
              "Allows to overwrite the finally installed VERSION with a different one, e.g. when you build 8.10.4 with your own patches, you might want to set this to '8.10.4-p1'"
            <> (completer $ versionCompleter Nothing GHC)
            )
          )
    <*> optional
          (option
            str
            (short 'f' <> long "flavour" <> metavar "BUILD_FLAVOUR" <> help
              "Set the compile build flavour (this value depends on the build system type: 'make' vs 'hadrian')"
            )
          )
    <*> switch
          (long "hadrian" <> help "Use the hadrian build system instead of make (only git versions seem to be properly supported atm)"
          )
    <*> optional
          (option
            (eitherReader isolateParser)
            (  short 'i'
            <> long "isolate"
            <> metavar "DIR"
            <> help "install in an isolated directory instead of the default one, no symlinks to this installation will be made"
            <> completer (bashCompleter "directory")
            )
           )

hlsCompileOpts :: Parser HLSCompileOptions
hlsCompileOpts =
  HLSCompileOptions
    <$> ((HLS.HackageDist <$> option
          (eitherReader
            ((>>= first displayException . V.version . V.prettyPVP) . first (const "Not a valid PVP version") . pvp . T.pack)
          )
          (short 'v' <> long "version" <> metavar "VERSION" <> help
            "The version to compile (pulled from hackage)"
            <> (completer $ versionCompleter' Nothing HLS (either (const False) (const True) . V.pvp . V.prettyVer))
          )
          )
          <|>
          (HLS.GitDist <$> (GitBranch <$> option
          str
          (short 'g' <> long "git-ref" <> metavar "GIT_REFERENCE" <> help
            "The git commit/branch/ref to build from (accepts anything 'git checkout' accepts)"
          ) <*>
          optional (option str (short 'r' <> long "repository" <> metavar "GIT_REPOSITORY" <> help "The git repository to build from (defaults to HLS upstream)"
            <> completer (gitFileUri ["https://github.com/haskell/haskell-language-server.git"])
          ))
          ))
          <|>
          (HLS.SourceDist <$> (option
            (eitherReader
              (first (const "Not a valid version") . version . T.pack)
            )
          (long "source-dist" <> metavar "VERSION" <> help
            "The version to compile (pulled from packaged git sources)"
            <> (completer $ versionCompleter Nothing HLS)
          )
          ))
          <|>
          (
           HLS.RemoteDist <$> (option
            (eitherReader uriParser)
            (long "remote-source-dist" <> metavar "URI" <> help
              "URI (https/http/file) to a HLS source distribution"
              <> completer fileUri
            )
          )
          )
          )
    <*> optional
          (option
            (eitherReader (readEither @Int))
            (short 'j' <> long "jobs" <> metavar "JOBS" <> help
              "How many jobs to use for make"
              <> (completer $ listCompleter $ fmap show ([1..12] :: [Int]))
            )
          )
    <*> fmap (fromMaybe True) (invertableSwitch "set" Nothing True (help "Don't set as active version after install"))
    <*> switch (long "cabal-update" <> help "Run 'cabal update' before the build")
    <*>
         (
          (Right <$> option
            (eitherReader
              (first (const "Not a valid version") . version . T.pack)
            )
            (short 'o' <> long "overwrite-version" <> metavar "OVERWRITE_VERSION" <> help
              "Allows to overwrite the finally installed VERSION with a different one, e.g. when you build 8.10.4 with your own patches, you might want to set this to '8.10.4-p1'"
            <> (completer $ versionCompleter Nothing HLS)
            )
          )
          <|>
          (Left <$> (switch
                      (long "git-describe-version"
                         <> help "Use the output of 'git describe' (if building from git) as the VERSION component of the installed binary."
                      )
                    )
          )
          )
    <*> optional
          (option
            (eitherReader isolateParser)
            (  short 'i'
            <> long "isolate"
            <> metavar "DIR"
            <> help "install in an isolated directory instead of the default one, no symlinks to this installation will be made"
            <> completer (bashCompleter "directory")
            )
           )
    <*> optional
          (option
            ((fmap Right $ eitherReader uriParser) <|> (fmap Left str))
            (long "cabal-project" <> metavar "CABAL_PROJECT" <> help
              "If relative filepath, specifies the path to cabal.project inside the unpacked HLS tarball/checkout. Otherwise expects a full URI with https/http/file scheme."
              <> completer fileUri
            )
          )
    <*> optional
          (option
            (eitherReader uriParser)
            (long "cabal-project-local" <> metavar "CABAL_PROJECT_LOCAL" <> help
              "URI (https/http/file) to a cabal.project.local to be used for the build. Will be copied over."
              <> completer fileUri
            )
          )
    <*> (optional
          (
            (fmap Right $ many $ option
              (eitherReader uriParser)
              (long "patch" <> metavar "PATCH_URI" <> help
                "URI to a patch (https/http/file)"
              <> completer fileUri
              )
            )
            <|>
            (fmap Left $ option
              str
              (short 'p' <> long "patchdir" <> metavar "PATCH_DIR" <> help
                "Absolute path to patch directory (applies all .patch and .diff files in order using -p1)"
              <> completer (bashCompleter "directory")
              )
            )
          )
        )
    <*> some (
          option (eitherReader ghcVersionTagEither)
            (  long "ghc" <> metavar "GHC_VERSION|TAG" <> help "For which GHC version to compile for (can be specified multiple times)"
            <> completer (tagCompleter GHC [])
            <> completer (versionCompleter Nothing GHC))
        )
    <*> many (argument str (metavar "CABAL_ARGS" <> help "Additional arguments to cabal install, prefix with '-- ' (longopts)"))





    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------


type GHCEffects = '[ AlreadyInstalled
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
type HLSEffects = '[ AlreadyInstalled
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
                  , TagNotFound
                  , NextVerNotFound
                  , NoToolVersionSet
                  , NotInstalled
                  , DirNotEmpty
                  , ArchiveResult
                  , UninstallFailed
                  , MergeFileTreeError
                  ]



runCompileGHC :: (MonadUnliftIO m, MonadIO m)
              => (ReaderT AppState m (VEither GHCEffects a) -> m (VEither GHCEffects a))
              -> Excepts GHCEffects (ResourceT (ReaderT AppState m)) a
              -> m (VEither GHCEffects a)
runCompileGHC runAppState =
        runAppState
        . runResourceT
        . runE
          @GHCEffects

runCompileHLS :: (MonadUnliftIO m, MonadIO m)
              => (ReaderT AppState m (VEither HLSEffects a) -> m (VEither HLSEffects a))
              -> Excepts HLSEffects (ResourceT (ReaderT AppState m)) a
              -> m (VEither HLSEffects a)
runCompileHLS runAppState =
        runAppState
        . runResourceT
        . runE
          @HLSEffects



    ------------------
    --[ Entrypoint ]--
    ------------------



compile :: ( Monad m
           , MonadMask m
           , MonadUnliftIO m
           , MonadFail m
           )
      => CompileCommand
      -> Settings
      -> Dirs
      -> (forall eff a . ReaderT AppState m (VEither eff a) -> m (VEither eff a))
      -> (ReaderT LeanAppState m () -> m ())
      -> m ExitCode
compile compileCommand settings Dirs{..} runAppState runLogger = do
  case compileCommand of
    (CompileHLS HLSCompileOptions { .. }) -> do
      runCompileHLS runAppState (do
        case targetHLS of
          HLS.SourceDist targetVer -> do
            GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
            let vi = getVersionInfo targetVer HLS dls
            forM_ (_viPreCompile =<< vi) $ \msg -> do
              lift $ logInfo msg
              lift $ logInfo
                "...waiting for 5 seconds, you can still abort..."
              liftIO $ threadDelay 5000000 -- for compilation, give the user a sec to intervene
          _ -> pure ()
        ghcs <- liftE $ forM targetGHCs (\ghc -> fmap (_tvVersion . fst) . fromVersion (Just ghc) $ GHC)
        targetVer <- liftE $ compileHLS
                    targetHLS
                    ghcs
                    jobs
                    ovewrwiteVer
                    (maybe GHCupInternal IsolateDir isolateDir)
                    cabalProject
                    cabalProjectLocal
                    updateCabal
                    patches
                    cabalArgs
        GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
        let vi = getVersionInfo targetVer HLS dls
        when setCompile $ void $ liftE $
          setHLS targetVer SetHLSOnly Nothing
        pure (vi, targetVer)
        )
        >>= \case
              VRight (vi, tv) -> do
                runLogger $ logInfo
                  "HLS successfully compiled and installed"
                forM_ (_viPostInstall =<< vi) $ \msg ->
                  runLogger $ logInfo msg
                liftIO $ putStr (T.unpack $ prettyVer tv)
                pure ExitSuccess
              VLeft err@(V (BuildFailed tmpdir _)) -> do
                case keepDirs settings of
                  Never -> runLogger $ logError $ T.pack $ prettyShow err
                  _ -> runLogger (logError $ T.pack (prettyShow err) <> "\n" <>
                        "Check the logs at " <> T.pack (fromGHCupPath logsDir) <> " and the build directory "
                        <> T.pack tmpdir <> " for more clues." <> "\n" <>
                        "Make sure to clean up " <> T.pack tmpdir <> " afterwards.")
                pure $ ExitFailure 9
              VLeft e -> do
                runLogger $ logError $ T.pack $ prettyShow e
                pure $ ExitFailure 9
    (CompileGHC GHCCompileOptions { hadrian = True, crossTarget = Just _ }) -> do
      runLogger $ logError "Hadrian cross compile support is not yet implemented!"
      pure $ ExitFailure 9
    (CompileGHC GHCCompileOptions {..}) ->
      runCompileGHC runAppState (do
        case targetGhc of
          GHC.SourceDist targetVer -> do
            GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
            let vi = getVersionInfo targetVer GHC dls
            forM_ (_viPreCompile =<< vi) $ \msg -> do
              lift $ logInfo msg
              lift $ logInfo
                "...waiting for 5 seconds, you can still abort..."
              liftIO $ threadDelay 5000000 -- for compilation, give the user a sec to intervene
          _ -> pure ()
        targetVer <- liftE $ compileGHC
                    ((\case
                        GHC.SourceDist v -> GHC.SourceDist $ GHCTargetVersion crossTarget v
                        GHC.GitDist g -> GHC.GitDist g
                        GHC.RemoteDist r -> GHC.RemoteDist r) targetGhc)
                    ovewrwiteVer
                    bootstrapGhc
                    jobs
                    buildConfig
                    patches
                    addConfArgs
                    buildFlavour
                    hadrian
                    (maybe GHCupInternal IsolateDir isolateDir)
        GHCupInfo { _ghcupDownloads = dls } <- lift getGHCupInfo
        let vi = getVersionInfo (_tvVersion targetVer) GHC dls
        when setCompile $ void $ liftE $
          setGHC targetVer SetGHCOnly Nothing
        pure (vi, targetVer)
        )
        >>= \case
              VRight (vi, tv) -> do
                runLogger $ logInfo
                  "GHC successfully compiled and installed"
                forM_ (_viPostInstall =<< vi) $ \msg ->
                  runLogger $ logInfo msg
                liftIO $ putStr (T.unpack $ tVerToText tv)
                pure ExitSuccess
              VLeft (V (AlreadyInstalled _ v)) -> do
                runLogger $ logWarn $
                  "GHC ver " <> prettyVer v <> " already installed, remove it first to reinstall"
                pure ExitSuccess
              VLeft (V (DirNotEmpty fp)) -> do
                runLogger $ logError $
                  "Install directory " <> T.pack fp <> " is not empty."
                pure $ ExitFailure 3
              VLeft err@(V (BuildFailed tmpdir _)) -> do
                case keepDirs settings of
                  Never -> runLogger $ logError $ T.pack $ prettyShow err
                  _ -> runLogger (logError $ T.pack (prettyShow err) <> "\n" <>
                        "Check the logs at " <> T.pack (fromGHCupPath logsDir) <> " and the build directory "
                        <> T.pack tmpdir <> " for more clues." <> "\n" <>
                        "Make sure to clean up " <> T.pack tmpdir <> " afterwards.")
                pure $ ExitFailure 9
              VLeft e -> do
                runLogger $ logError $ T.pack $ prettyShow e
                pure $ ExitFailure 9
