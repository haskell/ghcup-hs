{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns      #-}

module GHCup.OptParse.Install where




import           GHCup.OptParse.Common

import           GHCup
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Utils.Logger
import           GHCup.Utils.String.QQ

import           Codec.Archive
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Either
import           Data.Functor
import           Data.Maybe
import           Data.Versions           hiding ( str )
import           Haskus.Utils.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           URI.ByteString          hiding ( uriParser )

import qualified Data.Text                     as T




    ----------------
    --[ Commands ]--
    ----------------


data InstallCommand = InstallGHC InstallOptions
                    | InstallCabal InstallOptions
                    | InstallHLS InstallOptions
                    | InstallStack InstallOptions




    ---------------
    --[ Options ]--
    ---------------


data InstallOptions = InstallOptions
  { instVer      :: Maybe ToolVersion
  , instPlatform :: Maybe PlatformRequest
  , instBindist  :: Maybe URI
  , instSet      :: Bool
  , isolateDir   :: Maybe FilePath
  , forceInstall :: Bool
  }



    ---------------
    --[ Footers ]--
    ---------------

installCabalFooter :: String
installCabalFooter = [s|Discussion:
  Installs the specified cabal-install version (or a recommended default one)
  into "~/.ghcup/bin", so it can be overwritten by later
  "cabal install cabal-install", which installs into "~/.cabal/bin" by
  default. Make sure to set up your PATH appropriately, so the cabal
  installation takes precedence.|]



    ---------------
    --[ Parsers ]--
    ---------------

installParser :: Parser (Either InstallCommand InstallOptions)
installParser =
  (Left <$> subparser
      (  command
          "ghc"
          (   InstallGHC
          <$> info
                (installOpts (Just GHC) <**> helper)
                (  progDesc "Install GHC"
                <> footerDoc (Just $ text installGHCFooter)
                )
          )
      <> command
           "cabal"
           (   InstallCabal
           <$> info
                 (installOpts (Just Cabal) <**> helper)
                 (  progDesc "Install Cabal"
                 <> footerDoc (Just $ text installCabalFooter)
                 )
           )
      <> command
           "hls"
           (   InstallHLS
           <$> info
                 (installOpts (Just HLS) <**> helper)
                 (  progDesc "Install haskell-language-server"
                 <> footerDoc (Just $ text installHLSFooter)
                 )
           )
      <> command
           "stack"
           (   InstallStack
           <$> info
                 (installOpts (Just Stack) <**> helper)
                 (  progDesc "Install stack"
                 <> footerDoc (Just $ text installStackFooter)
                 )
           )
      )
    )
    <|> (Right <$> installOpts Nothing)
 where
  installHLSFooter :: String
  installHLSFooter = [s|Discussion:
  Installs haskell-language-server binaries and wrapper
  into "~/.ghcup/bin"

Examples:
  # install recommended HLS
  ghcup install hls|]

  installStackFooter :: String
  installStackFooter = [s|Discussion:
  Installs stack binaries into "~/.ghcup/bin"

Examples:
  # install recommended Stack
  ghcup install stack|]

  installGHCFooter :: String
  installGHCFooter = [s|Discussion:
  Installs the specified GHC version (or a recommended default one) into
  a self-contained "~/.ghcup/ghc/<ghcver>" directory
  and symlinks the ghc binaries to "~/.ghcup/bin/<binary>-<ghcver>".

Examples:
  # install recommended GHC
  ghcup install ghc

  # install latest GHC
  ghcup install ghc latest

  # install GHC 8.10.2
  ghcup install ghc 8.10.2

  # install GHC head fedora bindist
  ghcup install ghc -u https://gitlab.haskell.org/api/v4/projects/1/jobs/artifacts/master/raw/ghc-x86_64-fedora27-linux.tar.xz?job=validate-x86_64-linux-fedora27 head|]


installOpts :: Maybe Tool -> Parser InstallOptions
installOpts tool =
  (\p (u, v) b is f -> InstallOptions v p u b is f)
    <$> optional
          (option
            (eitherReader platformParser)
            (  short 'p'
            <> long "platform"
            <> metavar "PLATFORM"
            <> help
                 "Override for platform (triple matching ghc tarball names), e.g. x86_64-fedora27-linux"
            )
          )
    <*> (   (   (,)
            <$> optional
                  (option
                    (eitherReader uriParser)
                    (short 'u' <> long "url" <> metavar "BINDIST_URL" <> help
                      "Install the specified version from this bindist"
                      <> completer (toolDlCompleter (fromMaybe GHC tool))
                    )
                  )
            <*> (Just <$> toolVersionArgument Nothing tool)
            )
        <|> pure (Nothing, Nothing)
        )
    <*> fmap (fromMaybe setDefault) (invertableSwitch "set" Nothing setDefault
      (help $ if not setDefault then "Set as active version after install" else "Don't set as active version after install"))
    <*> optional
          (option
           (eitherReader isolateParser)
           (  short 'i'
           <> long "isolate"
           <> metavar "DIR"
           <> help "install in an isolated dir instead of the default one"
           <> completer (bashCompleter "directory")
           )
          )
    <*> switch
          (short 'f' <> long "force" <> help "Force install (THIS IS UNSAFE, only use it in Dockerfiles or CI)")
 where
  setDefault = case tool of
    Nothing  -> False
    Just GHC -> False
    Just _   -> True




    --------------
    --[ Footer ]--
    --------------


installToolFooter :: String
installToolFooter = [s|Discussion:
  Installs GHC or cabal. When no command is given, installs GHC
  with the specified version/tag.
  It is recommended to always specify a subcommand (ghc/cabal/hls/stack).|]




    ---------------------------
    --[ Effect interpreters ]--
    ---------------------------

type InstallEffects = '[ AlreadyInstalled
                       , UnknownArchive
                       , ArchiveResult
                       , FileDoesNotExistError
                       , CopyError
                       , NotInstalled
                       , DirNotEmpty
                       , NoDownload
                       , NotInstalled
                       , BuildFailed
                       , TagNotFound
                       , DigestError
                       , GPGError
                       , DownloadFailed
                       , TarDirDoesNotExist
                       , NextVerNotFound
                       , NoToolVersionSet
                       , FileAlreadyExistsError
                       , ProcessError
                       , UninstallFailed

                       , (AlreadyInstalled, ())
                       , (UnknownArchive, ())
                       , (ArchiveResult, ())
                       , (FileDoesNotExistError, ())
                       , (CopyError, ())
                       , (NotInstalled, ())
                       , (UninstallFailed, ())
                       , (DirNotEmpty, ())
                       , (NoDownload, ())
                       , (BuildFailed, ())
                       , (TagNotFound, ())
                       , (DigestError, ())
                       , (GPGError, ())
                       , (DownloadFailed, ())
                       , (TarDirDoesNotExist, ())
                       , (NextVerNotFound, ())
                       , (NoToolVersionSet, ())
                       , (FileAlreadyExistsError, ())
                       , (ProcessError, ())

                       , (AlreadyInstalled, NotInstalled)
                       , (UnknownArchive, NotInstalled)
                       , (ArchiveResult, NotInstalled)
                       , (FileDoesNotExistError, NotInstalled)
                       , (CopyError, NotInstalled)
                       , (NotInstalled, NotInstalled)
                       , (DirNotEmpty, NotInstalled)
                       , (NoDownload, NotInstalled)
                       , (NotInstalled, NotInstalled)
                       , (UninstallFailed, NotInstalled)
                       , (BuildFailed, NotInstalled)
                       , (TagNotFound, NotInstalled)
                       , (DigestError, NotInstalled)
                       , (GPGError, NotInstalled)
                       , (DownloadFailed, NotInstalled)
                       , (TarDirDoesNotExist, NotInstalled)
                       , (NextVerNotFound, NotInstalled)
                       , (NoToolVersionSet, NotInstalled)
                       , (FileAlreadyExistsError, NotInstalled)
                       , (ProcessError, NotInstalled)

                       , ((), NotInstalled)
                       ]


runInstTool :: AppState
            -> Maybe PlatformRequest
            -> Excepts InstallEffects (ResourceT (ReaderT AppState IO)) a
            -> IO (VEither InstallEffects a)
runInstTool appstate' mInstPlatform =
  flip runReaderT (maybe appstate' (\x -> appstate'{ pfreq = x } :: AppState) mInstPlatform)
  . runResourceT
  . runE
    @InstallEffects


type InstallGHCEffects = '[ TagNotFound
                          , NextVerNotFound
                          , NoToolVersionSet
                          , BuildFailed
                          , DirNotEmpty
                          , AlreadyInstalled
                          , UninstallFailed

                          , (AlreadyInstalled, NotInstalled)
                          , (UnknownArchive, NotInstalled)
                          , (ArchiveResult, NotInstalled)
                          , (FileDoesNotExistError, NotInstalled)
                          , (CopyError, NotInstalled)
                          , (NotInstalled, NotInstalled)
                          , (DirNotEmpty, NotInstalled)
                          , (NoDownload, NotInstalled)
                          , (UninstallFailed, NotInstalled)
                          , (BuildFailed, NotInstalled)
                          , (TagNotFound, NotInstalled)
                          , (DigestError, NotInstalled)
                          , (GPGError, NotInstalled)
                          , (DownloadFailed, NotInstalled)
                          , (TarDirDoesNotExist, NotInstalled)
                          , (NextVerNotFound, NotInstalled)
                          , (NoToolVersionSet, NotInstalled)
                          , (FileAlreadyExistsError, NotInstalled)
                          , (ProcessError, NotInstalled)

                          , (AlreadyInstalled, ())
                          , (UnknownArchive, ())
                          , (ArchiveResult, ())
                          , (FileDoesNotExistError, ())
                          , (CopyError, ())
                          , (NotInstalled, ())
                          , (DirNotEmpty, ())
                          , (NoDownload, ())
                          , (UninstallFailed, ())
                          , (BuildFailed, ())
                          , (TagNotFound, ())
                          , (DigestError, ())
                          , (GPGError, ())
                          , (DownloadFailed, ())
                          , (TarDirDoesNotExist, ())
                          , (NextVerNotFound, ())
                          , (NoToolVersionSet, ())
                          , (FileAlreadyExistsError, ())
                          , (ProcessError, ())

                          , ((), NotInstalled)
                          ]

runInstGHC :: AppState
           -> Maybe PlatformRequest
           -> Excepts InstallGHCEffects (ResourceT (ReaderT AppState IO)) a
           -> IO (VEither InstallGHCEffects a)
runInstGHC appstate' mInstPlatform =
  flip runReaderT (maybe appstate' (\x -> appstate'{ pfreq = x } :: AppState) mInstPlatform)
  . runResourceT
  . runE
    @InstallGHCEffects


    -------------------
    --[ Entrypoints ]--
    -------------------


install :: Either InstallCommand InstallOptions -> Settings -> IO AppState -> (ReaderT LeanAppState IO () -> IO ()) -> IO ExitCode
install installCommand settings getAppState' runLogger = case installCommand of
  (Right iopts) -> do
    runLogger (logWarn "This is an old-style command for installing GHC. Use 'ghcup install ghc' instead.")
    installGHC iopts
  (Left (InstallGHC iopts)) -> installGHC iopts
  (Left (InstallCabal iopts)) -> installCabal iopts
  (Left (InstallHLS iopts)) -> installHLS iopts
  (Left (InstallStack iopts)) -> installStack iopts
 where
  installGHC :: InstallOptions -> IO ExitCode
  installGHC InstallOptions{..} = do
    s'@AppState{ dirs = Dirs{ .. } } <- liftIO getAppState'
    (case instBindist of
       Nothing -> runInstGHC s' instPlatform $ do
         (v, vi) <- liftE $ fromVersion instVer GHC
         void $ liftE $ sequenceE (installGHCBin
                     (_tvVersion v)
                     (maybe GHCupInternal IsolateDir isolateDir)
                     forceInstall
                   )
                   $ when instSet $ when (isNothing isolateDir) $ void $ setGHC v SetGHCOnly Nothing
         pure vi
       Just uri -> do
         runInstGHC s'{ settings = settings {noVerify = True}} instPlatform $ do
           (v, vi) <- liftE $ fromVersion instVer GHC
           void $ liftE $ sequenceE (installGHCBindist
                       (DownloadInfo uri (Just $ RegexDir "ghc-.*") "")
                       (_tvVersion v)
                       (maybe GHCupInternal IsolateDir isolateDir)
                       forceInstall
                     )
                     $ when instSet $ when (isNothing isolateDir) $ void $ setGHC v SetGHCOnly Nothing
           pure vi
      )
        >>= \case
              VRight vi -> do
                runLogger $ logInfo "GHC installation successful"
                forM_ (_viPostInstall =<< vi) $ \msg ->
                  runLogger $ logInfo msg
                pure ExitSuccess

              VLeft (V (AlreadyInstalled _ v, ())) -> do
                runLogger $ logWarn $
                  "GHC ver " <> prettyVer v <> " already installed, remove it first to reinstall"
                pure ExitSuccess
              VLeft (V (AlreadyInstalled _ v)) -> do
                runLogger $ logWarn $
                  "GHC ver " <> prettyVer v <> " already installed, remove it first to reinstall"
                pure ExitSuccess

              VLeft (V (DirNotEmpty fp)) -> do
                runLogger $ logError $
                  "Install directory " <> T.pack fp <> " is not empty."
                pure $ ExitFailure 3
              VLeft (V (DirNotEmpty fp, ())) -> do
                runLogger $ logError $
                  "Install directory " <> T.pack fp <> " is not empty."
                pure $ ExitFailure 3

              VLeft err@(V (BuildFailed tmpdir _)) -> do
                case keepDirs settings of
                  Never -> runLogger (logError $ T.pack $ prettyShow err)
                  _ -> runLogger (logError $ T.pack (prettyShow err) <> "\n" <>
                    "Check the logs at " <> T.pack logsDir <> " and the build directory " <> T.pack tmpdir <> " for more clues." <> "\n" <>
                    "Make sure to clean up " <> T.pack tmpdir <> " afterwards.")
                pure $ ExitFailure 3
              VLeft err@(V (BuildFailed tmpdir _, ())) -> do
                case keepDirs settings of
                  Never -> runLogger (logError $ T.pack $ prettyShow err)
                  _ -> runLogger (logError $ T.pack (prettyShow err) <> "\n" <>
                    "Check the logs at " <> T.pack logsDir <> " and the build directory " <> T.pack tmpdir <> " for more clues." <> "\n" <>
                    "Make sure to clean up " <> T.pack tmpdir <> " afterwards.")
                pure $ ExitFailure 3

              VLeft e -> do
                runLogger $ do
                  logError $ T.pack $ prettyShow e
                  logError $ "Also check the logs in " <> T.pack logsDir
                pure $ ExitFailure 3


  installCabal :: InstallOptions -> IO ExitCode
  installCabal InstallOptions{..} = do
    s'@AppState{ dirs = Dirs{ .. } } <- liftIO getAppState'
    (case instBindist of
       Nothing -> runInstTool s' instPlatform $ do
         (_tvVersion -> v, vi) <- liftE $ fromVersion instVer Cabal
         void $ liftE $ sequenceE (installCabalBin
                                    v
                                    (maybe GHCupInternal IsolateDir isolateDir)
                                    forceInstall
                                  ) $ when instSet $ when (isNothing isolateDir) $ void $ setCabal v
         pure vi
       Just uri -> do
         runInstTool s'{ settings = settings { noVerify = True}} instPlatform $ do
           (_tvVersion -> v, vi) <- liftE $ fromVersion instVer Cabal
           void $ liftE $ sequenceE (installCabalBindist
                                      (DownloadInfo uri Nothing "")
                                      v
                                      (maybe GHCupInternal IsolateDir isolateDir)
                                      forceInstall
                                    ) $ when instSet $ when (isNothing isolateDir) $ void $ setCabal v
           pure vi
      )
      >>= \case
            VRight vi -> do
              runLogger $ logInfo "Cabal installation successful"
              forM_ (_viPostInstall =<< vi) $ \msg ->
                runLogger $ logInfo msg
              pure ExitSuccess
            VLeft (V (AlreadyInstalled _ v)) -> do
              runLogger $ logWarn $
                "Cabal ver " <> prettyVer v <> " already installed; if you really want to reinstall it, you may want to run 'ghcup install cabal --force " <> prettyVer v <> "'"
              pure ExitSuccess
            VLeft (V (FileAlreadyExistsError fp)) -> do
              runLogger $ logWarn $
                "File " <> T.pack fp <> " already exists. Use 'ghcup install cabal --isolate " <> T.pack fp <> " --force ..." <> "' if you want to overwrite."
              pure $ ExitFailure 3
            VLeft (V (AlreadyInstalled _ v, ())) -> do
              runLogger $ logWarn $
                "Cabal ver " <> prettyVer v <> " already installed; if you really want to reinstall it, you may want to run 'ghcup install cabal --force " <> prettyVer v <> "'"
              pure ExitSuccess
            VLeft (V (FileAlreadyExistsError fp, ())) -> do
              runLogger $ logWarn $
                "File " <> T.pack fp <> " already exists. Use 'ghcup install cabal --isolate " <> T.pack fp <> " --force ..." <> "' if you want to overwrite."
              pure $ ExitFailure 3
            VLeft e -> do
              runLogger $ do
                logError $ T.pack $ prettyShow e
                logError $ "Also check the logs in " <> T.pack logsDir
              pure $ ExitFailure 4

  installHLS :: InstallOptions -> IO ExitCode
  installHLS InstallOptions{..} = do
     s'@AppState{ dirs = Dirs{ .. } } <- liftIO getAppState'
     (case instBindist of
       Nothing -> runInstTool s' instPlatform $ do
         (_tvVersion -> v, vi) <- liftE $ fromVersion instVer HLS
         void $ liftE $ sequenceE (installHLSBin
                                    v
                                    (maybe GHCupInternal IsolateDir isolateDir)
                                    forceInstall
                                  ) $ when instSet $ when (isNothing isolateDir) $ void $ setHLS v SetHLSOnly Nothing
         pure vi
       Just uri -> do
         runInstTool s'{ settings = settings { noVerify = True}} instPlatform $ do
           (_tvVersion -> v, vi) <- liftE $ fromVersion instVer HLS
           -- TODO: support legacy
           void $ liftE $ sequenceE (installHLSBindist
                                      (DownloadInfo uri (Just $ RegexDir "haskell-language-server-*") "")
                                      v
                                      (maybe GHCupInternal IsolateDir isolateDir)
                                      forceInstall
                                    ) $ when instSet $ when (isNothing isolateDir) $ void $ setHLS v SetHLSOnly Nothing
           pure vi
      )
      >>= \case
            VRight vi -> do
              runLogger $ logInfo "HLS installation successful"
              forM_ (_viPostInstall =<< vi) $ \msg ->
                runLogger $ logInfo msg
              pure ExitSuccess
            VLeft (V (AlreadyInstalled _ v)) -> do
              runLogger $ logWarn $
                  "HLS ver "
                <> prettyVer v
                <> " already installed; if you really want to reinstall it, you may want to run 'ghcup install hls --force "
                <> prettyVer v
                <> "'"
              pure ExitSuccess
            VLeft (V (FileAlreadyExistsError fp)) -> do
              runLogger $ logWarn $
                "File " <> T.pack fp <> " already exists. Use 'ghcup install hls --isolate " <> T.pack fp <> " --force ..." <> "' if you want to overwrite."
              pure $ ExitFailure 3
            VLeft (V (AlreadyInstalled _ v, ())) -> do
              runLogger $ logWarn $
                  "HLS ver "
                <> prettyVer v
                <> " already installed; if you really want to reinstall it, you may want to run 'ghcup install hls --force "
                <> prettyVer v
                <> "'"
              pure ExitSuccess
            VLeft (V (FileAlreadyExistsError fp, ())) -> do
              runLogger $ logWarn $
                "File " <> T.pack fp <> " already exists. Use 'ghcup install hls --isolate " <> T.pack fp <> " --force ..." <> "' if you want to overwrite."
              pure $ ExitFailure 3
            VLeft e -> do
              runLogger $ do
                logError $ T.pack $ prettyShow e
                logError $ "Also check the logs in " <> T.pack logsDir
              pure $ ExitFailure 4

  installStack :: InstallOptions -> IO ExitCode
  installStack InstallOptions{..} = do
     s'@AppState{ dirs = Dirs{ .. } } <- liftIO getAppState'
     (case instBindist of
        Nothing -> runInstTool s' instPlatform $ do
          (_tvVersion -> v, vi) <- liftE $ fromVersion instVer Stack
          void $ liftE $ sequenceE (installStackBin
                                     v
                                     (maybe GHCupInternal IsolateDir isolateDir)
                                     forceInstall
                                   ) $ when instSet $ when (isNothing isolateDir) $ void $ setStack v
          pure vi
        Just uri -> do
          runInstTool s'{ settings = settings { noVerify = True}} instPlatform $ do
            (_tvVersion -> v, vi) <- liftE $ fromVersion instVer Stack
            void $ liftE $ sequenceE (installStackBindist
                                       (DownloadInfo uri Nothing "")
                                       v
                                       (maybe GHCupInternal IsolateDir isolateDir)
                                       forceInstall
                                     ) $ when instSet $ when (isNothing isolateDir) $ void $ setStack v
            pure vi
      )
      >>= \case
            VRight vi -> do
              runLogger $ logInfo "Stack installation successful"
              forM_ (_viPostInstall =<< vi) $ \msg ->
                runLogger $ logInfo msg
              pure ExitSuccess
            VLeft (V (AlreadyInstalled _ v)) -> do
              runLogger $ logWarn $
                "Stack ver " <> prettyVer v <> " already installed; if you really want to reinstall it, you may want to run 'ghcup install stack --force " <> prettyVer v <> "'"
              pure ExitSuccess
            VLeft (V (FileAlreadyExistsError fp)) -> do
              runLogger $ logWarn $
                "File " <> T.pack fp <> " already exists. Use 'ghcup install stack --isolate " <> T.pack fp <> " --force ..." <> "' if you want to overwrite."
              pure $ ExitFailure 3
            VLeft (V (AlreadyInstalled _ v, ())) -> do
              runLogger $ logWarn $
                "Stack ver " <> prettyVer v <> " already installed; if you really want to reinstall it, you may want to run 'ghcup install stack --force " <> prettyVer v <> "'"
              pure ExitSuccess
            VLeft (V (FileAlreadyExistsError fp, ())) -> do
              runLogger $ logWarn $
                "File " <> T.pack fp <> " already exists. Use 'ghcup install stack --isolate " <> T.pack fp <> " --force ..." <> "' if you want to overwrite."
              pure $ ExitFailure 3
            VLeft e -> do
              runLogger $ do
                logError $ T.pack $ prettyShow e
                logError $ "Also check the logs in " <> T.pack logsDir
              pure $ ExitFailure 4

