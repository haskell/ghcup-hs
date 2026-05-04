{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeOperators     #-}

module GHCup.OptParse.Install where




import           GHCup.OptParse.Common

import           GHCup.Errors
import           GHCup.Command.Install
import           GHCup.Command.Set
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Query.GHCupDirs
import           GHCup.Input.Parsers (fromVersion, isolateParser, uriParser, toolParser, installTargetParser)
import           GHCup.Prelude
import           GHCup.Prelude.String.QQ

import           Control.Concurrent (threadDelay)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad (when, forM_)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Functor
import           Data.Maybe
import           Data.Variant.Excepts
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Pretty.Shim ( text )
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           URI.ByteString          hiding ( uriParser )
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import qualified Data.Text                     as T




    ----------------
    --[ Commands ]--
    ----------------


data InstallCommand = InstallGHC InstallOptions
                    | InstallCabal InstallOptions
                    | InstallHLS InstallOptions
                    | InstallStack InstallOptions
                    | InstallOtherTool InstallOptionsNew
                    deriving (Eq, Show)




    ---------------
    --[ Options ]--
    ---------------

data InstallOptions = InstallOptions
  { instVer      :: Maybe ToolVersion
  , instBindist  :: Maybe URI
  , instSet      :: Bool
  , isolateDir   :: Maybe FilePath
  , forceInstall :: Bool
  , installTargets :: Maybe [String]
  , addConfArgs  :: [String]
  } deriving (Eq, Show)

data InstallOptionsNew = InstallOptionsNew
  { instTool     :: Tool
  , instVer      :: Maybe ToolVersion
  , instBindist  :: Maybe URI
  , instSet      :: Bool
  , isolateDir   :: Maybe FilePath
  , forceInstall :: Bool
  , installTargets :: Maybe [String]
  , addConfArgs  :: [String]
  } deriving (Eq, Show)



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

installParser :: Parser InstallCommand
installParser =
  subparser
      (  command
          "ghc"
          (   InstallGHC
          <$> info
                (installOpts (Just ghc) <**> helper)
                (  progDesc "Install GHC"
                <> footerDoc (Just $ text installGHCFooter)
                )
          )
      <> command
           "cabal"
           (   InstallCabal
           <$> info
                 (installOpts (Just cabal) <**> helper)
                 (  progDesc "Install Cabal"
                 <> footerDoc (Just $ text installCabalFooter)
                 )
           )
      <> command
           "hls"
           (   InstallHLS
           <$> info
                 (installOpts (Just hls) <**> helper)
                 (  progDesc "Install haskell-language-server"
                 <> footerDoc (Just $ text installHLSFooter)
                 )
           )
      <> command
           "stack"
           (   InstallStack
           <$> info
                 (installOpts (Just stack) <**> helper)
                 (  progDesc "Install stack"
                 <> footerDoc (Just $ text installStackFooter)
                 )
           )
      )
    <|> ( InstallOtherTool
           <$> installOptsNew
           )
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

  # install GHC 8.10.2 from vanilla channel
  ghcup -s vanilla install ghc 8.10.2

  # install GHC head fedora bindist
  ghcup install ghc -u 'https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz?job=x86_64-linux-fedora33-release' head|]


installOpts :: Maybe Tool -> Parser InstallOptions
installOpts tool =
  (\(u, v) b is f -> InstallOptions v u b is f)
    <$> (   (   (,)
            <$> optional
                  (option
                    (eitherReader uriParser)
                    (short 'u' <> long "url" <> metavar "BINDIST_URL" <> help
                      "Install the specified version from this bindist"
                      <> completer (toolDlCompleter (fromMaybe ghc tool))
                    )
                  )
            <*> (Just <$> toolVersionTagArgument [] tool)
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
           <> help "install in an isolated absolute directory instead of the default one"
           <> completer (bashCompleter "directory")
           )
          )
    <*> switch
          (short 'f' <> long "force" <> help "Force install (THIS IS UNSAFE, only use it in Dockerfiles or CI)")
    <*> optional (option (eitherReader installTargetParser)
           (  long "install-targets"
           <> metavar "TARGETS"
           <> help "Overwrite make based install targets"
           <> completer (listCompleter ["install", "install_bin", "install_lib", "install_extra", "install_man", "install_docs", "install_data", "update_package_db"])
           )
           )
    <*> many (argument str (metavar "CONFIGURE_ARGS" <> help "Additional arguments to bindist configure, prefix with '-- ' (longopts)"))
 where
  setDefault = case tool of
    Nothing  -> False
    Just (Tool "ghc") -> False
    Just _   -> True

installOptsNew :: Parser InstallOptionsNew
installOptsNew =
  (\t (u, v) -> InstallOptionsNew t v u)
    <$> argument (eitherReader toolParser) (metavar "TOOL" <> help "Which tool to install")
    <*> (   (   (,)
            <$> optional
                  (option
                    (eitherReader uriParser)
                    (short 'u' <> long "url" <> metavar "BINDIST_URL" <> help
                      "Install the specified version from this bindist"
                    )
                  )
            <*> (Just <$> toolVersionTagArgument [] Nothing)
            )
        <|> pure (Nothing, Nothing)
        )
    <*> fmap (fromMaybe False) (invertableSwitch "set" Nothing False
      (help "Set as active version after install"))
    <*> optional
          (option
           (eitherReader isolateParser)
           (  short 'i'
           <> long "isolate"
           <> metavar "DIR"
           <> help "install in an isolated absolute directory instead of the default one"
           <> completer (bashCompleter "directory")
           )
          )
    <*> switch
          (short 'f' <> long "force" <> help "Force install (THIS IS UNSAFE, only use it in Dockerfiles or CI)")
    <*> optional (option (eitherReader installTargetParser)
           (  long "install-targets"
           <> metavar "TARGETS"
           <> help "Overwrite make based install targets"
           <> completer (listCompleter ["install", "install_bin", "install_lib", "install_extra", "install_man", "install_docs", "install_data", "update_package_db"])
           )
           )
    <*> many (argument str (metavar "CONFIGURE_ARGS" <> help "Additional arguments to bindist configure, prefix with '-- ' (longopts)"))



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


runInstTool :: AppState
            -> Excepts InstallEffects (ResourceT (ReaderT AppState IO)) a
            -> IO (VEither InstallEffects a)
runInstTool appstate' =
  flip runReaderT appstate'
  . runResourceT
  . runE
    @InstallEffects



type InstallEffects = '[ AlreadyInstalled
                       , ArchiveResult
                       , BuildFailed
                       , CopyError
                       , DigestError
                       , ContentLengthError
                       , DirNotEmpty
                       , DownloadFailed
                       , FileAlreadyExistsError
                       , FileDoesNotExistError
                       , GPGError
                       , MergeFileTreeError
                       , NextVerNotFound
                       , NoDownload
                       , NoToolVersionSet
                       , NotInstalled
                       , ProcessError
                       , TagNotFound
                       , DayNotFound
                       , TarDirDoesNotExist
                       , UninstallFailed
                       , UnknownArchive
                       , InstallSetError
                       , NoCompatiblePlatform
                       , GHCup.Errors.ParseError
                       , UnsupportedSetupCombo
                       , DistroNotFound
                       , NoCompatibleArch
                       , URIParseError
                       , NoInstallInfo
                       , GHCup.Errors.ParseError
                       , MalformedInstallInfo
                       ]



    -------------------
    --[ Entrypoints ]--
    -------------------


install :: InstallCommand -> Settings -> IO AppState -> (ReaderT LeanAppState IO () -> IO ()) -> IO ExitCode
install installCommand settings getAppState' runLogger = case installCommand of
  (InstallGHC iGHCopts) -> installOther (toInstallOptionsNew ghc iGHCopts)
  (InstallCabal iopts)  -> installOther (toInstallOptionsNew cabal iopts)
  (InstallHLS iopts)    -> installOther (toInstallOptionsNew hls iopts)
  (InstallStack iopts)  -> installOther (toInstallOptionsNew stack iopts)
  (InstallOtherTool iopts)  -> installOther iopts
 where
  guessMode = if guessVersion settings then GLax else GStrict

  toInstallOptionsNew instTool InstallOptions{..} = InstallOptionsNew{..}

  installOther :: InstallOptionsNew -> IO ExitCode
  installOther InstallOptionsNew{..} = do
    s'@AppState{ dirs = Dirs{ .. } } <- liftIO getAppState'
    (case instBindist of
       Nothing -> runInstTool s' $ do
         (v, vi) <- liftE $ fromVersion instVer guessMode instTool
         forM_ (_viPreInstall =<< vi) $ \msg -> do
           lift $ logWarn msg
           lift $ logWarn
             "...waiting for 5 seconds, you can still abort..."
           liftIO $ threadDelay 5000000 -- give the user a sec to intervene
         -- TODO: install targets
         liftE $ runBothE' (installTool
                                    instTool
                                    v
                                    (maybe GHCupInternal IsolateDir isolateDir)
                                    forceInstall
                                    addConfArgs
                                    installTargets
                                  ) $ when instSet $ when (isNothing isolateDir) $ liftE $ void $ setToolVersion instTool v
         pure vi
       Just uri -> do
         runInstTool s'{ settings = settings {noVerify = True}} $ do
           pfreq <- lift getPlatformReq
           (v, vi) <- liftE $ fromVersion instVer guessMode instTool
           forM_ (_viPreInstall =<< vi) $ \msg -> do
             lift $ logWarn msg
             lift $ logWarn
               "...waiting for 5 seconds, you can still abort..."
             liftIO $ threadDelay 5000000 -- give the user a sec to intervene
           liftE $ runBothE' (installBindist
                       instTool
                       Nothing
                       (DownloadInfo ((decUTF8Safe . serializeURIRef') uri) regexDir "" Nothing Nothing Nothing (toInstallationInputSpec <$> defaultToolInstallSpec instTool pfreq v))
                       v
                       (maybe GHCupInternal IsolateDir isolateDir)
                       forceInstall
                       addConfArgs
                       installTargets
                     )
                     $ when instSet $ when (isNothing isolateDir) $ liftE $ void $ setToolVersion instTool v
           pure vi
      )
        >>= \case
              VRight vi -> do
                runLogger $ logInfo $ T.pack (prettyShow instTool) <> " installation successful"
                forM_ (_viPostInstall =<< vi) $ \msg ->
                  runLogger $ logInfo msg
                pure ExitSuccess

              VLeft e@(V (AlreadyInstalled _ _)) -> do
                runLogger $ logWarn $ T.pack $ prettyHFError e
                pure ExitSuccess

              VLeft (V (DirNotEmpty fp)) -> do
                runLogger $ logError $
                  "Install directory " <> T.pack fp <> " is not empty."
                pure $ ExitFailure 3

              VLeft err@(V (BuildFailed tmpdir _)) -> do
                case keepDirs settings of
                  Never -> runLogger (logError $ T.pack $ prettyHFError err)
                  _ -> runLogger (logError $ T.pack (prettyHFError err) <> "\n" <>
                    "Check the logs at " <> T.pack (fromGHCupPath logsDir) <> " and the build directory " <> T.pack tmpdir <> " for more clues." <> "\n" <>
                    "Make sure to clean up " <> T.pack tmpdir <> " afterwards.")
                pure $ ExitFailure 3

              VLeft e -> do
                runLogger $ do
                  logError $ T.pack $ prettyHFError e
                  logError $ "Also check the logs in " <> T.pack (fromGHCupPath logsDir)
                pure $ ExitFailure 3
   where
    regexDir = case instTool of
                 (Tool "ghc") -> Just $ RegexDir "ghc-.*"
                 (Tool "cabal") -> Nothing
                 (Tool "stack") -> Nothing
                 (Tool "hls") -> if isWindows then Nothing else Just (RegexDir "haskell-language-server-*")
                 _ -> Nothing -- TODO

