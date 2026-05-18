{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}


module Main where

import GHCup.PlanJson
#if defined(BRICK)
import GHCup.BrickMain ( brickMain )
#endif
import GHCup.Compat.Pager
import GHCup.Download
import GHCup.Errors
import GHCup.Hardcoded.Version
import GHCup.Input.Parsers     ( resolveVersion )
import GHCup.OptParse
import GHCup.Prelude (enableAnsiSupport, decUTF8Safe')
import GHCup.Prelude.Logger
import GHCup.Prelude.String.QQ
import GHCup.Query.GHCupDirs
import GHCup.Query.Metadata
import GHCup.Query.System
import GHCup.Setup
import GHCup.Types
import GHCup.Types.Optics      hiding ( toolRequirements )

import qualified GHCup.Command.Compile.GHC as GHC
import qualified GHCup.Command.Compile.HLS as HLS

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad            ( unless )
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Data.Aeson                      ( Value, decodeStrict' )
import Data.Aeson.Encode.Pretty        ( encodePretty )
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions                   ( version )
import GHC.IO.Encoding
import Language.Haskell.TH
import Language.Haskell.TH.Syntax      ( Quasi (qAddDependentFile) )
import Options.Applicative             hiding ( ParseError, style )
import Options.Applicative.Pretty.Shim ( text )
import Prelude                         hiding ( appendFile )
import System.Environment
import System.Exit
import System.FilePath                 ( (</>) )
import System.IO                       hiding ( appendFile )
import System.IO.Unsafe                ( unsafeInterleaveIO )
import Text.PrettyPrint.HughesPJClass  ( prettyShow )
import Witherable                      ( witherM )

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO       as T
import qualified GHCup.Types        as Types



toSettings :: Bool -> Maybe FilePath -> Options -> IO (Settings, KeyBindings, UserSettings)
toSettings noColor pagerCmd options = do
  userConf <- runE @'[ JSONError ] ghcupConfigFile >>= \case
    VRight r -> pure r
    VLeft (V (JSONDecodeError e)) -> do
      B.hPut stderr ("Error decoding config file: " <> (E.encodeUtf8 . T.pack . show $ e))
      pure defaultUserSettings
    _ -> do
      die "Unexpected error!"
  pure $ (\(s', k) -> (s', k, userConf)) $ mergeConf options userConf
 where
   mergeConf :: Options -> UserSettings -> (Settings, KeyBindings)
   mergeConf Options{..} UserSettings{..} =
     let cache       = fromMaybe (fromMaybe (Types.cache defaultSettings) uCache) optCache
         metaCache   = fromMaybe (fromMaybe (Types.metaCache defaultSettings) uMetaCache) optMetaCache
         metaMode    = fromMaybe (fromMaybe (Types.metaMode defaultSettings) uMetaMode) optMetaMode
         noVerify    = fromMaybe (fromMaybe (Types.noVerify defaultSettings) uNoVerify) optNoVerify
         verbose     = maybe (fromMaybe (Types.verbose defaultSettings) uVerbose) Verbosity optVerbose
         keepDirs    = fromMaybe (fromMaybe (Types.keepDirs defaultSettings) uKeepDirs) optKeepDirs
         downloader  = fromMaybe (fromMaybe defaultDownloader uDownloader) optsDownloader
         keyBindings = maybe defaultKeyBindings mergeKeys uKeyBindings
         urlSource   = fromMaybe (maybe (Types.urlSource defaultSettings) fromURLSource uUrlSource) optUrlSource
         noNetwork   = fromMaybe (fromMaybe (Types.noNetwork defaultSettings) uNoNetwork) optNoNetwork
         gpgSetting  = fromMaybe (fromMaybe (Types.gpgSetting defaultSettings) uGPGSetting) optGpg
         platformOverride = optPlatform <|> (uPlatformOverride <|> Types.platformOverride defaultSettings)
         mirrors  = fromMaybe (Types.mirrors defaultSettings) uMirrors
         defGHCConfOptions  = fromMaybe (Types.defGHCConfOptions defaultSettings) uDefGHCConfOptions
         pager = case maybe (fromMaybe (Types.pager defaultSettings) uPager) (`PagerConfig` Nothing) optPager of
                   PagerConfig b Nothing -> PagerConfig b pagerCmd
                   x                     -> x
         guessVersion = fromMaybe (fromMaybe (Types.guessVersion defaultSettings) uGuessVersion) optGuessVersion
         buildWrapper = uBuildWrapper
     in (Settings {..}, keyBindings)
#if defined(INTERNAL_DOWNLOADER)
   defaultDownloader = Internal
#else
   defaultDownloader = Curl
#endif
   mergeKeys :: UserKeyBindings -> KeyBindings
   mergeKeys UserKeyBindings {..} =
     let KeyBindings {..} = defaultKeyBindings
     in KeyBindings {
           bUp = fromMaybe bUp kUp
         , bDown = fromMaybe bDown kDown
         , bQuit = fromMaybe bQuit kQuit
         , bLeft = fromMaybe bLeft kLeft
         , bRight = fromMaybe bRight kRight
         , bTab = fromMaybe bTab kTab
         , bInstall = fromMaybe bInstall kInstall
         , bUninstall = fromMaybe bUninstall kUninstall
         , bSet = fromMaybe bSet kSet
         , bChangelog = fromMaybe bChangelog kChangelog
         , bShowAllVersions = fromMaybe bShowAllVersions kShowAll
         }


plan_json :: String
plan_json = $( do
                (fp, c) <- runIO (handleIO (\_ -> pure ("", "")) $ do
                             fp <- findPlanJson "."
                             c <- B.readFile fp
                             (Just res) <- pure $ decodeStrict' @Value c
                             pure (fp, T.unpack $ decUTF8Safe' $ encodePretty res))
                unless (null fp) $ qAddDependentFile fp
                pure . LitE . StringL $ c)


main :: IO ()
main = do
  -- https://gitlab.haskell.org/ghc/ghc/issues/8118
  setLocaleEncoding utf8

  void enableAnsiSupport

  let versionHelp = infoOption
        ( "The GHCup Haskell installer, version " <> (head . lines $ describe_result)
        )
        (long "version" <> help "Show version" <> hidden)
  let planJson = infoOption
        plan_json
        (long "plan-json" <> help "Show the build-time configuration" <> internal)
  let numericVersionHelp = infoOption
        numericVer
        (  long "numeric-version"
        <> help "Show the numeric version (for use in scripts)"
        <> hidden
        )
  let listCommands = infoOption
        ("install set rm install-cabal list"
          <> " upgrade"
          <> " compile debug-info tool-requirements changelog"
        )
        (  long "list-commands"
        <> help "List available commands for shell completion"
        <> internal
        )

  let main_footer = [s|Discussion:
  ghcup installs the Glasgow Haskell Compiler from the official
  release channels, enabling you to easily switch between different
  versions. It maintains a self-contained ~/.ghcup directory.

ENV variables:
  * GHCUP_INSTALL_BASE_PREFIX: the base of ghcup (default: $HOME)
  * GHCUP_USE_XDG_DIRS: set to anything to use XDG style directories

Report bugs at <https://github.com/haskell/ghcup-hs/issues>|]

  args <- getArgs
  pagerCmd <- unsafeInterleaveIO getPager

  let
    parseArgsWith opts' = execParserPure
      (prefs showHelpOnError)
      (info (opts' <**> helper <**> versionHelp <**> numericVersionHelp <**> planJson <**> listCommands)
            (footerDoc (Just $ text main_footer))
      ) args


  handleParseResult' pagerCmd (argsHasHelp args) (parseArgsWith opts) >>= \case
      opt@Options {..} -> do

          dirs@Dirs{..} <- getAllDirs

          -- create ~/.ghcup dir
          ensureDirectories dirs

          no_color <- isJust <$> lookupEnv "NO_COLOR"
          (settings, keybindings, userConf) <- toSettings no_color pagerCmd opt

          -- logger interpreter
          logfile <- runReaderT initGHCupFileLogging dirs
          let loggerConfig = LoggerConfig
                { lcPrintDebugLvl = Just ((\(Verbosity i) -> i) . verbose $ settings)
                , consoleOutter  = T.hPutStr stderr
                , fileOutter    =
                    case optCommand of
                      Nuke -> \_ -> pure ()
                      _    -> T.appendFile logfile
                , fancyColors = not no_color
                }
          pfreq <- case platformOverride settings of
                     Just pfreq' -> return pfreq'
                     Nothing -> (flip runReaderT loggerConfig . runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound] . liftE $ platformRequest) >>= \case
                                    VRight r -> pure r
                                    VLeft e -> do
                                      runReaderT (logError $ T.pack $ prettyHFError e) loggerConfig
                                      exitWith (ExitFailure 2)
          let leanAppstate = LeanAppState settings dirs keybindings pfreq loggerConfig
          let runLogger = flip runReaderT leanAppstate
          let silentRunLogger = flip runReaderT (leanAppstate { loggerConfig = loggerConfig { consoleOutter = \_ -> pure () } } :: LeanAppState)


          -------------------------
          -- Setting up appstate --
          -------------------------


          let getAppState_and_updateCheckAction = do
                ghcupInfo <-
                  ( flip runReaderT leanAppstate . runE @'[ContentLengthError, DigestError, DistroNotFound, DownloadFailed, FileDoesNotExistError, GPGError, JSONError, NoCompatibleArch, NoCompatiblePlatform, NoDownload, GHCup.Errors.ParseError, ProcessError, UnsupportedSetupCombo, StackPlatformDetectError, UnsupportedMetadataFormat] $ do
                     liftE $ getDownloadsF pfreq
                  )
                    >>= \case
                          VRight r -> pure r
                          VLeft  e -> do
                            runLogger
                              (logError $ T.pack $ prettyHFError e)
                            exitWith (ExitFailure 2)
                let s' = AppState settings dirs keybindings ghcupInfo pfreq loggerConfig

                race_ (liftIO $ runReaderT cleanupTrash s')
                      (threadDelay 5000000 >> runLogger (logWarn $ "Killing cleanup thread (exceeded 5s timeout)... please remove leftover files in " <> T.pack (fromGHCupPath recycleDir) <> " manually"))

                let updateCheckAction
                      | Just 0 <- optVerbose = pure ()
                      | otherwise = void . flip runReaderT s' . runE @'[TagNotFound, DayNotFound, NextVerNotFound, NoToolVersionSet, ParseError] $ liftIO (lookupEnv "GHCUP_SKIP_UPDATE_CHECK") >>= \case
                          Nothing -> do
                            newTools <- lift checkForUpdates
                            warnMsg <- fmap (T.intercalate "\n") $ flip witherM newTools $ \newTool@(t, l) -> do
                              -- https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/283
                              alreadyInstalling' <- alreadyInstalling optCommand newTool
                              if alreadyInstalling'
                              then pure Nothing
                              else
                                pure $ Just $
                                  case t of
                                    Tool "ghcup" ->  "New GHCup version available: "
                                                 <> T.pack (prettyShow l)
                                                 <> ". To upgrade, run 'ghcup upgrade'\n"
                                                 <> "To skip this check in the future, export GHCUP_SKIP_UPDATE_CHECK=1"
                                    _ -> "New "
                                             <> T.pack (prettyShow t)
                                             <> " version available. "
                                             <> "If you want to install this latest version, run 'ghcup install "
                                             <> T.pack (prettyShow t)
                                             <> " "
                                             <> T.pack (prettyShow l)
                                             <> "'\n"
                                             <> "To skip this check in the future, export GHCUP_SKIP_UPDATE_CHECK=1"

                            let warnFile = fromGHCupPath cacheDir </> "ghcup_update_check"
                            prevWarn <- try @_ @SomeException $ liftIO $ T.readFile warnFile
                            if either (const Nothing) Just prevWarn == Just warnMsg
                            then pure ()
                            else do
                              liftIO $ T.writeFile warnFile warnMsg
                              lift $ logWarn warnMsg
                          Just _ -> do
                            logDebug "GHCUP_SKIP_UPDATE_CHECK is set, skipping update check"
                            pure ()

                silentRunLogger (flip runReaderT s' $ runE ensureShimGen) >>= \case
                  VRight _ -> pure ()
                  VLeft e -> do
                    runLogger
                      (logError $ T.pack $ prettyHFError e)
                    exitWith (ExitFailure 30)

                pure (s', updateCheckAction)




          -----------------
          -- Run command --
          -----------------

          res <- case optCommand of
#if defined(BRICK)
            Interactive -> do
              (appState, _) <- liftIO $ getAppState_and_updateCheckAction
              liftIO $ brickMain appState >> pure ExitSuccess
#endif
            Install installCommand     -> install installCommand settings (getAppState_and_updateCheckAction, leanAppstate)
            Test testCommand           -> test testCommand settings (getAppState_and_updateCheckAction, leanAppstate)
            Set setCommand             -> set setCommand settings (getAppState_and_updateCheckAction, leanAppstate)
            UnSet unsetCommand         -> unset unsetCommand (getAppState_and_updateCheckAction, leanAppstate)
            List lo                    -> list lo no_color (pager settings) (getAppState_and_updateCheckAction, leanAppstate)
            Rm rmCommand               -> rm rmCommand (getAppState_and_updateCheckAction, leanAppstate)
            DInfo                      -> dinfo (getAppState_and_updateCheckAction, leanAppstate)
            Compile compileCommand     -> compile compileCommand settings dirs (getAppState_and_updateCheckAction, leanAppstate)
            Config configCommand       -> config configCommand settings userConf keybindings (getAppState_and_updateCheckAction, leanAppstate)
            Whereis whereisOptions
                    whereisCommand     -> whereis whereisCommand whereisOptions settings (getAppState_and_updateCheckAction, leanAppstate)
            Upgrade uOpts force' fatal -> upgrade uOpts force' fatal dirs (getAppState_and_updateCheckAction, leanAppstate)
            ToolRequirements topts     -> toolRequirements topts (getAppState_and_updateCheckAction, leanAppstate)
            ChangeLog changelogOpts    -> changelog changelogOpts (getAppState_and_updateCheckAction, leanAppstate)
            Nuke                       -> nuke (getAppState_and_updateCheckAction, leanAppstate)
            Prefetch pfCom             -> prefetch pfCom settings (getAppState_and_updateCheckAction, leanAppstate)
            GC gcOpts                  -> gc gcOpts (getAppState_and_updateCheckAction, leanAppstate)
            Run runCommand             -> run runCommand settings (getAppState_and_updateCheckAction, leanAppstate)
            PrintAppErrors             -> putStrLn allHFError >> pure ExitSuccess
            HealthCheck hcCommands     -> hc hcCommands (getAppState_and_updateCheckAction, leanAppstate)
#if defined(DHALL)
            Generate gen -> generate gen (getAppState_and_updateCheckAction, leanAppstate)
#endif


          case res of
            ExitSuccess        -> pure ()
            ef@(ExitFailure _) -> exitWith ef

  pure ()

 where
  alreadyInstalling :: ( HasLog env
                       , MonadReader env m
                       , HasGHCupInfo env
                       , HasDirs env
                       , MonadIOish m
                       )
                    => Command
                    -> (Tool, TargetVersionRev)
                    -> Excepts
                         '[ TagNotFound
                          , DayNotFound
                          , NextVerNotFound
                          , NoToolVersionSet
                          , ParseError
                          ] m Bool
  alreadyInstalling (Install (InstallGHC InstallOptions{..}))     (Tool "ghc", ver)   = cmp' ghc instVer ver
  alreadyInstalling (Install (InstallCabal InstallOptions{..}))   (Tool "cabal", ver) = cmp' cabal instVer ver
  alreadyInstalling (Install (InstallHLS InstallOptions{..}))     (Tool "hls", ver)   = cmp' hls instVer ver
  alreadyInstalling (Install (InstallStack InstallOptions{..}))   (Tool "stack", ver) = cmp' stack instVer ver
  alreadyInstalling (Install (InstallOtherTool InstallOptionsNew{..})) (tool, ver)
    | instTool == tool = cmp' tool instVer ver
  alreadyInstalling (Compile (CompileGHC GHCCompileOptions{ overwriteVer = Just [S over] })) (ghc', ver)
    | Right over' <- version (T.pack over) = cmp' ghc' (Just $ GHCVersion (mkTVer over' `TargetVersionReq` Nothing)) ver
    | otherwise = pure False
  alreadyInstalling (Compile (CompileGHC GHCCompileOptions{ targetGhc = GHC.SourceDist tver }))
    (Tool "ghc", ver)   = cmp' ghc (Just $ ToolVersion (tver `VersionReq` Nothing)) ver
  alreadyInstalling (Compile (CompileHLS HLSCompileOptions{ overwriteVer = Just [S over] })) (hls', ver)
    | Right over' <- version (T.pack over) = cmp' hls' (Just $ ToolVersion (over' `VersionReq` Nothing)) ver
    | otherwise = pure False
  alreadyInstalling (Compile (CompileHLS HLSCompileOptions{ targetHLS = HLS.SourceDist tver }))
    (Tool "hls", ver)   = cmp' hls (Just $ ToolVersion (tver `VersionReq` Nothing)) ver
  alreadyInstalling (Compile (CompileHLS HLSCompileOptions{ targetHLS = HLS.HackageDist tver }))
    (Tool "hls", ver)   = cmp' hls (Just $ ToolVersion (tver `VersionReq` Nothing)) ver
  alreadyInstalling (Upgrade {}) (Tool "ghcup", _) = pure True
  alreadyInstalling _ _ = pure False

  cmp' :: ( HasLog env
          , MonadReader env m
          , HasGHCupInfo env
          , HasDirs env
          , MonadIOish m
          )
       => Tool
       -> Maybe ToolVersion
       -> TargetVersionRev
       -> Excepts
            '[ TagNotFound
             , DayNotFound
             , NextVerNotFound
             , NoToolVersionSet
             , ParseError
             ] m Bool
  cmp' tool instVer TargetVersionRev{..} = do
    GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
    (TargetVersionReq v mRev) <- liftE $ resolveVersion instVer GLax tool
    let rev = fromMaybe (maybe 0 (\(_, i, _) -> i) $ getRev dls tool v Nothing) mRev
    pure (v == _tvrTargetVer && _tvrRev == rev)

