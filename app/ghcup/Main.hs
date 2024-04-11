{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Main where

#if defined(BRICK)
-- import           BrickMain                    ( brickMain )
import           GHCup.BrickMain (brickMain)
#endif

import qualified GHCup.GHC as GHC
import qualified GHCup.HLS as HLS
import           GHCup.OptParse

import           GHCup.Download
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Types
import           GHCup.Types.Optics      hiding ( toolRequirements )
import           GHCup.Utils
import           GHCup.Utils.Parsers (fromVersion)
import           GHCup.Prelude
import           GHCup.Prelude.Logger
import           GHCup.Prelude.String.QQ
import           GHCup.Version

import           Cabal.Plan ( findPlanJson, SearchPlanJson(..) )
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Safe
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Data.Aeson                     ( decodeStrict', Value )
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Data.Either
import           Data.Functor
import           Data.Versions (version)
import           Data.Maybe
import           GHC.IO.Encoding
import           Haskus.Utils.Variant.Excepts
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( Quasi(qAddDependentFile) )
import           Options.Applicative     hiding ( style )
import           Options.Applicative.Help.Pretty ( text )
import           Prelude                 hiding ( appendFile )
import           System.Environment
import           System.Exit
import           System.IO               hiding ( appendFile )
import           Text.PrettyPrint.HughesPJClass ( prettyShow )

import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified GHCup.Types                   as Types



toSettings :: Options -> IO (Settings, KeyBindings, UserSettings)
toSettings options = do
  noColor <- isJust <$> lookupEnv "NO_COLOR"
  userConf <- runE @'[ JSONError ] ghcupConfigFile >>= \case
    VRight r -> pure r
    VLeft (V (JSONDecodeError e)) -> do
      B.hPut stderr ("Error decoding config file: " <> (E.encodeUtf8 . T.pack . show $ e))
      pure defaultUserSettings
    _ -> do
      die "Unexpected error!"
  pure $ (\(s', k) -> (s', k, userConf)) $ mergeConf options userConf noColor
 where
   mergeConf :: Options -> UserSettings -> Bool -> (Settings, KeyBindings)
   mergeConf Options{..} UserSettings{..} noColor =
     let cache       = fromMaybe (fromMaybe (Types.cache defaultSettings) uCache) optCache
         metaCache   = fromMaybe (fromMaybe (Types.metaCache defaultSettings) uMetaCache) optMetaCache
         metaMode    = fromMaybe (fromMaybe (Types.metaMode defaultSettings) uMetaMode) optMetaMode
         noVerify    = fromMaybe (fromMaybe (Types.noVerify defaultSettings) uNoVerify) optNoVerify
         verbose     = fromMaybe (fromMaybe (Types.verbose defaultSettings) uVerbose) optVerbose
         keepDirs    = fromMaybe (fromMaybe (Types.keepDirs defaultSettings) uKeepDirs) optKeepDirs
         downloader  = fromMaybe (fromMaybe defaultDownloader uDownloader) optsDownloader
         keyBindings = maybe defaultKeyBindings mergeKeys uKeyBindings
         urlSource   = fromMaybe (fromMaybe (Types.urlSource defaultSettings) uUrlSource) optUrlSource
         noNetwork   = fromMaybe (fromMaybe (Types.noNetwork defaultSettings) uNoNetwork) optNoNetwork
         gpgSetting  = fromMaybe (fromMaybe (Types.gpgSetting defaultSettings) uGPGSetting) optGpg
         platformOverride = optPlatform <|> (uPlatformOverride <|> Types.platformOverride defaultSettings)
         mirrors  = fromMaybe (Types.mirrors defaultSettings) uMirrors
         defGHCConfOptions  = fromMaybe (Types.defGHCConfOptions defaultSettings) uDefGHCConfOptions
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
         , bInstall = fromMaybe bInstall kInstall
         , bUninstall = fromMaybe bUninstall kUninstall
         , bSet = fromMaybe bSet kSet
         , bChangelog = fromMaybe bChangelog kChangelog
         , bShowAllVersions = fromMaybe bShowAllVersions kShowAll
         }



plan_json :: String
plan_json = $( do
                (fp, c) <- runIO (handleIO (\_ -> pure ("", "")) $ do
                             fp <- findPlanJson (ProjectRelativeToDir ".")
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

  customExecParser
      (prefs showHelpOnError)
      (info (opts <**> helper <**> versionHelp <**> numericVersionHelp <**> planJson <**> listCommands)
            (footerDoc (Just $ text main_footer))
      )
    >>= \opt@Options {..} -> do
          dirs@Dirs{..} <- getAllDirs

          -- create ~/.ghcup dir
          ensureDirectories dirs

          (settings, keybindings, userConf) <- toSettings opt

          -- logger interpreter
          logfile <- runReaderT initGHCupFileLogging dirs
          no_color <- isJust <$> lookupEnv "NO_COLOR"
          let loggerConfig = LoggerConfig
                { lcPrintDebug = verbose settings
                , consoleOutter  = T.hPutStr stderr
                , fileOutter    =
                    case optCommand of
                      Nuke -> \_ -> pure ()
                      _ -> T.appendFile logfile
                , fancyColors = not no_color
                }
          let leanAppstate = LeanAppState settings dirs keybindings loggerConfig
          let runLogger = flip runReaderT leanAppstate
          let siletRunLogger = flip runReaderT (leanAppstate { loggerConfig = loggerConfig { consoleOutter = \_ -> pure () } } :: LeanAppState)


          -------------------------
          -- Setting up appstate --
          -------------------------


          let appState = do
                pfreq <- case platformOverride settings of
                           Just pfreq' -> return pfreq'
                           Nothing -> (runLogger . runE @'[NoCompatiblePlatform, NoCompatibleArch, DistroNotFound] . liftE $ platformRequest) >>= \case
                                          VRight r -> pure r
                                          VLeft e -> do
                                            runLogger
                                              (logError $ T.pack $ prettyHFError e)
                                            exitWith (ExitFailure 2)

                ghcupInfo <-
                  ( flip runReaderT leanAppstate . runE @'[ContentLengthError, DigestError, DistroNotFound, DownloadFailed, FileDoesNotExistError, GPGError, JSONError, NoCompatibleArch, NoCompatiblePlatform, NoDownload, GHCup.Errors.ParseError, ProcessError, UnsupportedSetupCombo, StackPlatformDetectError] $ do
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

                case optCommand of
                  Nuke -> pure ()
                  Whereis _ _ -> pure ()
                  DInfo -> pure ()
                  ToolRequirements _ -> pure ()
                  ChangeLog _ -> pure ()
                  UnSet _ -> pure ()
#if defined(BRICK)
                  Interactive -> pure ()
#endif
                  -- check for new tools
                  _
                    | Just False <- optVerbose -> pure ()
                    | otherwise -> lookupEnv "GHCUP_SKIP_UPDATE_CHECK" >>= \case
                         Nothing -> void . flip runReaderT s' . runE @'[TagNotFound, DayNotFound, NextVerNotFound, NoToolVersionSet] $ do
                           newTools <- lift checkForUpdates
                           forM_ newTools $ \newTool@(t, l) -> do
                             -- https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/283
                             alreadyInstalling' <- alreadyInstalling optCommand newTool
                             when (not alreadyInstalling') $
                               case t of
                                 GHCup -> runLogger $
                                            logWarn ("New GHCup version available: "
                                              <> tVerToText l
                                              <> ". To upgrade, run 'ghcup upgrade'")
                                 _ -> runLogger $
                                        logWarn ("New "
                                          <> T.pack (prettyShow t)
                                          <> " version available. "
                                          <> "If you want to install this latest version, run 'ghcup install "
                                          <> T.pack (prettyShow t)
                                          <> " "
                                          <> tVerToText l
                                          <> "'")
                         Just _ -> pure ()

                -- TODO: always run for windows
                siletRunLogger (flip runReaderT s' $ runE ensureShimGen) >>= \case
                  VRight _ -> pure ()
                  VLeft e -> do
                    runLogger
                      (logError $ T.pack $ prettyHFError e)
                    exitWith (ExitFailure 30)
                pure s'


#if defined(IS_WINDOWS)
              -- FIXME: windows needs 'ensureGlobalTools', which requires
              -- full appstate
              runLeanAppState = runAppState
#else
              runLeanAppState = flip runReaderT leanAppstate
#endif
              runAppState action' = do
                s' <- liftIO appState
                runReaderT action' s'


          -----------------
          -- Run command --
          -----------------

          res <- case optCommand of
#if defined(BRICK)
            Interactive -> do
              s' <- appState
              liftIO $ brickMain s' >> pure ExitSuccess
#endif
            Install installCommand     -> install installCommand settings appState runLogger
            InstallCabalLegacy iopts   -> install (Left (InstallCabal iopts)) settings appState runLogger
            Test testCommand           -> test testCommand settings appState runLogger
            Set setCommand             -> set setCommand runAppState runLeanAppState runLogger
            UnSet unsetCommand         -> unset unsetCommand runLeanAppState runLogger
            List lo                    -> list lo no_color runAppState
            Rm rmCommand               -> rm rmCommand runAppState runLogger
            DInfo                      -> dinfo runAppState runLogger
            Compile compileCommand     -> compile compileCommand settings dirs runAppState runLogger
            Config configCommand       -> config configCommand settings userConf keybindings runLogger
            Whereis whereisOptions
                    whereisCommand     -> whereis whereisCommand whereisOptions runAppState leanAppstate runLogger
            Upgrade uOpts force' fatal -> upgrade uOpts force' fatal dirs runAppState runLogger
            ToolRequirements topts     -> toolRequirements topts runAppState runLogger
            ChangeLog changelogOpts    -> changelog changelogOpts runAppState runLogger
            Nuke                       -> nuke appState runLogger
            Prefetch pfCom             -> prefetch pfCom runAppState runLogger
            GC gcOpts                  -> gc gcOpts runAppState runLogger
            Run runCommand             -> run runCommand appState leanAppstate runLogger
            PrintAppErrors             -> putStrLn allHFError >> pure ExitSuccess

          case res of
            ExitSuccess        -> pure ()
            ef@(ExitFailure _) -> exitWith ef

  pure ()

 where
  alreadyInstalling :: ( HasLog env
                       , MonadFail m
                       , MonadReader env m
                       , HasGHCupInfo env
                       , HasDirs env
                       , MonadThrow m
                       , MonadIO m
                       , MonadCatch m
                       )
                    => Command
                    -> (Tool, GHCTargetVersion)
                    -> Excepts
                         '[ TagNotFound
                          , DayNotFound
                          , NextVerNotFound
                          , NoToolVersionSet
                          ] m Bool
  alreadyInstalling (Install (Right InstallOptions{..}))                 (GHC, ver)   = cmp' GHC instVer ver
  alreadyInstalling (Install (Left (InstallGHC InstallOptions{..})))     (GHC, ver)   = cmp' GHC instVer ver
  alreadyInstalling (Install (Left (InstallCabal InstallOptions{..})))   (Cabal, ver)    = cmp' Cabal instVer ver
  alreadyInstalling (Install (Left (InstallHLS InstallOptions{..})))     (HLS, ver)      = cmp' HLS instVer ver
  alreadyInstalling (Install (Left (InstallStack InstallOptions{..})))   (Stack, ver)    = cmp' Stack instVer ver
  alreadyInstalling (Compile (CompileGHC GHCCompileOptions{ overwriteVer = Just [S over] })) (GHC, ver)
    | Right over' <- version (T.pack over) = cmp' GHC (Just $ GHCVersion (mkTVer over')) ver
    | otherwise = pure False
  alreadyInstalling (Compile (CompileGHC GHCCompileOptions{ targetGhc = GHC.SourceDist tver }))
    (GHC, ver)   = cmp' GHC (Just $ ToolVersion tver) ver
  alreadyInstalling (Compile (CompileHLS HLSCompileOptions{ overwriteVer = Just [S over] })) (HLS, ver)
    | Right over' <- version (T.pack over) = cmp' HLS (Just $ ToolVersion over') ver
    | otherwise = pure False
  alreadyInstalling (Compile (CompileHLS HLSCompileOptions{ targetHLS = HLS.SourceDist tver }))
    (HLS, ver)   = cmp' HLS (Just $ ToolVersion tver) ver
  alreadyInstalling (Compile (CompileHLS HLSCompileOptions{ targetHLS = HLS.HackageDist tver }))
    (HLS, ver)   = cmp' HLS (Just $ ToolVersion tver) ver
  alreadyInstalling (Upgrade {}) (GHCup, _) = pure True
  alreadyInstalling _ _ = pure False

  cmp' :: ( HasLog env
          , MonadFail m
          , MonadReader env m
          , HasGHCupInfo env
          , HasDirs env
          , MonadThrow m
          , MonadIO m
          , MonadCatch m
          )
       => Tool
       -> Maybe ToolVersion
       -> GHCTargetVersion
       -> Excepts
            '[ TagNotFound
             , DayNotFound
             , NextVerNotFound
             , NoToolVersionSet
             ] m Bool
  cmp' tool instVer ver = do
    (v, _) <- liftE $ fromVersion instVer tool
    pure (v == ver)

