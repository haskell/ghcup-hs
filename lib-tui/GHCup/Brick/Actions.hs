{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHCup.Brick.Actions where

import GHCup.CabalConfig
import GHCup.Command.Install
import GHCup.Command.List
import GHCup.Command.Rm
import GHCup.Command.Set
import GHCup.Command.Upgrade
import GHCup.Download
import GHCup.Errors
import GHCup.Input.Prompts
import GHCup.Prelude
import GHCup.Prelude.Process
import GHCup.Query.GHCupDirs
import GHCup.Query.Metadata
import GHCup.System.Directory
import GHCup.Types            hiding ( LeanAppState (..) )
import GHCup.Types.Optics     ( HasLog, getDirs, getPlatformReq )

import qualified GHCup.Command.Compile.GHC as GHC
import qualified GHCup.Command.Compile.HLS as HLS
import qualified GHCup.Input.Parsers       as Utils

import GHCup.Brick.BrickState
import GHCup.Brick.Common
    ( BrickData (..), BrickSettings (..), Mode (..), Name (..) )
import GHCup.Brick.Widgets.Menu        ( MenuKeyBindings (..) )
import GHCup.Brick.Widgets.Navigation  ( BrickInternalState )
import GHCup.Brick.Widgets.SectionList

import qualified GHCup.Brick.Common                        as Common
import qualified GHCup.Brick.Widgets.Menus.AdvancedInstall as AdvancedInstall
import qualified GHCup.Brick.Widgets.Menus.CompileGHC      as CompileGHC
import qualified GHCup.Brick.Widgets.Menus.CompileHLS      as CompileHLS
import qualified GHCup.Brick.Widgets.Menus.Context         as ContextMenu

import qualified Brick
import qualified Brick.Focus            as F
import qualified Brick.Widgets.List     as L
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad          ( forM, forM_, when )
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail ( MonadFail )
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Resource
import Data.Bool
import Data.Function                  ( on, (&) )
import Data.Functor
import Data.IORef
    ( IORef, modifyIORef, newIORef, readIORef )
import Data.List
import Data.Maybe
import Data.Variant.Excepts
import Data.Versions                  hiding ( Lens' )
import Prelude                        hiding ( appendFile )
import System.Exit
import System.IO.Unsafe
import System.Process                 ( system )
import Text.PrettyPrint.HughesPJClass ( prettyShow )
import URI.ByteString

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as L
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Vector            as V
import qualified Graphics.Vty           as Vty
import           System.Environment     ( getExecutablePath )
#if !IS_WINDOWS
import qualified System.Posix.Process as SPP
#endif

import System.FilePath

import Control.Concurrent     ( threadDelay )
import Optics                 ( to, (^.) )
import Optics.Getter          ( view )
import Optics.Operators       ( (%~), (.~) )
import Optics.Optic           ( (%) )
import Optics.State           ( use )
import Optics.State.Operators ( (.=) )



{- Core Logic.

This module defines the IO actions we can execute within the Brick App:
 - Install
 - Set
 - UnInstall
 - Launch the Changelog

-}

-- | Update app data and list internal state based on new evidence.
-- This synchronises @BrickInternalState@ with @BrickData@
-- and @BrickSettings@.
updateList :: BrickData -> BrickState -> BrickState
updateList appD bst =
  let newInternalState = constructList appD (bst ^. appSettings) (Just (bst ^. appState))
  in  bst
        & appState .~ newInternalState
        & appData .~ appD
        & mode .~ Navigation

constructList :: BrickData
              -> BrickSettings
              -> Maybe BrickInternalState
              -> BrickInternalState
constructList appD settings =
  replaceLR (filterVisible (_showAllVersions settings))
            (_lr appD)

-- | Focus on the tool section and the predicate which matches. If no result matches, focus on index 0
selectBy :: Tool -> (ListResult -> Bool) -> BrickInternalState -> BrickInternalState
selectBy tool predicate internal_state =
  let new_focus = F.focusSetCurrent (Singular tool) (view sectionListFocusRingL internal_state)
      tool_lens = sectionL (Singular tool)
   in internal_state
        & sectionListFocusRingL .~ new_focus
        & tool_lens %~ L.listMoveTo 0            -- We move to 0 first
        & tool_lens %~ L.listFindBy predicate    -- The lookup by the predicate.

-- | Select the latest GHC tool
selectLatest :: BrickInternalState -> BrickInternalState
selectLatest = selectBy ghc (elem Latest . lTag)


-- | Replace the @appState@ or construct it based on a filter function
-- and a new @[ListResult]@ evidence.
-- When passed an existing @appState@, tries to keep the selected element.
replaceLR :: (ListResult -> Bool)
          -> [ListResult]
          -> Maybe BrickInternalState
          -> BrickInternalState
replaceLR filterF list_result s =
  let oldElem = s >>= sectionListSelectedElement -- Maybe (Int, e)
      newVec  =  [(Singular $ lTool (head g), V.fromList g) | g <- groupBy ((==) `on` lTool ) (filter filterF list_result)]
      newSectionList = sectionList AllTools newVec 1
  in case oldElem of
      Just (_, el) -> selectBy (lTool el) (toolEqual el) newSectionList
      Nothing      -> selectLatest newSectionList
 where
  toolEqual e1 e2 =
    lTool e1 == lTool e2 && lVer e1 == lVer e2 && lCross e1 == lCross e2


filterVisible :: Bool -> ListResult -> Bool
filterVisible v e | lInstalled e = True
                  | v
                  , Nightly `notElem` lTag e = True
                  | not v
                  , Old `notElem` lTag e
                  , Nightly `notElem` lTag e = True
                  | otherwise = (Old `notElem` lTag e)       &&
                                  (Nightly `notElem` lTag e)

-- | Suspend the current UI and run an IO action in terminal. If the
-- IO action returns a Left value, then it's thrown as userError.
withIOAction :: (Ord n, Eq n)
             => ( (Int, ListResult) -> ReaderT AppState IO (Either String a))
             -> Brick.EventM n BrickState ()
withIOAction action = do
  as <- Brick.get
  case sectionListSelectedElement (view appState as) of
    Nothing      -> pure ()
    Just (curr_ix, e) -> do
      Brick.suspendAndResume $ do
        settings <- readIORef settings'
        flip runReaderT settings $ action (curr_ix, e) >>= \case
          Left  err -> liftIO $ putStrLn ("Error: " <> err)
          Right _   -> liftIO $ putStrLn "Success"
        getAppData Nothing >>= \case
          Right data' -> do
            putStrLn "Press enter to continue"
            _ <- getLine
            pure (updateList data' as)
          Left err -> throwIO $ userError err

installWithOptions :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
         => AdvancedInstall.InstallOptions
         -> (Int, ListResult)
         -> m (Either String ())
installWithOptions opts (_, ListResult {..}) = do
  AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
  let
    misolated = opts ^. AdvancedInstall.isolateDirL
    shouldIsolate = maybe GHCupInternal IsolateDir (opts ^. AdvancedInstall.isolateDirL)
    shouldForce   = opts ^. AdvancedInstall.forceInstallL
    shouldSet     = opts ^. AdvancedInstall.instSetL
    extraArgs     = opts ^. AdvancedInstall.addConfArgsL
    installTargets = opts ^. AdvancedInstall.installTargetsL
    v = fromMaybe (TargetVersion lCross lVer) (opts ^. AdvancedInstall.instVersionL)
  let run =
        runResourceT
          . runE
            @'[ AlreadyInstalled
              , ArchiveResult
              , UnknownArchive
              , FileDoesNotExistError
              , CopyError
              , NoDownload
              , NotInstalled
              , BuildFailed
              , TagNotFound
              , DigestError
              , ContentLengthError
              , GPGError
              , DownloadFailed
              , DirNotEmpty
              , NoUpdate
              , TarDirDoesNotExist
              , FileAlreadyExistsError
              , ProcessError
              , ToolShadowed
              , UninstallFailed
              , MergeFileTreeError
              , NoCompatiblePlatform
              , GHCup.Errors.ParseError
              , UnsupportedSetupCombo
              , DistroNotFound
              , NoCompatibleArch
              , InstallSetError
              , URIParseError
              , NoInstallInfo
              , MalformedInstallInfo
              ]

      withNoVerify :: (MonadReader AppState m) => m a -> m a
      withNoVerify = local (\s -> s { settings = (settings s) { noVerify = True}})

  run (do
      ce <- liftIO $ fmap (either (const Nothing) Just) $
        try @_ @SomeException $ getExecutablePath >>= canonicalizePath
      dirs <- lift getDirs
      case lTool of
        Tool "ghcup" -> do
          let vi = snd <$> getLatest dls lTool
          forM_ (_viPreInstall =<< vi) $ \msg -> do
            lift $ logWarn msg
            lift $ logWarn
              "...waiting for 5 seconds, you can still abort..."
            liftIO $ threadDelay 5000000 -- give the user a sec to intervene
          liftE $ upgradeGHCup Nothing False False $> (vi, dirs, ce)
        _ -> do
          let vi = getVersionInfo v lTool dls
          forM_ (_viPreInstall =<< vi) $ \msg -> do
            lift $ logWarn msg
            lift $ logWarn
              "...waiting for 5 seconds, you can still abort..."
            liftIO $ threadDelay 5000000 -- give the user a sec to intervene
          case opts ^. AdvancedInstall.instBindistL of
            Nothing -> do
              liftE $
                runBothE'
                  (installTool lTool v shouldIsolate shouldForce (T.unpack <$> extraArgs) (words . T.unpack <$> installTargets))
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setToolVersion lTool v))
              pure (vi, dirs, ce)
            Just uri -> do
              liftE $
                runBothE'
                  (withNoVerify $ installBindist
                      lTool
                      (DownloadInfo ((decUTF8Safe . serializeURIRef') uri) (Just $ RegexDir "ghc-.*") "" Nothing Nothing Nothing Nothing)
                      v
                      shouldIsolate
                      shouldForce
                      (T.unpack <$> extraArgs)
                      (words . T.unpack <$> installTargets)
                      )
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setToolVersion lTool v))
              pure (vi, dirs, ce)

    )
    >>= \case
          VRight (vi, Dirs{..}, Just ce) -> do
            forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
            case lTool of
              Tool "ghcup" -> do
#if !IS_WINDOWS
                up <- liftIO $ fmap (either (const Nothing) Just)
                  $ try @_ @SomeException $ canonicalizePath (binDir </> "ghcup" <.> exeExt)
                when ((normalise <$> up) == Just (normalise ce)) $
                  -- TODO: track cli arguments of previous invocation
                  liftIO $ SPP.executeFile ce False ["tui"] Nothing
#else
                logInfo "Please restart 'ghcup' for the changes to take effect"
#endif
              _ -> pure ()
            pure $ Right ()
          VRight (vi, _, _) -> do
            forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
            logInfo "Please restart 'ghcup' for the changes to take effect"
            pure $ Right ()
          VLeft  (V (AlreadyInstalled _ _)) -> pure $ Right ()
          VLeft (V NoUpdate) -> pure $ Right ()
          VLeft e -> pure $ Left $ prettyHFError e <> "\n"
            <> "Also check the logs in ~/.ghcup/logs"

install' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
         => (Int, ListResult) -> m (Either String ())
install' = installWithOptions (AdvancedInstall.InstallOptions Nothing False Nothing Nothing False [] Nothing)

set' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
     => (Int, ListResult)
     -> m (Either String ())
set' input@(_, ListResult {..}) = do
  settings <- liftIO $ readIORef settings'

  let run =
        flip runReaderT settings
          . runResourceT
          . runE
            @'[ AlreadyInstalled
              , ArchiveResult
              , UnknownArchive
              , FileDoesNotExistError
              , CopyError
              , NoDownload
              , NotInstalled
              , BuildFailed
              , TagNotFound
              , DigestError
              , ContentLengthError
              , GPGError
              , DownloadFailed
              , DirNotEmpty
              , NoUpdate
              , TarDirDoesNotExist
              , FileAlreadyExistsError
              , ProcessError
              , ToolShadowed
              , UninstallFailed
              , MergeFileTreeError
              , NoCompatiblePlatform
              , GHCup.Errors.ParseError
              , UnsupportedSetupCombo
              , DistroNotFound
              , NoCompatibleArch
              , URIParseError
              ]

  run (do
      case lTool of
        Tool "ghcup" -> do
          promptAnswer <- getUserPromptResponse
            "Switching GHCup versions is not supported.\nDo you want to install the latest version? [Y/n]: "
            PromptYes
          case promptAnswer of
                PromptYes -> do
                  void $ liftE $ upgradeGHCup Nothing False False
                PromptNo -> pure ()
        _ -> liftE $ setToolVersion lTool (TargetVersion lCross lVer) $> ()
    )
    >>= \case
          VRight _ -> pure $ Right ()
          VLeft  e -> case e of
            (V (NotInstalled tool _)) -> do
              promptAnswer <- getUserPromptResponse userPrompt PromptYes
              case promptAnswer of
                PromptYes -> do
                  res <- install' input
                  case res of
                    (Left err) -> pure $ Left err
                    (Right _) -> do
                      logInfo "Setting now..."
                      set' input

                PromptNo -> pure $ Left (prettyHFError e)
              where
                userPrompt = L.toStrict . B.toLazyText . B.fromString $
                  "This Version of "
                  <> show tool
                  <> " you are trying to set is not installed.\n"
                  <> "Would you like to install it first? [Y/n]: "

            _ -> pure $ Left (prettyHFError e)

logGHCPostRm :: (MonadReader env m, HasLog env, MonadIO m) => TargetVersion -> m ()
logGHCPostRm ghcVer = do
  cabalStore <- liftIO $ handleIO (\_ -> if isWindows then pure "C:\\cabal\\store" else pure "~/.cabal/store or ~/.local/state/cabal/store")
    getStoreDir
  let storeGhcDir = cabalStore </> ("ghc-" <> T.unpack (prettyVer $ _tvVersion ghcVer))
  logInfo $ T.pack $ "After removing GHC you might also want to clean up your cabal store at: " <> storeGhcDir


del' :: (MonadReader AppState m, MonadIO m, MonadFail m, MonadMask m, MonadUnliftIO m)
     => (Int, ListResult)
     -> m (Either String ())
del' (_, ListResult {..}) = do
  AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask

  let run = runE @'[NotInstalled, UninstallFailed, ParseError, MalformedInstallInfo]

  run (do
      let vi = getVersionInfo crossVer lTool dls
      case lTool of
        Tool "ghcup" -> pure Nothing
        _            -> liftE $ rmToolVersion lTool crossVer $> vi
    )
    >>= \case
          VRight vi -> do
            when (lTool == ghc) $ logGHCPostRm crossVer
            logInfo $ "Successfully removed " <> T.pack (prettyShow lTool) <> " " <> (if lTool == ghc then tVerToText crossVer else prettyVer lVer)
            forM_ (_viPostRemove =<< vi) $ \msg ->
              logInfo msg
            pure $ Right ()
          VLeft  e -> pure $ Left (prettyHFError e)
 where
  crossVer = TargetVersion lCross lVer


changelog' :: (MonadReader AppState m, MonadIO m)
           => (Int, ListResult)
           -> m (Either String ())
changelog' (_, ListResult {..}) = do
  AppState { pfreq, ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
  case getChangeLog dls lTool (ToolVersion lVer) of
    Nothing -> pure $ Left $
      "Could not find ChangeLog for " <> prettyShow lTool <> ", version " <> T.unpack (prettyVer lVer)
    Just uri -> do
      case _rPlatform pfreq of
            Darwin  -> exec "open" [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing
            Linux _ -> exec "xdg-open" [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing
            FreeBSD -> exec "xdg-open" [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing
            OpenBSD -> exec "xdg-open" [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing
            Windows -> do
              let args = "start \"\" " ++ T.unpack (decUTF8Safe $ serializeURIRef' uri)
              c <- liftIO $ system args
              case c of
                 (ExitFailure xi) -> pure $ Left $ NonZeroExit xi "cmd.exe" [args]
                 ExitSuccess -> pure $ Right ()

       >>= \case
        Right _ -> pure $ Right ()
        Left  e -> pure $ Left $ prettyHFError e

compileGHC :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
           => CompileGHC.CompileGHCOptions -> (Int, ListResult) -> m (Either String ())
compileGHC compopts (_, lr@ListResult{lTool = Tool "ghc", ..}) = do
  appstate <- ask
  let run =
        runResourceT
         . runE @'[ AlreadyInstalled
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
                  , URIParseError
                  , ParseError
                  , FileAlreadyExistsError
                  , NoInstallInfo
                  , MalformedInstallInfo
                  ]
  compileResult <- run (do
      AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
      ghcVer <- case compopts ^. CompileGHC.gitRef of
        Just ref -> pure (GHC.GitDist (GitBranch ref Nothing))
        Nothing -> do
          -- Compile the version user is pointing to in the tui
          let vi = getVersionInfo (mkTVer lVer) ghc dls
          forM_ (_viPreInstall =<< vi) $ \msg -> do
            lift $ logWarn msg
            lift $ logWarn
              "...waiting for 5 seconds, you can still abort..."
            liftIO $ threadDelay 5000000 -- give the user a sec to intervene
          forM_ (_viPreCompile =<< vi) $ \msg -> do
            logInfo msg
            logInfo
              "...waiting for 5 seconds, you can still abort..."
            liftIO $ threadDelay 5000000 -- for compilation, give the user a sec to intervene
          pure (GHC.SourceDist lVer)

      targetVer <- liftE $ GHC.compileGHC
                    ghcVer
                    (compopts ^. CompileGHC.crossTarget)
                    (compopts ^. CompileGHC.overwriteVer)
                    (compopts ^. CompileGHC.bootstrapGhc)
                    (compopts ^. CompileGHC.hadrianGhc)
                    (compopts ^. CompileGHC.jobs)
                    (compopts ^. CompileGHC.buildConfig)
                    (compopts ^. CompileGHC.patches)
                    (fmap T.unpack $ compopts ^. CompileGHC.addConfArgs)
                    (compopts ^. CompileGHC.buildFlavour)
                    (compopts ^. CompileGHC.buildSystem)
                    (maybe GHCupInternal IsolateDir $ compopts ^. CompileGHC.isolateDir)
                    (fmap (words . T.unpack) $ compopts ^. CompileGHC.installTargets)
      AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls2 }} <- ask
      let vi2 = getVersionInfo targetVer ghc dls2
      when
        (compopts ^. CompileGHC.setCompile)
        (liftE . void $ setToolVersion ghc targetVer)
      pure (vi2, targetVer)
      )
  case compileResult of
      VRight (vi, tv) -> do
        logInfo "GHC successfully compiled and installed"
        forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
        liftIO $ putStr (T.unpack $ tVerToText tv)
        pure $ Right ()
      VLeft (V (AlreadyInstalled _ v)) -> do
        pure $ Left $
          "GHC ver " <> prettyShow v <> " already installed, remove it first to reinstall"
      VLeft (V (DirNotEmpty fp)) -> do
        pure $ Left $
          "Install directory " <> fp <> " is not empty."
      VLeft err@(V (BuildFailed tmpdir _)) -> pure $ Left $
        case keepDirs (appstate & settings) of
          Never -> prettyHFError err
          _ -> prettyHFError err <> "\n"
            <> "Check the logs at " <> fromGHCupPath (appstate & dirs & logsDir)
            <> " and the build directory "
            <> tmpdir <> " for more clues." <> "\n"
            <> "Make sure to clean up " <> tmpdir <> " afterwards."
      VLeft e -> do
        pure $ Left $ prettyHFError e
-- This is the case when the tool is not GHC... which should be impossible but,
-- it exhaustes pattern matches
compileGHC _ (_, ListResult{lTool = _}) = pure (Right ())


compileHLS :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
           => CompileHLS.CompileHLSOptions -> (Int, ListResult) -> m (Either String ())
compileHLS compopts (_, lr@ListResult{lTool = Tool "hls", ..}) = do
  appstate <- ask
  let run =
        runResourceT
         . runE @'[ AlreadyInstalled
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
                  , DayNotFound
                  , NextVerNotFound
                  , NoToolVersionSet
                  , NotInstalled
                  , DirNotEmpty
                  , ArchiveResult
                  , UninstallFailed
                  , MergeFileTreeError
                  , URIParseError
                  , ParseError
                  ]
  compileResult <- run (do
      AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
      hlsVer <- case compopts ^. CompileHLS.gitRef of
        Just ref -> pure (HLS.GitDist (GitBranch ref Nothing))
        Nothing -> do
          -- Compile the version user is pointing to in the tui
          let vi = getVersionInfo (mkTVer lVer) hls dls
          forM_ (_viPreInstall =<< vi) $ \msg -> do
            lift $ logWarn msg
            lift $ logWarn
              "...waiting for 5 seconds, you can still abort..."
            liftIO $ threadDelay 5000000 -- give the user a sec to intervene
          forM_ (_viPreCompile =<< vi) $ \msg -> do
            logInfo msg
            logInfo
              "...waiting for 5 seconds, you can still abort..."
            liftIO $ threadDelay 5000000 -- for compilation, give the user a sec to intervene
          pure (HLS.SourceDist lVer)

      ghcs <-
        liftE $ forM (compopts ^. CompileHLS.targetGHCs)
                     (\ghc' -> fmap (_tvVersion . fst) . Utils.fromVersion (Just ghc') GStrict $ ghc)
      targetVer <- liftE $ HLS.compileHLS
                      hlsVer
                      ghcs
                      (compopts ^. CompileHLS.jobs)
                      (compopts ^. CompileHLS.overwriteVer)
                      (maybe GHCupInternal IsolateDir $ compopts ^. CompileHLS.isolateDir)
                      (compopts ^. CompileHLS.cabalProject)
                      (compopts ^. CompileHLS.cabalProjectLocal)
                      (compopts ^. CompileHLS.updateCabal)
                      (compopts ^. CompileHLS.patches)
                      (compopts ^. CompileHLS.cabalArgs)
      AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls2 }} <- ask
      let vi2 = getVersionInfo (mkTVer targetVer) ghc dls2
      when
        (compopts ^. CompileHLS.setCompile)
        (liftE . void $ setToolVersion hls (mkTVer targetVer))
      pure (vi2, targetVer)
      )
  case compileResult of
      VRight (vi, tv) -> do
        logInfo "HLS successfully compiled and installed"
        forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
        liftIO $ putStr (T.unpack $ prettyVer tv)
        pure $ Right ()
      VLeft err@(V (BuildFailed tmpdir _)) -> pure $ Left $
        case keepDirs (appstate & settings) of
          Never -> prettyHFError err
          _ -> prettyHFError err <> "\n"
            <> "Check the logs at " <> fromGHCupPath (appstate & dirs & logsDir)
               <> " and the build directory "
            <> tmpdir <> " for more clues." <> "\n"
            <> "Make sure to clean up " <> tmpdir <> " afterwards."
      VLeft e -> do
        pure $ Left $ prettyHFError e
-- This is the case when the tool is not HLS... which should be impossible but,
-- it exhausts pattern matches
compileHLS _ (_, ListResult{lTool = _}) = pure (Right ())


settings' :: IORef AppState
{-# NOINLINE settings' #-}
settings' = unsafePerformIO $ do
  dirs <- getAllDirs
  let loggerConfig = LoggerConfig { lcPrintDebugLvl = Nothing
                                  , consoleOutter = \_ -> pure ()
                                  , fileOutter    = \_ -> pure ()
                                  , fancyColors   = True
                                  }
  newIORef $ AppState defaultSettings
                      dirs
                      defaultKeyBindings
                      (GHCupInfo mempty mempty Nothing)
                      (PlatformRequest A_64 Darwin Nothing)
                      loggerConfig


getGHCupInfo :: IO (Either String GHCupInfo)
getGHCupInfo = do
  settings <- readIORef settings'

  r <-
    flip runReaderT settings
    . runE @'[DigestError, ContentLengthError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError, StackPlatformDetectError, UnsupportedMetadataFormat]
    $ do
      pfreq <- lift getPlatformReq
      liftE $ getDownloadsF pfreq

  case r of
    VRight a -> pure $ Right a
    VLeft  e -> pure $ Left (prettyHFError e)


getAppData :: Maybe GHCupInfo
           -> IO (Either String BrickData)
getAppData mgi = runExceptT $ do
  r <- ExceptT $ maybe getGHCupInfo (pure . Right) mgi
  liftIO $ modifyIORef settings' (\s -> s { ghcupInfo = r })
  settings <- liftIO $ readIORef settings'

  r' <- lift $ flip runReaderT settings $ runE $ do
    lV <- listVersions Nothing [] False True (Nothing, Nothing)
    pure $ BrickData lV
  ExceptT $ pure $  either (Left . prettyHFError) Right $ veitherToEither r'

--

keyHandlers :: KeyBindings
            -> [ ( KeyCombination
                 , BrickSettings -> String
                 , Brick.EventM Name BrickState ()
                 )
               ]
keyHandlers KeyBindings {..} =
  [ (bQuit, const "Quit"     , Brick.halt)
  , (bInstall, const "Install"  , withIOAction install')
  , (bUninstall, const "Uninstall", withIOAction del')
  , (bSet, const "Set"      , withIOAction set')
  , (bChangelog, const "ChangeLog", withIOAction changelog')
  , ( bShowAllVersions
    , \BrickSettings {..} ->
       if _showAllVersions then "Don't show all versions" else "Show all versions"
    , hideShowHandler' (not . _showAllVersions)
    )
  , (bUp, const "Up", Common.zoom appState moveUp)
  , (bDown, const "Down", Common.zoom appState moveDown)
  , (KeyCombination (Vty.KChar 'h') [], const "Help", mode .= KeyInfo)
  , (KeyCombination Vty.KEnter [], const "Advanced options", createMenuforTool )
  ]
 where
  createMenuforTool = do
    e <- use (appState % to sectionListSelectedElement)
    case e of
      Nothing     -> pure ()
      Just (_, r) -> do
        -- Create new ContextMenu, but maintain the state of Install/Compile
        -- menus. This is especially useful in case the user made a typo and
        -- would like to retry the action.
        contextMenu .= ContextMenu.create r
          (MenuKeyBindings { mKbUp = bUp, mKbDown = bDown, mKbQuit = bQuit})
        -- Set mode to context
        mode           .= ContextPanel
    pure ()

  --hideShowHandler' :: (BrickSettings -> Bool) -> (BrickSettings -> Bool) -> m ()
  hideShowHandler' f = do
    app_settings <- use appSettings
    let
      vers = f app_settings
      newAppSettings = app_settings & Common.showAllVersions .~ vers
    ad <- use appData
    current_app_state <- use appState
    appSettings .= newAppSettings
    appState    .= constructList ad newAppSettings (Just current_app_state)
