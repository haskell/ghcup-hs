{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHCup.Brick.Actions where

import           GHCup
import           GHCup.CabalConfig
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types.Optics ( getDirs, getPlatformReq, HasLog )
import           GHCup.Types         hiding ( LeanAppState(..) )
import           GHCup.Utils
import           GHCup.Prelude ( decUTF8Safe, runBothE' )
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process
import           GHCup.Prompts
import qualified GHCup.Brick.App.Common as Common
import qualified GHCup.Brick.App.AdvanceInstallOptions as AdvanceInstall
import           GHCup.Brick.Widgets.SectionList

import qualified Brick
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as F
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad (when, forM, forM_)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import           Data.Bool
import           Data.Functor
import           Data.Function ( (&), on)
import           Data.List
import           Data.Maybe
import           Data.Versions hiding (Lens')
import           Data.Variant.Excepts
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           System.IO.Unsafe
import           System.Process                 ( system )
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           URI.ByteString

import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy                as L
import qualified Graphics.Vty                  as Vty
import qualified Data.Vector                   as V
import System.Environment (getExecutablePath)
#if !IS_WINDOWS
import           GHCup.Prelude.File
import qualified System.Posix.Process          as SPP
#endif

import           System.FilePath

import           Optics.State (use)
import           Optics.State.Operators ( (.=))
import           Optics.Operators ((.~),(%~))
import           Optics.Getter (view)
import Optics.Optic ((%))
import Optics ((^.), to)
import Control.Concurrent (threadDelay)
import qualified GHCup.GHC as GHC
import qualified GHCup.Utils.Parsers as Utils
import qualified GHCup.HLS as HLS

type NavigationList = SectionList Common.Name ListResult


{- Core Logic.

This module defines the IO actions we can execute within the Brick App:
 - Install
 - Set
 - UnInstall
 - Launch the Changelog

-}

-- | Focus on the tool section and the predicate which matches. If no result matches, focus on index 0
selectBy :: Tool -> (ListResult -> Bool) -> NavigationList -> NavigationList
selectBy tool predicate internal_state =
  let new_focus = F.focusSetCurrent (Common.Singular tool) (view sectionListFocusRingL internal_state)
      tool_lens = sectionL (Common.Singular tool)
   in internal_state
        & sectionListFocusRingL .~ new_focus
        & tool_lens %~ L.listMoveTo 0            -- We move to 0 first
        & tool_lens %~ L.listFindBy predicate    -- The lookup by the predicate.

-- | Select the latests GHC tool
selectLatest :: NavigationList -> NavigationList
selectLatest = selectBy GHC (elem Latest . lTag)


-- | Replace the @appState@ or construct it based on a filter function
-- and a new @[ListResult]@ evidence.
-- When passed an existing @appState@, tries to keep the selected element.
replaceLR :: (ListResult -> Bool)
          -> [ListResult]
          -> Maybe NavigationList
          -> NavigationList
replaceLR filterF list_result s =
  let oldElem = s >>= sectionListSelectedElement -- Maybe (Int, e)
      newVec  =  [(Common.Singular $ lTool (head g), V.fromList g) | g <- groupBy ((==) `on` lTool ) (filter filterF list_result)]
      newSectionList = sectionList Common.AllTools newVec 1
  in case oldElem of
      Just (_, el) -> selectBy (lTool el) (toolEqual el) newSectionList
      Nothing -> selectLatest newSectionList
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
-- IO action returns a Left value, then it is printed.
suspendBrickAndRunAction :: (Ord n)
  => AppState
  -> ReaderT AppState IO (Either String a)
  -> Brick.EventM n s ()
suspendBrickAndRunAction s action = do
  Brick.suspendAndResume' $ do
    flip runReaderT s $ action >>= \case
      Left  err -> liftIO $ putStrLn ("Error: " <> err)
      Right _   -> liftIO $ putStrLn "Success"


getPreInstallMessage :: AppState -> GHCTargetVersion -> Tool -> Maybe T.Text
getPreInstallMessage s v lTool =
  let
    dls = _ghcupDownloads $ ghcupInfo s
  in case lTool of
    GHC   -> do
      let vi = getVersionInfo v GHC dls
      (_viPreInstall =<< vi)
    Cabal -> do
      let vi = getVersionInfo v Cabal dls
      (_viPreInstall =<< vi)
    GHCup -> do
      let vi = snd <$> getLatest dls GHCup
      (_viPreInstall =<< vi)
    HLS   -> do
      let vi = getVersionInfo v HLS dls
      (_viPreInstall =<< vi)
    Stack -> do
      let vi = getVersionInfo v Stack dls
      (_viPreInstall =<< vi)

installWithOptions :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
         => AdvanceInstall.InstallOptions
         -> ListResult
         -> m (Either String ())
installWithOptions opts ListResult {..} = do
  AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
  let
    misolated = opts ^. AdvanceInstall.isolateDir
    shouldIsolate = maybe GHCupInternal IsolateDir (opts ^. AdvanceInstall.isolateDir)
    shouldForce   = opts ^. AdvanceInstall.forceInstall
    shouldSet     = opts ^. AdvanceInstall.instSet
    extraArgs     = opts ^. AdvanceInstall.addConfArgs
    installTargets = opts ^. AdvanceInstall.installTargets
    v = fromMaybe (GHCTargetVersion lCross lVer) (opts ^. AdvanceInstall.instVersion)
    toolV = _tvVersion v
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
              ]

      withNoVerify :: (MonadReader AppState m) => m a -> m a
      withNoVerify = local (\s -> s { settings = (settings s) { noVerify = True}})

  run (do
      ce <- liftIO $ fmap (either (const Nothing) Just) $
        try @_ @SomeException $ getExecutablePath >>= canonicalizePath
      dirs <- lift getDirs
      case lTool of
        GHC   -> do
          let vi = getVersionInfo v GHC dls
          case opts ^. AdvanceInstall.instBindist of
            Nothing -> do
              liftE $
                runBothE'
                  (installGHCBin v shouldIsolate shouldForce extraArgs installTargets)
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setGHC v SetGHCOnly Nothing))
              pure (vi, dirs, ce)
            Just uri -> do
              liftE $
                runBothE'
                  (withNoVerify $ installGHCBindist
                      (DownloadInfo ((decUTF8Safe . serializeURIRef') uri) (Just $ RegexDir "ghc-.*") "" Nothing Nothing Nothing)
                      v
                      shouldIsolate
                      shouldForce
                      extraArgs
                      installTargets
                      )
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setGHC v SetGHCOnly Nothing))
              pure (vi, dirs, ce)

        Cabal -> do
          let vi = getVersionInfo v Cabal dls
          case opts ^. AdvanceInstall.instBindist of
            Nothing -> do
              liftE $
                runBothE'
                  (installCabalBin toolV shouldIsolate shouldForce)
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setCabal toolV))
              pure (vi, dirs, ce)
            Just uri -> do
              liftE $
                runBothE'
                  (withNoVerify $ installCabalBindist (DownloadInfo ((decUTF8Safe . serializeURIRef') uri) Nothing "" Nothing Nothing Nothing) toolV shouldIsolate shouldForce)
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setCabal toolV))
              pure (vi, dirs, ce)

        GHCup -> do
          let vi = snd <$> getLatest dls GHCup
          liftE $ upgradeGHCup Nothing False False $> (vi, dirs, ce)
        HLS   -> do
          let vi = getVersionInfo v HLS dls
          case opts ^. AdvanceInstall.instBindist of
            Nothing -> do
              liftE $
                runBothE'
                  (installHLSBin toolV shouldIsolate shouldForce)
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setHLS toolV SetHLSOnly Nothing))
              pure (vi, dirs, ce)
            Just uri -> do
              liftE $
                runBothE'
                  (withNoVerify $ installHLSBindist
                    (DownloadInfo ((decUTF8Safe . serializeURIRef') uri) (if isWindows then Nothing else Just (RegexDir "haskell-language-server-*")) "" Nothing Nothing Nothing)
                    toolV
                    shouldIsolate
                    shouldForce)
                  (when (shouldSet && isNothing misolated)  (liftE $ void $ setHLS toolV SetHLSOnly Nothing))
              pure (vi, dirs, ce)

        Stack -> do
          let vi = getVersionInfo v Stack dls
          case opts ^. AdvanceInstall.instBindist of
            Nothing -> do
              liftE $
                runBothE'
                  (installStackBin toolV shouldIsolate shouldForce)
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setStack toolV))
              pure (vi, dirs, ce)
            Just uri -> do
              liftE $
                runBothE'
                  (withNoVerify $ installStackBindist (DownloadInfo ((decUTF8Safe . serializeURIRef') uri) Nothing "" Nothing Nothing Nothing) toolV shouldIsolate shouldForce)
                  (when (shouldSet && isNothing misolated) (liftE $ void $ setStack toolV))
              pure (vi, dirs, ce)

    )
    >>= \case
          VRight (vi, Dirs{..}, Just ce) -> do
            forM_ (_viPostInstall =<< vi) $ \msg -> logInfo msg
            case lTool of
              GHCup -> do
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
install' (_, lr) = installWithOptions (AdvanceInstall.InstallOptions Nothing False Nothing Nothing False [] "install") lr

set' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
     => (Int, ListResult)
     -> m (Either String ())
set' input@(_, ListResult {..}) = do
  settings <- ask

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
        GHC   -> liftE $ setGHC (GHCTargetVersion lCross lVer) SetGHCOnly Nothing $> ()
        Cabal -> liftE $ setCabal lVer $> ()
        HLS   -> liftE $ setHLS lVer SetHLSOnly Nothing $> ()
        Stack -> liftE $ setStack lVer $> ()
        GHCup -> do
          promptAnswer <- getUserPromptResponse
            "Switching GHCup versions is not supported.\nDo you want to install the latest version? [Y/n]: "
            PromptYes
          case promptAnswer of
                PromptYes -> do
                  void $ liftE $ upgradeGHCup Nothing False False
                PromptNo -> pure ()
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

logGHCPostRm :: (MonadReader env m, HasLog env, MonadIO m) => GHCTargetVersion -> m ()
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

  let run = runE @'[NotInstalled, UninstallFailed]

  run (do
      let vi = getVersionInfo crossVer lTool dls
      case lTool of
        GHC   -> liftE $ rmGHCVer crossVer $> vi
        Cabal -> liftE $ rmCabalVer lVer $> vi
        HLS   -> liftE $ rmHLSVer lVer $> vi
        Stack -> liftE $ rmStackVer lVer $> vi
        GHCup -> pure Nothing
    )
    >>= \case
          VRight vi -> do
            when (lTool == GHC) $ logGHCPostRm crossVer
            logInfo $ "Successfuly removed " <> T.pack (prettyShow lTool) <> " " <> (if lTool == GHC then tVerToText crossVer else prettyVer lVer)
            forM_ (_viPostRemove =<< vi) $ \msg ->
              logInfo msg
            pure $ Right ()
          VLeft  e -> pure $ Left (prettyHFError e)
 where
  crossVer = GHCTargetVersion lCross lVer


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
              let args = "start \"\" " ++ (T.unpack $ decUTF8Safe $ serializeURIRef' uri)
              c <- liftIO $ system $ args
              case c of
                 (ExitFailure xi) -> pure $ Left $ NonZeroExit xi "cmd.exe" [args]
                 ExitSuccess -> pure $ Right ()

       >>= \case
        Right _ -> pure $ Right ()
        Left  e -> pure $ Left $ prettyHFError e

getUpdatedAppState :: AppState -> IO (Either String (AppState, [ListResult]))
getUpdatedAppState s = runExceptT $ do
  r <- ExceptT $ getGHCupInfo s
  let newS = s { ghcupInfo = r }
  ls <- liftIO $ getListResults newS
  pure (newS, ls)

  where
  getGHCupInfo :: AppState -> IO (Either String GHCupInfo)
  getGHCupInfo settings = do
    r <-
      flip runReaderT settings
      . runE @'[DigestError, ContentLengthError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError, StackPlatformDetectError]
      $ do
        pfreq <- lift getPlatformReq
        liftE $ getDownloadsF pfreq

    case r of
      VRight a -> pure $ Right a
      VLeft  e -> pure $ Left (prettyHFError e)


getListResults :: AppState -> IO [ListResult]
getListResults s =
  flip runReaderT s $ do
    lV <- listVersions Nothing [] False True (Nothing, Nothing)
    pure $ reverse lV

--
