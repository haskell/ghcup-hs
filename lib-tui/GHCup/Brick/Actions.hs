{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHCup.Brick.Actions where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types.Optics ( getDirs, getPlatformReq )
import           GHCup.Types         hiding ( LeanAppState(..) )
import           GHCup.Utils
import           GHCup.OptParse.Common (logGHCPostRm)
import           GHCup.Prelude ( decUTF8Safe )
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process
import           GHCup.Prompts
import           GHCup.Brick.Common (BrickData(..), BrickSettings(..), Name(..), Mode(..))
import qualified GHCup.Brick.Common as Common
import           GHCup.Brick.BrickState
import           GHCup.Brick.Widgets.SectionList
import           GHCup.Brick.Widgets.Navigation (BrickInternalState)

import qualified Brick
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as F
import           Codec.Archive
import           Control.Applicative
import           Control.Exception.Safe
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
import           Data.IORef (IORef, readIORef, newIORef, modifyIORef)
import           Data.Versions hiding (Lens')
import           Haskus.Utils.Variant.Excepts
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
import           System.FilePath
import qualified System.Posix.Process          as SPP
#endif

import           Optics.State (use)
import           Optics.State.Operators ( (.=))
import           Optics.Operators ((.~),(%~))
import           Optics.Getter (view)


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
updateList appD BrickState{..} =
  let newInternalState = constructList appD _appSettings (Just _appState)
  in  BrickState { _appState    = newInternalState
                 , _appData     = appD
                 , _appSettings = _appSettings
                 , _appKeys     = _appKeys
                 , _mode        = Navigation
                 }

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

-- | Select the latests GHC tool
selectLatest :: BrickInternalState -> BrickInternalState
selectLatest = selectBy GHC (elem Latest . lTag)


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

install' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m, Alternative m)
         => (Int, ListResult)
         -> m (Either String ())
install' (_, ListResult {..}) = do
  AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask

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
              ]

  run (do
      ce <- liftIO $ fmap (either (const Nothing) Just) $
        try @_ @SomeException $ getExecutablePath >>= canonicalizePath
      dirs <- lift getDirs
      case lTool of
        GHC   -> do
          let vi = getVersionInfo (GHCTargetVersion lCross lVer) GHC dls
          liftE $ installGHCBin (GHCTargetVersion lCross lVer) GHCupInternal False [] $> (vi, dirs, ce)
        Cabal -> do
          let vi = getVersionInfo (GHCTargetVersion lCross lVer) Cabal dls
          liftE $ installCabalBin lVer GHCupInternal False $> (vi, dirs, ce)
        GHCup -> do
          let vi = snd <$> getLatest dls GHCup
          liftE $ upgradeGHCup Nothing False False $> (vi, dirs, ce)
        HLS   -> do
          let vi = getVersionInfo (GHCTargetVersion lCross lVer) HLS dls
          liftE $ installHLSBin lVer GHCupInternal False $> (vi, dirs, ce)
        Stack -> do
          let vi = getVersionInfo (GHCTargetVersion lCross lVer) Stack dls
          liftE $ installStackBin lVer GHCupInternal False $> (vi, dirs, ce)
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
              ]

  run (do
      case lTool of
        GHC   -> liftE $ setGHC (GHCTargetVersion lCross lVer) SetGHCOnly Nothing $> ()
        Cabal -> liftE $ setCabal lVer $> ()
        HLS   -> liftE $ setHLS lVer SetHLSOnly Nothing $> ()
        Stack -> liftE $ setStack lVer $> ()
        GHCup -> do
          promptAnswer <- getUserPromptResponse "Switching GHCup versions is not supported.\nDo you want to install the latest version? [Y/N]: "
          case promptAnswer of
                PromptYes -> do
                  void $ liftE $ upgradeGHCup Nothing False False
                PromptNo -> pure ()
    )
    >>= \case
          VRight _ -> pure $ Right ()
          VLeft  e -> case e of
            (V (NotInstalled tool _)) -> do
              promptAnswer <- getUserPromptResponse userPrompt
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
                  <> "Would you like to install it first? [Y/N]: "

            _ -> pure $ Left (prettyHFError e)



del' :: (MonadReader AppState m, MonadIO m, MonadFail m, MonadMask m, MonadUnliftIO m)
     => (Int, ListResult)
     -> m (Either String ())
del' (_, ListResult {..}) = do
  AppState { ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask

  let run = runE @'[NotInstalled, UninstallFailed]

  run (do
      let vi = getVersionInfo (GHCTargetVersion lCross lVer) lTool dls
      case lTool of
        GHC   -> liftE $ rmGHCVer (GHCTargetVersion lCross lVer) $> vi
        Cabal -> liftE $ rmCabalVer lVer $> vi
        HLS   -> liftE $ rmHLSVer lVer $> vi
        Stack -> liftE $ rmStackVer lVer $> vi
        GHCup -> pure Nothing
    )
    >>= \case
          VRight vi -> do
            when (lTool == GHC) $ logGHCPostRm (mkTVer lVer)
            forM_ (_viPostRemove =<< vi) $ \msg ->
              logInfo msg
            pure $ Right ()
          VLeft  e -> pure $ Left (prettyHFError e)


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
            Windows -> do
              let args = "start \"\" " ++ (T.unpack $ decUTF8Safe $ serializeURIRef' uri)
              c <- liftIO $ system $ args
              case c of
                 (ExitFailure xi) -> pure $ Left $ NonZeroExit xi "cmd.exe" [args]
                 ExitSuccess -> pure $ Right ()

       >>= \case
        Right _ -> pure $ Right ()
        Left  e -> pure $ Left $ prettyHFError e


settings' :: IORef AppState
{-# NOINLINE settings' #-}
settings' = unsafePerformIO $ do
  dirs <- getAllDirs
  let loggerConfig = LoggerConfig { lcPrintDebug  = False
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
    . runE @'[DigestError, ContentLengthError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError, StackPlatformDetectError]
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

  flip runReaderT settings $ do
    lV <- listVersions Nothing [] False True (Nothing, Nothing)
    pure $ BrickData (reverse lV)

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
  , (KeyCombination (Vty.KChar 'h') [], const "help", mode .= KeyInfo)
  ]
 where
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