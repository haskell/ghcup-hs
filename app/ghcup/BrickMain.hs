{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module BrickMain where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types.Optics ( getDirs )
import           GHCup.Types         hiding ( LeanAppState(..) )
import           GHCup.Utils
import           GHCup.OptParse.Common (logGHCPostRm)
import           GHCup.Prelude ( decUTF8Safe )
import           GHCup.Prelude.File
import           GHCup.Prelude.Logger
import           GHCup.Prelude.Process
import           GHCup.Prompts

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.List             ( listSelectedFocusedAttr
                                                , listSelectedAttr
                                                , listAttr
                                                )
import           Codec.Archive
import           Control.Exception.Safe
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import           Data.Bool
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.IORef
import           Data.Vector                    ( Vector
                                                , (!?)
                                                )
import           Data.Versions
import           Haskus.Utils.Variant.Excepts
import           Prelude                 hiding ( appendFile )
import           System.FilePath
import           System.Exit
import           System.IO.Unsafe
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           URI.ByteString

import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy                as L
import qualified Graphics.Vty                  as Vty
import qualified Data.Vector                   as V
import System.Environment (getExecutablePath)
import qualified System.Posix.Process          as SPP


hiddenTools :: [Tool]
hiddenTools = []


data BrickData = BrickData
  { lr    :: [ListResult]
  }
  deriving Show

data BrickSettings = BrickSettings
  { showAllVersions    :: Bool
  , showAllTools       :: Bool
  }
  deriving Show

data BrickInternalState = BrickInternalState
  { clr :: Vector ListResult
  , ix  :: Int
  }
  deriving Show

data BrickState = BrickState
  { appData     :: BrickData
  , appSettings :: BrickSettings
  , appState    :: BrickInternalState
  , appKeys     :: KeyBindings
  }
  deriving Show


keyHandlers :: KeyBindings
            -> [ ( KeyCombination
                 , BrickSettings -> String
                 , BrickState -> EventM String BrickState ()
                 )
               ]
keyHandlers KeyBindings {..} =
  [ (bQuit, const "Quit"     , \_ -> halt)
  , (bInstall, const "Install"  , withIOAction install')
  , (bUninstall, const "Uninstall", withIOAction del')
  , (bSet, const "Set"      , withIOAction set')
  , (bChangelog, const "ChangeLog", withIOAction changelog')
  , ( bShowAllVersions
    , \BrickSettings {..} ->
       if showAllVersions then "Don't show all versions" else "Show all versions"
    , hideShowHandler (not . showAllVersions) showAllTools
    )
  , ( bShowAllTools
    , \BrickSettings {..} ->
       if showAllTools then "Don't show all tools" else "Show all tools"
    , hideShowHandler showAllVersions (not . showAllTools)
    )
  , (bUp, const "Up", \BrickState {..} -> put BrickState{ appState = moveCursor 1 appState Up, .. })
  , (bDown, const "Down", \BrickState {..} -> put BrickState{ appState = moveCursor 1 appState Down, .. })
  ]
 where
  hideShowHandler f p BrickState{..} =
    let newAppSettings   = appSettings { showAllVersions = f appSettings , showAllTools = p appSettings }
        newInternalState = constructList appData newAppSettings (Just appState)
    in  put (BrickState appData newAppSettings newInternalState appKeys)


showKey :: Vty.Key -> String
showKey (Vty.KChar c) = [c]
showKey Vty.KUp = "↑"
showKey Vty.KDown = "↓"
showKey key = tail (show key)

showMod :: Vty.Modifier -> String
showMod = tail . show


ui :: AttrMap -> BrickState -> Widget String
ui dimAttrs BrickState{ appSettings = as@BrickSettings{}, ..}
  = padBottom Max
      ( withBorderStyle unicode
        $ borderWithLabel (str "GHCup")
          (center (header <=> hBorder <=> renderList' appState))
      )
    <=> footer

 where
  footer =
    withAttr (attrName "help")
      . txtWrap
      . T.pack
      . foldr1 (\x y -> x <> "  " <> y)
      . fmap (\(KeyCombination key mods, s, _) -> intercalate "+" (showKey key : (showMod <$> mods)) <> ":" <> s as)
      $ keyHandlers appKeys
  header =
    minHSize 2 emptyWidget
      <+> padLeft (Pad 2) (minHSize 6 $ str "Tool")
      <+> minHSize 15 (str "Version")
      <+> padLeft (Pad 1) (minHSize 25 $ str "Tags")
      <+> padLeft (Pad 5) (str "Notes")
  renderList' bis@BrickInternalState{..} =
    let minTagSize = V.maximum $ V.map (length . intercalate "," . fmap tagToString . lTag) clr
        minVerSize = V.maximum $ V.map (\ListResult{..} -> T.length $ tVerToText (GHCTargetVersion lCross lVer)) clr
    in withDefAttr listAttr . drawListElements (renderItem minTagSize minVerSize) True $ bis
  renderItem minTagSize minVerSize _ b listResult@ListResult{lTag = lTag', ..} =
    let marks = if
          | lSet       -> (withAttr (attrName "set") $ str "✔✔")
          | lInstalled -> (withAttr (attrName "installed") $ str "✓ ")
          | otherwise  -> (withAttr (attrName "not-installed") $ str "✗ ")
        ver = case lCross of
          Nothing -> T.unpack . prettyVer $ lVer
          Just c  -> T.unpack (c <> "-" <> prettyVer lVer)
        dim
          | lNoBindist && not lInstalled
            && not b -- TODO: overloading dim and active ignores active
                       --       so we hack around it here
          = updateAttrMap (const dimAttrs) . withAttr (attrName "no-bindist")
          | otherwise  = id
        hooray
          | elem Latest lTag' && not lInstalled =
              withAttr (attrName "hooray")
          | otherwise = id
        active = if b then putCursor "GHCup" (Location (0,0)) . forceAttr (attrName "active") else id
    in  hooray $ active $ dim
          (   marks
          <+> padLeft (Pad 2)
               ( minHSize 6
                 (printTool lTool)
               )
          <+> minHSize minVerSize (str ver)
          <+> (let l = catMaybes . fmap printTag $ sort lTag'
               in  padLeft (Pad 1) $ minHSize minTagSize $ if null l
                     then emptyWidget
                     else foldr1 (\x y -> x <+> str "," <+> y) l
              )
          <+> padLeft (Pad 5)
              ( let notes = printNotes listResult
                in  if null notes
                      then emptyWidget
                      else foldr1 (\x y -> x <+> str "," <+> y) notes
              )
          <+> vLimit 1 (fill ' ')
          )

  printTag Recommended    = Just $ withAttr (attrName "recommended") $ str "recommended"
  printTag Latest         = Just $ withAttr (attrName "latest") $ str "latest"
  printTag Prerelease     = Just $ withAttr (attrName "prerelease") $ str "prerelease"
  printTag Nightly        = Just $ withAttr (attrName "nightly") $ str "nightly"
  printTag (Base pvp'')   = Just $ str ("base-" ++ T.unpack (prettyPVP pvp''))
  printTag Old            = Nothing
  printTag LatestPrerelease = Just $ withAttr (attrName "latest-prerelease") $ str "latest-prerelease"
  printTag LatestNightly    = Just $ withAttr (attrName "latest-nightly") $ str "latest-nightly"
  printTag (UnknownTag t) = Just $ str t

  printTool Cabal = str "cabal"
  printTool GHC = str "GHC"
  printTool GHCup = str "GHCup"
  printTool HLS = str "HLS"
  printTool Stack = str "Stack"

  printNotes ListResult {..} =
    (if hlsPowered then [withAttr (attrName "hls-powered") $ str "hls-powered"] else mempty
      )
      ++ (if lStray then [withAttr (attrName "stray") $ str "stray"] else mempty)
      ++ (case lReleaseDay of
            Nothing -> mempty
            Just d  -> [withAttr (attrName "day") $ str (show d)])

  -- | Draws the list elements.
  --
  -- Evaluates the underlying container up to, and a bit beyond, the
  -- selected element. The exact amount depends on available height
  -- for drawing and 'listItemHeight'. At most, it will evaluate up to
  -- element @(i + h + 1)@ where @i@ is the selected index and @h@ is the
  -- available height.
  drawListElements :: (Int -> Bool -> ListResult -> Widget String)
                   -> Bool
                   -> BrickInternalState
                   -> Widget String
  drawListElements drawElem foc is@(BrickInternalState clr _) =
    Widget Greedy Greedy $
      let
        es = clr
        listSelected        = fmap fst $ listSelectedElement' is

        drawnElements = flip V.imap es $ \i' e ->
          let addSeparator w = case es !? (i' - 1) of
                Just e' | lTool e' /= lTool e ->
                  hBorder <=> w
                _                             -> w

              isSelected  = Just i' == listSelected
              elemWidget  = drawElem i' isSelected e
              selItemAttr = if foc
                then withDefAttr listSelectedFocusedAttr
                else withDefAttr listSelectedAttr
              makeVisible' = if isSelected then visible . selItemAttr else id
          in  addSeparator $ makeVisible' elemWidget

      in render
        $ viewport "GHCup" Vertical
        $ vBox
        $ V.toList drawnElements


minHSize :: Int -> Widget n -> Widget n
minHSize s' = hLimit s' . vLimit 1 . (<+> fill ' ')


app :: AttrMap -> AttrMap -> App BrickState e String
app attrs dimAttrs =
  App { appDraw         = \st -> [ui dimAttrs st]
  , appHandleEvent  = \be -> get >>= \s -> eventHandler s be
  , appStartEvent   = return ()
  , appAttrMap      = const attrs
  , appChooseCursor = showFirstCursor
  }

defaultAttributes :: Bool -> AttrMap
defaultAttributes no_color = attrMap
  Vty.defAttr
  [ (attrName "active"            , Vty.defAttr `withBackColor` Vty.blue)
  , (attrName "not-installed"     , Vty.defAttr `withForeColor` Vty.red)
  , (attrName "set"               , Vty.defAttr `withForeColor` Vty.green)
  , (attrName "installed"         , Vty.defAttr `withForeColor` Vty.green)
  , (attrName "recommended"       , Vty.defAttr `withForeColor` Vty.green)
  , (attrName "hls-powered"       , Vty.defAttr `withForeColor` Vty.green)
  , (attrName "latest"            , Vty.defAttr `withForeColor` Vty.yellow)
  , (attrName "latest-prerelease" , Vty.defAttr `withForeColor` Vty.red)
  , (attrName "latest-nightly"    , Vty.defAttr `withForeColor` Vty.red)
  , (attrName "prerelease"        , Vty.defAttr `withForeColor` Vty.red)
  , (attrName "nightly"           , Vty.defAttr `withForeColor` Vty.red)
  , (attrName "compiled"          , Vty.defAttr `withForeColor` Vty.blue)
  , (attrName "stray"             , Vty.defAttr `withForeColor` Vty.blue)
  , (attrName "day"               , Vty.defAttr `withForeColor` Vty.blue)
  , (attrName "help"              , Vty.defAttr `withStyle`     Vty.italic)
  , (attrName "hooray"            , Vty.defAttr `withForeColor` Vty.brightWhite)
  ]
  where
    withForeColor | no_color  = const
                  | otherwise = Vty.withForeColor

    withBackColor | no_color  = \attr _ -> attr `Vty.withStyle` Vty.reverseVideo
                  | otherwise = Vty.withBackColor

    withStyle                 = Vty.withStyle

dimAttributes :: Bool -> AttrMap
dimAttributes no_color = attrMap
  (Vty.defAttr `Vty.withStyle` Vty.dim)
  [ (attrName "active"    , Vty.defAttr `withBackColor` Vty.blue) -- has no effect ??
  , (attrName "no-bindist", Vty.defAttr `Vty.withStyle` Vty.dim)
  ]
  where
    withBackColor | no_color  = \attr _ -> attr `Vty.withStyle` Vty.reverseVideo
                  | otherwise = Vty.withBackColor

eventHandler :: BrickState -> BrickEvent String e -> EventM String BrickState ()
eventHandler st@BrickState{..} ev = do
  AppState { keyBindings = kb } <- liftIO $ readIORef settings'
  case ev of
    (MouseDown _ Vty.BScrollUp _ _) ->
      put (BrickState { appState = moveCursor 1 appState Up, .. })
    (MouseDown _ Vty.BScrollDown _ _) ->
      put (BrickState { appState = moveCursor 1 appState Down, .. })
    (VtyEvent (Vty.EvResize _ _)) -> put st
    (VtyEvent (Vty.EvKey Vty.KUp [])) ->
      put BrickState{ appState = moveCursor 1 appState Up, .. }
    (VtyEvent (Vty.EvKey Vty.KDown [])) ->
      put BrickState{ appState = moveCursor 1 appState Down, .. }
    (VtyEvent (Vty.EvKey key mods)) ->
      case find (\(keyCombo, _, _) -> keyCombo == KeyCombination key mods) (keyHandlers kb) of
        Nothing -> put st
        Just (_, _, handler) -> handler st
    _ -> put st


moveCursor :: Int -> BrickInternalState -> Direction -> BrickInternalState
moveCursor steps ais@BrickInternalState{..} direction =
  let newIx = if direction == Down then ix + steps else ix - steps
  in  case clr !? newIx of
        Just _  -> BrickInternalState { ix = newIx, .. }
        Nothing -> ais


-- | Suspend the current UI and run an IO action in terminal. If the
-- IO action returns a Left value, then it's thrown as userError.
withIOAction :: Ord n
             => (BrickState
                 -> (Int, ListResult)
                 -> ReaderT AppState IO (Either String a))
             -> BrickState
             -> EventM n BrickState ()
withIOAction action as = case listSelectedElement' (appState as) of
  Nothing      -> put as
  Just (ix, e) -> do
    suspendAndResume $ do
      settings <- readIORef settings'
      flip runReaderT settings $ action as (ix, e) >>= \case
        Left  err -> liftIO $ putStrLn ("Error: " <> err)
        Right _   -> liftIO $ putStrLn "Success"
      getAppData Nothing >>= \case
        Right data' -> do
          putStrLn "Press enter to continue"
          _ <- getLine
          pure (updateList data' as)
        Left err -> throwIO $ userError err


-- | Update app data and list internal state based on new evidence.
-- This synchronises @BrickInternalState@ with @BrickData@
-- and @BrickSettings@.
updateList :: BrickData -> BrickState -> BrickState
updateList appD BrickState{..} =
  let newInternalState = constructList appD appSettings (Just appState)
  in  BrickState { appState    = newInternalState
                 , appData     = appD
                 , appSettings = appSettings
                 , appKeys     = appKeys
                 }


constructList :: BrickData
              -> BrickSettings
              -> Maybe BrickInternalState
              -> BrickInternalState
constructList appD appSettings =
  replaceLR (filterVisible (showAllVersions appSettings)
                           (showAllTools appSettings))
            (lr appD)

listSelectedElement' :: BrickInternalState -> Maybe (Int, ListResult)
listSelectedElement' BrickInternalState{..} = fmap (ix, ) $ clr !? ix


selectLatest :: Vector ListResult -> Int
selectLatest = fromMaybe 0 . V.findIndex (\ListResult {..} -> lTool == GHC && Latest `elem` lTag)


-- | Replace the @appState@ or construct it based on a filter function
-- and a new @[ListResult]@ evidence.
-- When passed an existing @appState@, tries to keep the selected element.
replaceLR :: (ListResult -> Bool)
          -> [ListResult]
          -> Maybe BrickInternalState
          -> BrickInternalState
replaceLR filterF lr s =
  let oldElem = s >>= listSelectedElement'
      newVec  = V.fromList . filter filterF $ lr
      newSelected =
        case oldElem >>= \(_, oldE) -> V.findIndex (toolEqual oldE) newVec of
          Just ix -> ix
          Nothing -> selectLatest newVec
  in  BrickInternalState newVec newSelected
 where
  toolEqual e1 e2 =
    lTool e1 == lTool e2 && lVer e1 == lVer e2 && lCross e1 == lCross e2


filterVisible :: Bool -> Bool -> ListResult -> Bool
filterVisible v t e | lInstalled e = True
                    | v
                    , not t
                    , Nightly `notElem` lTag e
                    , lTool e `notElem` hiddenTools = True
                    | not v
                    , t
                    , Old `notElem` lTag e
                    , Nightly `notElem` lTag e = True
                    | v
                    , Nightly `notElem` lTag e
                    , t = True
                    | otherwise = (Old `notElem` lTag e)       &&
                                  (Nightly `notElem` lTag e)   &&
                                  (lTool e `notElem` hiddenTools)


install' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m)
         => BrickState
         -> (Int, ListResult)
         -> m (Either String ())
install' _ (_, ListResult {..}) = do
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
                up <- liftIO $ fmap (either (const Nothing) Just)
                  $ try @_ @SomeException $ canonicalizePath (binDir </> "ghcup" <.> exeExt)
                when ((normalise <$> up) == Just (normalise ce)) $
                  -- TODO: track cli arguments of previous invocation
                  liftIO $ SPP.executeFile ce False ["tui"] Nothing
                logInfo "Please restart 'ghcup' for the changes to take effect"
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


set' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m)
     => BrickState
     -> (Int, ListResult)
     -> m (Either String ())
set' bs input@(_, ListResult {..}) = do
  settings <- liftIO $ readIORef settings'

  let run =
        flip runReaderT settings
          . runE @'[FileDoesNotExistError , NotInstalled , TagNotFound]

  run (do
      case lTool of
        GHC   -> liftE $ setGHC (GHCTargetVersion lCross lVer) SetGHCOnly Nothing $> ()
        Cabal -> liftE $ setCabal lVer $> ()
        HLS   -> liftE $ setHLS lVer SetHLSOnly Nothing $> ()
        Stack -> liftE $ setStack lVer $> ()
        GHCup -> pure ()
    )
    >>= \case
          VRight _ -> pure $ Right ()
          VLeft  e -> case e of
            (V (NotInstalled tool _)) -> do
              promptAnswer <- getUserPromptResponse userPrompt
              case promptAnswer of
                PromptYes -> do
                  res <- install' bs input
                  case res of
                    (Left err) -> pure $ Left err
                    (Right _) -> do
                      logInfo "Setting now..."
                      set' bs input

                PromptNo -> pure $ Left (prettyHFError e)
              where
                userPrompt = L.toStrict . B.toLazyText . B.fromString $
                  "This Version of "
                  <> show tool
                  <> " you are trying to set is not installed.\n"
                  <> "Would you like to install it first? [Y/N]: "

            _ -> pure $ Left (prettyHFError e)



del' :: (MonadReader AppState m, MonadIO m, MonadFail m, MonadMask m, MonadUnliftIO m)
     => BrickState
     -> (Int, ListResult)
     -> m (Either String ())
del' _ (_, ListResult {..}) = do
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
           => BrickState
           -> (Int, ListResult)
           -> m (Either String ())
changelog' _ (_, ListResult {..}) = do
  AppState { pfreq, ghcupInfo = GHCupInfo { _ghcupDownloads = dls }} <- ask
  case getChangeLog dls lTool (ToolVersion lVer) of
    Nothing -> pure $ Left $
      "Could not find ChangeLog for " <> prettyShow lTool <> ", version " <> T.unpack (prettyVer lVer)
    Just uri -> do
      let cmd = case _rPlatform pfreq of
            Darwin  -> "open"
            Linux _ -> "xdg-open"
            FreeBSD -> "xdg-open"
            Windows -> "start"
      exec cmd [T.unpack $ decUTF8Safe $ serializeURIRef' uri] Nothing Nothing >>= \case
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
                      (GHCupInfo mempty mempty mempty)
                      (PlatformRequest A_64 Darwin Nothing)
                      loggerConfig



brickMain :: AppState
          -> IO ()
brickMain s = do
  writeIORef settings' s

  eAppData <- getAppData (Just $ ghcupInfo s)
  case eAppData of
    Right ad ->
      defaultMain
          (app (defaultAttributes (noColor $ settings s)) (dimAttributes (noColor $ settings s)))
          (BrickState ad
                    defaultAppSettings
                    (constructList ad defaultAppSettings Nothing)
                    (keyBindings (s :: AppState))

          )
        $> ()
    Left e -> do
      flip runReaderT s $ logError $ "Error building app state: " <> T.pack (show e)
      exitWith $ ExitFailure 2


defaultAppSettings :: BrickSettings
defaultAppSettings = BrickSettings { showAllVersions = False, showAllTools = False }


getGHCupInfo :: IO (Either String GHCupInfo)
getGHCupInfo = do
  settings <- readIORef settings'

  r <-
    flip runReaderT settings
    . runE @'[DigestError, ContentLengthError, GPGError, JSONError , DownloadFailed , FileDoesNotExistError]
    $ liftE getDownloadsF

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
