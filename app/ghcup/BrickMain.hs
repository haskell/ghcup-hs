{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module BrickMain where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Utils
import           GHCup.Utils.File
import           GHCup.Utils.Logger

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.List             ( listSelectedFocusedAttr
                                                , listSelectedAttr
                                                , listAttr
                                                )
#if !defined(TAR)
import           Codec.Archive
#endif
import           Control.Exception.Safe
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Bool
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.IORef
import           Data.String.Interpolate
import           Data.Vector                    ( Vector
                                                , (!?)
                                                )
import           Data.Versions           hiding ( str )
import           Haskus.Utils.Variant.Excepts
import           Prelude                 hiding ( appendFile )
import           System.Exit
import           System.IO.Unsafe
import           URI.ByteString

import qualified Data.Text                     as T
import qualified Graphics.Vty                  as Vty
import qualified Data.Vector                   as V



data BrickData = BrickData
  { lr    :: [ListResult]
  , dls   :: GHCupDownloads
  , pfreq :: PlatformRequest
  }
  deriving Show

data BrickSettings = BrickSettings
  { showAll :: Bool
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
  }
  deriving Show


keyHandlers :: [ ( Char
                 , BrickSettings -> String
                 , BrickState -> EventM n (Next BrickState)
                 )
               ]
keyHandlers =
  [ ('q', const "Quit"     , halt)
  , ('i', const "Install"  , withIOAction install')
  , ('u', const "Uninstall", withIOAction del')
  , ('s', const "Set"      , withIOAction set')
  , ('c', const "ChangeLog", withIOAction changelog')
  , ( 'a'
    , (\BrickSettings {..} ->
        if showAll then "Hide old versions" else "Show all versions"
      )
    , hideShowHandler
    )
  ]
 where
  hideShowHandler (BrickState {..}) =
    let newAppSettings   = appSettings { showAll = not . showAll $ appSettings }
        newInternalState = constructList appData newAppSettings (Just appState)
    in  continue (BrickState appData newAppSettings newInternalState)


ui :: BrickState -> Widget String
ui BrickState { appSettings = as@(BrickSettings {}), ..}
  = ( padBottom Max
    $ ( withBorderStyle unicode
      $ borderWithLabel (str "GHCup")
      $ (center $ (header <=> hBorder <=> renderList' appState))
      )
    )
    <=> footer

 where
  footer =
    withAttr "help"
      . txtWrap
      . T.pack
      . foldr1 (\x y -> x <> "  " <> y)
      . (++ ["↑↓:Navigation"])
      $ (fmap (\(c, s, _) -> (c : ':' : s as)) keyHandlers)
  header =
    (minHSize 2 $ emptyWidget)
      <+> (padLeft (Pad 2) $ minHSize 6 $ str "Tool")
      <+> (minHSize 15 $ str "Version")
      <+> (padLeft (Pad 1) $ minHSize 25 $ str "Tags")
      <+> (padLeft (Pad 5) $ str "Notes")
  renderList' = withDefAttr listAttr . drawListElements renderItem True
  renderItem _ b listResult@(ListResult {..}) =
    let marks = if
          | lSet       -> (withAttr "set" $ str "✔✔")
          | lInstalled -> (withAttr "installed" $ str "✓ ")
          | otherwise  -> (withAttr "not-installed" $ str "✗ ")
        ver = case lCross of
          Nothing -> T.unpack . prettyVer $ lVer
          Just c  -> T.unpack (c <> "-" <> prettyVer lVer)
        dim = if lNoBindist
          then updateAttrMap (const dimAttributes) . withAttr "no-bindist"
          else id
        hooray
          | elem Latest lTag && not lInstalled =
              withAttr "hooray"
          | otherwise = id
        active = if b then forceAttr "active" else id
    in  hooray $ active $ dim
          (   marks
          <+> (( padLeft (Pad 2)
               $ minHSize 6
               $ (printTool lTool)
               )
              )
          <+> (minHSize 15 $ (str ver))
          <+> (let l = catMaybes . fmap printTag $ sort lTag
               in  padLeft (Pad 1) $ minHSize 25 $ if null l
                     then emptyWidget
                     else foldr1 (\x y -> x <+> str "," <+> y) l
              )
          <+> ( padLeft (Pad 5)
              $ let notes = printNotes listResult
                in  if null notes
                      then emptyWidget
                      else foldr1 (\x y -> x <+> str "," <+> y) $ notes
              )
          <+> (vLimit 1 $ fill ' ')
          )

  printTag Recommended    = Just $ withAttr "recommended" $ str "recommended"
  printTag Latest         = Just $ withAttr "latest" $ str "latest"
  printTag Prerelease     = Just $ withAttr "prerelease" $ str "prerelease"
  printTag (Base pvp'')   = Just $ str ("base-" ++ T.unpack (prettyPVP pvp''))
  printTag Old            = Nothing
  printTag (UnknownTag t) = Just $ str t

  printTool Cabal = str "cabal"
  printTool GHC = str "GHC"
  printTool GHCup = str "GHCup"
  printTool HLS = str "HLS"

  printNotes ListResult {..} =
    (if hlsPowered then [withAttr "hls-powered" $ str "hls-powered"] else mempty
      )
      ++ (if fromSrc then [withAttr "compiled" $ str "compiled"] else mempty)
      ++ (if lStray then [withAttr "stray" $ str "stray"] else mempty)

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
              makeVisible = if isSelected then visible . selItemAttr else id
          in  addSeparator $ makeVisible elemWidget

      in render
        $ viewport "GHCup" Vertical
        $ vBox
        $ V.toList drawnElements


minHSize :: Int -> Widget n -> Widget n
minHSize s' = hLimit s' . vLimit 1 . (<+> fill ' ')


app :: App BrickState e String
app = App { appDraw         = \st -> [ui st]
          , appHandleEvent  = eventHandler
          , appStartEvent   = return
          , appAttrMap      = const defaultAttributes
          , appChooseCursor = neverShowCursor
          }

defaultAttributes :: AttrMap
defaultAttributes = attrMap
  Vty.defAttr
  [ ("active"       , Vty.defAttr `Vty.withBackColor` Vty.blue)
  , ("not-installed", Vty.defAttr `Vty.withForeColor` Vty.red)
  , ("set"          , Vty.defAttr `Vty.withForeColor` Vty.green)
  , ("installed"    , Vty.defAttr `Vty.withForeColor` Vty.green)
  , ("recommended"  , Vty.defAttr `Vty.withForeColor` Vty.green)
  , ("hls-powered"  , Vty.defAttr `Vty.withForeColor` Vty.green)
  , ("latest"       , Vty.defAttr `Vty.withForeColor` Vty.yellow)
  , ("prerelease"   , Vty.defAttr `Vty.withForeColor` Vty.red)
  , ("compiled"     , Vty.defAttr `Vty.withForeColor` Vty.blue)
  , ("stray"        , Vty.defAttr `Vty.withForeColor` Vty.blue)
  , ("help"         , Vty.defAttr `Vty.withStyle` Vty.italic)
  , ("hooray"       , Vty.defAttr `Vty.withForeColor` Vty.brightWhite)
  ]


dimAttributes :: AttrMap
dimAttributes = attrMap
  (Vty.defAttr `Vty.withStyle` Vty.dim)
  [ ("active"    , Vty.defAttr `Vty.withBackColor` Vty.blue)
  , ("no-bindist", Vty.defAttr `Vty.withStyle` Vty.dim)
  ]

eventHandler :: BrickState -> BrickEvent n e -> EventM n (Next BrickState)
eventHandler st (VtyEvent (Vty.EvResize _               _)) = continue st
eventHandler st (VtyEvent (Vty.EvKey    (Vty.KChar 'q') _)) = halt st
eventHandler st (VtyEvent (Vty.EvKey    Vty.KEsc        _)) = halt st
eventHandler BrickState {..} (VtyEvent (Vty.EvKey (Vty.KUp) _)) =
  continue (BrickState { appState = (moveCursor appState Up), .. })
eventHandler BrickState {..} (VtyEvent (Vty.EvKey (Vty.KDown) _)) =
  continue (BrickState { appState = (moveCursor appState Down), .. })
eventHandler as (VtyEvent (Vty.EvKey (Vty.KChar c) _)) =
  case find (\(c', _, _) -> c' == c) keyHandlers of
    Nothing              -> continue as
    Just (_, _, handler) -> handler as
eventHandler st _ = continue st


moveCursor :: BrickInternalState -> Direction -> BrickInternalState
moveCursor ais@(BrickInternalState {..}) direction =
  let newIx = if direction == Down then ix + 1 else ix - 1
  in  case clr !? newIx of
        Just _  -> BrickInternalState { ix = newIx, .. }
        Nothing -> ais


-- | Suspend the current UI and run an IO action in terminal. If the
-- IO action returns a Left value, then it's thrown as userError.
withIOAction :: (BrickState -> (Int, ListResult) -> IO (Either String a))
             -> BrickState
             -> EventM n (Next BrickState)
withIOAction action as = case listSelectedElement' (appState as) of
  Nothing      -> continue as
  Just (ix, e) -> suspendAndResume $ do
    action as (ix, e) >>= \case
      Left  err -> putStrLn $ ("Error: " <> err)
      Right _   -> putStrLn "Success"
    getAppData Nothing (pfreq . appData $ as) >>= \case
      Right data' -> do
        putStrLn "Press enter to continue"
        _ <- getLine
        pure (updateList data' as)
      Left err -> throwIO $ userError err


-- | Update app data and list internal state based on new evidence.
-- This synchronises @BrickInternalState@ with @BrickData@
-- and @BrickSettings@.
updateList :: BrickData -> BrickState -> BrickState
updateList appD (BrickState {..}) =
  let newInternalState = constructList appD appSettings (Just appState)
  in  BrickState { appState    = newInternalState
               , appData     = appD
               , appSettings = appSettings
               }


constructList :: BrickData
              -> BrickSettings
              -> Maybe BrickInternalState
              -> BrickInternalState
constructList appD appSettings mapp =
  replaceLR (filterVisible (showAll appSettings)) (lr appD) mapp

listSelectedElement' :: BrickInternalState -> Maybe (Int, ListResult)
listSelectedElement' (BrickInternalState {..}) = fmap (ix, ) $ clr !? ix


selectLatest :: Vector ListResult -> Int
selectLatest v =
  case V.findIndex (\ListResult {..} -> lTool == GHC && Latest `elem` lTag) v of
    Just ix -> ix
    Nothing -> 0


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


filterVisible :: Bool -> ListResult -> Bool
filterVisible showAll e | lInstalled e = True
                        | showAll      = True
                        | otherwise    = not (elem Old (lTag e))


install' :: BrickState -> (Int, ListResult) -> IO (Either String ())
install' BrickState { appData = BrickData {..} } (_, ListResult {..}) = do
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  let run =
        runLogger
          . flip runReaderT settings
          . runResourceT
          . runE
            @'[ AlreadyInstalled
#if !defined(TAR)
              , ArchiveResult
#endif
              , UnknownArchive
              , FileDoesNotExistError
              , CopyError
              , NoDownload
              , NotInstalled
              , BuildFailed
              , TagNotFound
              , DigestError
              , DownloadFailed
              , NoUpdate
              , TarDirDoesNotExist
              ]

  (run $ do
      case lTool of
        GHC   -> liftE $ installGHCBin dls lVer pfreq
        Cabal -> liftE $ installCabalBin dls lVer pfreq
        GHCup -> liftE $ upgradeGHCup dls Nothing False pfreq $> ()
        HLS   -> liftE $ installHLSBin dls lVer pfreq $> ()
    )
    >>= \case
          VRight _                          -> pure $ Right ()
          VLeft  (V (AlreadyInstalled _ _)) -> pure $ Right ()
          VLeft (V (BuildFailed _ e)) ->
            pure $ Left [i|Build failed with #{e}|]
          VLeft (V NoDownload) ->
            pure $ Left [i|No available version for #{prettyVer lVer}|]
          VLeft (V NoUpdate) -> pure $ Right ()
          VLeft e -> pure $ Left [i|#{e}
Also check the logs in ~/.ghcup/logs|]


set' :: BrickState -> (Int, ListResult) -> IO (Either String ())
set' _ (_, ListResult {..}) = do
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  let run =
        runLogger
          . flip runReaderT settings
          . runE @'[FileDoesNotExistError , NotInstalled , TagNotFound]

  (run $ do
      case lTool of
        GHC   -> liftE $ setGHC (GHCTargetVersion lCross lVer) SetGHCOnly $> ()
        Cabal -> liftE $ setCabal lVer $> ()
        HLS   -> liftE $ setHLS lVer $> ()
        GHCup -> pure ()
    )
    >>= \case
          VRight _ -> pure $ Right ()
          VLeft  e -> pure $ Left [i|#{e}|]


del' :: BrickState -> (Int, ListResult) -> IO (Either String ())
del' _ (_, ListResult {..}) = do
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  let run = runLogger . flip runReaderT settings . runE @'[NotInstalled]

  (run $ do
      case lTool of
        GHC   -> liftE $ rmGHCVer (GHCTargetVersion lCross lVer) $> ()
        Cabal -> liftE $ rmCabalVer lVer $> ()
        HLS   -> liftE $ rmHLSVer lVer $> ()
        GHCup -> pure ()
    )
    >>= \case
          VRight _ -> pure $ Right ()
          VLeft  e -> pure $ Left [i|#{e}|]


changelog' :: BrickState -> (Int, ListResult) -> IO (Either String ())
changelog' BrickState { appData = BrickData {..} } (_, ListResult {..}) = do
  case getChangeLog dls lTool (Left lVer) of
    Nothing -> pure $ Left
      [i|Could not find ChangeLog for #{lTool}, version #{prettyVer lVer}|]
    Just uri -> do
      let cmd = case _rPlatform pfreq of
            Darwin  -> "open"
            Linux _ -> "xdg-open"
            FreeBSD -> "xdg-open"
      exec cmd True [serializeURIRef' uri] Nothing Nothing >>= \case
        Right _ -> pure $ Right ()
        Left  e -> pure $ Left [i|#{e}|]


uri' :: IORef (Maybe URI)
{-# NOINLINE uri' #-}
uri' = unsafePerformIO (newIORef Nothing)


settings' :: IORef AppState
{-# NOINLINE settings' #-}
settings' = unsafePerformIO $ do
  dirs <- getDirs
  newIORef $ AppState (Settings { cache      = True
                                , noVerify   = False
                                , keepDirs   = Never
                                , downloader = Curl
                                , verbose    = False
                                , ..
                                })
                      dirs



logger' :: IORef LoggerConfig
{-# NOINLINE logger' #-}
logger' = unsafePerformIO
  (newIORef $ LoggerConfig { lcPrintDebug = False
                           , colorOutter  = \_ -> pure ()
                           , rawOutter    = \_ -> pure ()
                           }
  )


brickMain :: AppState
          -> Maybe URI
          -> LoggerConfig
          -> GHCupDownloads
          -> PlatformRequest
          -> IO ()
brickMain s muri l av pfreq' = do
  writeIORef uri'      muri
  writeIORef settings' s
  -- logger interpreter
  writeIORef logger'   l
  let runLogger = myLoggerT l

  eAppData <- getAppData (Just av) pfreq'
  case eAppData of
    Right ad ->
      defaultMain
          app
          (BrickState ad
                    defaultAppSettings
                    (constructList ad defaultAppSettings Nothing)
          )
        $> ()
    Left e -> do
      runLogger ($(logError) [i|Error building app state: #{show e}|])
      exitWith $ ExitFailure 2


defaultAppSettings :: BrickSettings
defaultAppSettings = BrickSettings { showAll = False }


getDownloads' :: IO (Either String GHCupDownloads)
getDownloads' = do
  muri     <- readIORef uri'
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  r <-
    runLogger
    . flip runReaderT settings
    . runE @'[JSONError , DownloadFailed , FileDoesNotExistError]
    $ fmap _ghcupDownloads
    $ liftE
    $ getDownloadsF (maybe GHCupURL OwnSource muri)

  case r of
    VRight a -> pure $ Right a
    VLeft  e -> pure $ Left [i|#{e}|]


getAppData :: Maybe GHCupDownloads
           -> PlatformRequest
           -> IO (Either String BrickData)
getAppData mg pfreq' = do
  settings <- readIORef settings'
  l        <- readIORef logger'
  let runLogger = myLoggerT l

  r <- maybe getDownloads' (pure . Right) mg

  runLogger . flip runReaderT settings $ do
    case r of
      Right dls -> do
        lV <- listVersions dls Nothing Nothing pfreq'
        pure $ Right $ (BrickData (reverse lV) dls pfreq')
      Left e -> pure $ Left [i|#{e}|]

