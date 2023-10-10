{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell   #-}

module BrickMain where

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

import Brick
    ( BrickEvent(VtyEvent, MouseDown),
      App(..),
      Padding(Max, Pad),
      AttrMap,
      EventM,
      Size(..),
      Named(..),
      Widget(..), 
      ViewportType (Vertical), 
      (<+>),
      (<=>))
import qualified Brick
import           Brick.Widgets.Border ( hBorder, borderWithLabel)
import           Brick.Widgets.Border.Style ( unicode )
import           Brick.Widgets.Center ( center, centerLayer )
import qualified Brick.Widgets.List as L
import           Brick.Focus (FocusRing)
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
import           Data.IORef (IORef, readIORef, newIORef, writeIORef, modifyIORef)
import           Data.Vector                    ( Vector
                                                
                                                )
import           Data.Versions
import           Haskus.Utils.Variant.Excepts
import           Prelude                 hiding ( appendFile )
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
#if !IS_WINDOWS
import           GHCup.Prelude.File
import           System.FilePath
import qualified System.Posix.Process          as SPP
#endif

import           Optics.TH (makeLenses, makeLensesFor)
import           Optics.State (use)
import           Optics.State.Operators ( (.=), (%=), (<%=))
import           Optics.Operators ((.~), (^.), (%~))
import           Optics.Getter (view)
import           Optics.Lens (Lens', lens, toLensVL)

{- Brick's widget:
It is a FocusRing over many list's. Each list contains the information for each tool. Each list has an internal name (for Brick's runtime)
and a label which we can use in rendering. This data-structure helps to reuse Brick.Widget.List and to navegate easily across

Consider this code as private. GenericSectionList should not be used directly as the FocusRing should be align with the Vector containing 
the elements, otherwise you'd be focusing on a non-existent widget with unknown result (In theory the code is safe unless you have an empty section list). 

- To build a SectionList use the safe constructor sectionList
- To access sections use the lens provider sectionL and the name of the section you'd like to access
- You can modify Brick.Widget.List.GenericList within GenericSectionList via sectionL but do not
  modify the vector length

-}

data GenericSectionList n t e
    = GenericSectionList
    { sectionListFocusRing :: FocusRing n                   -- ^ The FocusRing for all sections
    , sectionListElements :: !(Vector (L.GenericList n t e)) -- ^ A key-value vector
    , sectionListName :: n                                   -- ^ The section list name
    }

makeLensesFor [("sectionListFocusRing", "sectionListFocusRingL"), ("sectionListElements", "sectionListElementsL"), ("sectionListName", "sectionListNameL")] ''GenericSectionList

type SectionList n e = GenericSectionList n V.Vector e

instance Named (GenericSectionList n t e) n where
    getName = sectionListName


-- | Build a SectionList from nonempty list. If empty we could not defined sectionL lenses. 
sectionList :: Foldable t 
            => n                     -- The name of the section list
            -> [(n, t e)]            -- a list of tuples (section name, collection of elements)
            -> Int
            -> GenericSectionList n t e
sectionList name elements height
  = GenericSectionList
  { sectionListFocusRing = F.focusRing [section_name | (section_name, _) <- elements]
  , sectionListElements  = V.fromList [L.list section_name els height | (section_name, els) <- elements]
  , sectionListName = name
  }
-- | This lens constructor, takes a name and looks if a section has such a name.
--   Used to dispatch events to sections. It is a partial function only meant to 
--   be used with the FocusRing inside GenericSectionList
sectionL :: Eq n => n -> Lens' (GenericSectionList n t e) (L.GenericList n t e)
sectionL section_name = lens g s
    where is_section_name = (== section_name) . L.listName
          g section_list =
            let elms   = section_list ^. sectionListElementsL
                zeroth = elms V.! 0 -- TODO: This crashes for empty vectors. 
            in fromMaybe zeroth (V.find is_section_name elms)
          s gl@(GenericSectionList _ elms _) list =
            case V.findIndex is_section_name elms of
                 Nothing -> gl
                 Just i  -> let new_elms = V.update elms (V.fromList [(i, list)])
                             in gl & sectionListElementsL .~ new_elms

moveDown :: (L.Splittable t, Ord n, Foldable t) => EventM n (GenericSectionList n t e) ()
moveDown = do 
    ring <- use sectionListFocusRingL
    case F.focusGetCurrent ring of 
        Nothing -> pure ()
        Just l  -> do      -- If it is the last element, move to the first element of the next focus; else, just handle regular list event.
            current_list <- use (sectionL l)
            let current_idx = L.listSelected current_list
                list_length = current_list & length
            if current_idx == Just (list_length - 1)
                then do 
                    new_focus <- sectionListFocusRingL <%= F.focusNext
                    case F.focusGetCurrent new_focus of
                        Nothing -> pure () -- |- Optic.Zoom.zoom doesn't typecheck but Lens.Micro.Mtl.zoom does. It is re-exported by Brick
                        Just new_l -> Brick.zoom (toLensVL $ sectionL new_l) (Brick.modify L.listMoveToBeginning)
                else Brick.zoom (toLensVL $ sectionL l) $ Brick.modify L.listMoveDown

moveUp :: (L.Splittable t, Ord n, Foldable t) => EventM n (GenericSectionList n t e) ()
moveUp = do
    ring <- use sectionListFocusRingL
    case F.focusGetCurrent ring of
        Nothing -> pure ()
        Just l  -> do  -- If it is the first element, move to the last element of the prev focus; else, just handle regular list event.
            current_list <- use (sectionL l)
            let current_idx = L.listSelected current_list
            if current_idx == Just 0
                then do 
                    new_focus <- sectionListFocusRingL <%= F.focusPrev
                    case F.focusGetCurrent new_focus of
                        Nothing -> pure ()  
                        Just new_l -> Brick.zoom (toLensVL $ sectionL new_l) (Brick.modify L.listMoveToEnd)
                else Brick.zoom (toLensVL $ sectionL l) $ Brick.modify L.listMoveUp

-- | Handle events for list cursor movement.  Events handled are:
--
-- * Up (up arrow key). If first element of section, then jump prev section
-- * Down (down arrow key). If last element of section, then jump next section
-- * Page Up (PgUp)
-- * Page Down (PgDown)
-- * Go to next section (Tab)
-- * Go to prev section (BackTab)
handleGenericListEvent :: (Foldable t, L.Splittable t, Ord n)
                       => BrickEvent n a
                       -> EventM n (GenericSectionList n t e) ()
handleGenericListEvent (VtyEvent (Vty.EvResize _ _))              = pure ()
handleGenericListEvent (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = sectionListFocusRingL %= F.focusNext
handleGenericListEvent (VtyEvent (Vty.EvKey Vty.KBackTab []))     = sectionListFocusRingL %= F.focusPrev
handleGenericListEvent (MouseDown _ Vty.BScrollDown _ _)          = moveDown
handleGenericListEvent (MouseDown _ Vty.BScrollUp _ _)            = moveUp
handleGenericListEvent (VtyEvent (Vty.EvKey Vty.KDown []))     = moveDown
handleGenericListEvent (VtyEvent (Vty.EvKey Vty.KUp []))       = moveUp
handleGenericListEvent (VtyEvent ev) = do
    ring <- use sectionListFocusRingL
    case F.focusGetCurrent ring of
        Nothing -> pure ()
        Just l  -> Brick.zoom (toLensVL $ sectionL l) $ L.handleListEvent ev
handleGenericListEvent _ = pure ()

-- This re-uses Brick.Widget.List.renderList
renderSectionList :: (Traversable t, Ord n, Show n, Eq n, L.Splittable t)
                  => (Bool -> n -> Widget n)             -- ^ Rendering function for separator between sections. True for selected before section
                  -> (Bool -> n -> Widget n -> Widget n) -- ^ Rendering function for the borders. True for selected section
                  -> (Bool -> e -> Widget n)             -- ^ Rendering function of the list element, True for the selected element
                  -> Bool                                -- ^ Whether the section list has focus
                  -> GenericSectionList n t e            -- ^ The section list to render
                  -> Widget n
renderSectionList render_separator render_border render_elem section_focus (GenericSectionList focus elms sl_name) =
  Widget Greedy Greedy $ render $
    Brick.viewport sl_name Vertical $
      V.foldl' (\wacc list ->
                  let has_focus_list = is_focused_section list
                      list_name = L.listName list
                  in   wacc
                    <=> render_separator has_focus_list list_name
                    <=> inner_widget has_focus_list list_name list
              )
              Brick.emptyWidget elms
  where
    is_focused_section l = section_focus && Just (L.listName l) == F.focusGetCurrent focus
    inner_widget has_focus k l = 
      let w = render_border has_focus k (Brick.vLimit (length l) $ L.renderList render_elem has_focus l)
       in if has_focus then Brick.visible w else w


-- | Equivalent to listSelectedElement
sectionListSelectedElement :: (Eq n, L.Splittable t, Traversable t, Semigroup (t e)) => GenericSectionList n t e -> Maybe (Int, e)
sectionListSelectedElement generic_section_list = do
  current_focus <- generic_section_list ^. sectionListFocusRingL & F.focusGetCurrent 
  let current_section = generic_section_list ^. sectionL current_focus
  L.listSelectedElement current_section 

{- GHCUp State

-}

data Name = AllTools       -- The main list widget
          | Singular Tool  -- The particular list for each tool
          | IODialog       -- The pop up when installing a tool
          | TutorialBox    -- The tutorial widget
          deriving (Eq, Ord, Show)

data Mode = Navigation | Tutorial deriving (Eq, Show, Ord)

installedSign :: String
#if IS_WINDOWS
installedSign = "I "
#else
installedSign = "✓ "
#endif

setSign :: String
#if IS_WINDOWS
setSign = "IS"
#else
setSign = "✔✔"
#endif

notInstalledSign :: String
#if IS_WINDOWS
notInstalledSign = "X "
#else
notInstalledSign = "✗ "
#endif


data BrickData = BrickData
  { _lr    :: [ListResult]
  }
  deriving Show

makeLenses ''BrickData

data BrickSettings = BrickSettings
  { _showAllVersions    :: Bool
  , _showAllTools       :: Bool
  }
  --deriving Show

makeLenses ''BrickSettings

type BrickInternalState = SectionList Name ListResult

data BrickState = BrickState
  { _appData     :: BrickData
  , _appSettings :: BrickSettings
  , _appState    :: BrickInternalState
  , _appKeys     :: KeyBindings
  , _mode        :: Mode
  }
  --deriving Show

makeLenses ''BrickState

keyHandlers :: KeyBindings
            -> [ ( KeyCombination
                 , BrickSettings -> String
                 , EventM Name BrickState ()
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
    , hideShowHandler' (not . _showAllVersions) _showAllTools
    )
  , (bUp, const "Up", appState %= moveCursor 1 Up)
  , (bDown, const "Down", appState %= moveCursor 1 Down)
  , (Vty.KChar 'x', const "Tutorial", mode .= Tutorial)
  ]
 where
  --hideShowHandler' :: (BrickSettings -> Bool) -> (BrickSettings -> Bool) -> m ()
  hideShowHandler' f p = do 
    app_settings <- use appSettings
    let 
      vers = f app_settings
      tools = p app_settings
      newAppSettings = app_settings & showAllVersions .~ vers & showAllTools .~ tools
    ad <- use appData
    current_app_state <- use appState
    appSettings .= newAppSettings
    appState    .= constructList ad newAppSettings (Just current_app_state)



showKey :: Vty.Key -> String
showKey (Vty.KChar c) = [c]
showKey Vty.KUp = "↑"
showKey Vty.KDown = "↓"
showKey key = tail (show key)

showMod :: Vty.Modifier -> String
showMod = tail . show


ui :: AttrMap -> BrickState -> Widget Name
ui dimAttrs BrickState{ _appSettings = as@BrickSettings{}, ..}
  = Brick.padBottom Max
      ( Brick.withBorderStyle unicode
        $ borderWithLabel (Brick.str "GHCup")
          (center (header <=> renderList' _appState))
      )
    <=> footer
 where
  footer =
    Brick.withAttr (Brick.attrName "help")
      . Brick.txtWrap
      . T.pack
      . foldr1 (\x y -> x <> "  " <> y)
      . fmap (\(KeyCombination key mods, s, _) -> intercalate "+" (showKey key : (showMod <$> mods)) <> ":" <> s as)
      $ keyHandlers _appKeys
  header =
    minHSize 2 Brick.emptyWidget
      <+> Brick.padLeft (Pad 2) (minHSize 6 $ Brick.str "Tool")
      <+> minHSize 15 (Brick.str "Version")
      <+> Brick.padLeft (Pad 1) (minHSize 25 $ Brick.str "Tags")
      <+> Brick.padLeft (Pad 5) (Brick.str "Notes")
  renderList' bis = 
    let allElements = V.concatMap L.listElements $ sectionListElements bis
        minTagSize = V.maximum $ V.map (length . intercalate "," . fmap tagToString . lTag) allElements
        minVerSize = V.maximum $ V.map (\ListResult{..} -> T.length $ tVerToText (GHCTargetVersion lCross lVer)) allElements
        render_separator _ _ = hBorder
        render_border _ _ = id
    in Brick.withDefAttr L.listAttr $ renderSectionList render_separator render_border (renderItem minTagSize minVerSize) True bis
  renderItem minTagSize minVerSize b listResult@ListResult{lTag = lTag', ..} =
    let marks = if
          | lSet       -> (Brick.withAttr (Brick.attrName "set") $ str setSign)
          | lInstalled -> (Brick.withAttr (Brick.attrName "installed") $ str installedSign)
          | otherwise  -> (Brick.withAttr (Brick.attrName "not-installed") $ str notInstalledSign)
        ver = case lCross of
          Nothing -> T.unpack . prettyVer $ lVer
          Just c  -> T.unpack (c <> "-" <> prettyVer lVer)
        dim
          | lNoBindist && not lInstalled
            && not b -- TODO: overloading dim and active ignores active
                       --       so we hack around it here
          = Brick.updateAttrMap (const dimAttrs) . Brick.withAttr (Brick.attrName "no-bindist")
          | otherwise  = id
        hooray
          | elem Latest lTag' && not lInstalled =
              Brick.withAttr (Brick.attrName "hooray")
          | otherwise = id
    in  hooray $ dim
          (   marks
          <+> Brick.padLeft (Pad 2)
               ( minHSize 6
                 (printTool lTool)
               )
          <+> minHSize minVerSize (Brick.str ver)
          <+> (let l = catMaybes . fmap printTag $ sort lTag'
               in  Brick.padLeft (Pad 1) $ minHSize minTagSize $ if null l
                     then Brick.emptyWidget
                     else foldr1 (\x y -> x <+> Brick.str "," <+> y) l
              )
          <+> Brick.padLeft (Pad 5)
              ( let notes = printNotes listResult
                in  if null notes
                      then Brick.emptyWidget
                      else foldr1 (\x y -> x <+> Brick.str "," <+> y) notes
              )
          <+> Brick.vLimit 1 (Brick.fill ' ')
          )

  printTag Recommended    = Just $ Brick.withAttr (Brick.attrName "recommended") $ Brick.str "recommended"
  printTag Latest         = Just $ Brick.withAttr (Brick.attrName "latest") $ Brick.str "latest"
  printTag Prerelease     = Just $ Brick.withAttr (Brick.attrName "prerelease") $ Brick.str "prerelease"
  printTag Nightly        = Just $ Brick.withAttr (Brick.attrName "nightly") $ Brick.str "nightly"
  printTag (Base pvp'')   = Just $ Brick.str ("base-" ++ T.unpack (prettyPVP pvp''))
  printTag Old            = Nothing
  printTag LatestPrerelease = Just $ Brick.withAttr (Brick.attrName "latest-prerelease") $ Brick.str "latest-prerelease"
  printTag LatestNightly    = Just $ Brick.withAttr (Brick.attrName "latest-nightly") $ Brick.str "latest-nightly"
  printTag (UnknownTag t) = Just $ Brick.str t

  printTool Cabal = Brick.str "cabal"
  printTool GHC = Brick.str "GHC"
  printTool GHCup = Brick.str "GHCup"
  printTool HLS = Brick.str "HLS"
  printTool Stack = Brick.str "Stack"

  printNotes ListResult {..} =
    (if hlsPowered then [Brick.withAttr (Brick.attrName "hls-powered") $ Brick.str "hls-powered"] else mempty
      )
      ++ (if lStray then [Brick.withAttr (Brick.attrName "stray") $ Brick.str "stray"] else mempty)
      ++ (case lReleaseDay of
            Nothing -> mempty
            Just d  -> [Brick.withAttr (Brick.attrName "day") $ Brick.str (show d)])

minHSize :: Int -> Widget n -> Widget n
minHSize s' = Brick.hLimit s' . Brick.vLimit 1 . (<+> Brick.fill ' ')

app :: AttrMap -> AttrMap -> App BrickState () Name
app attrs dimAttrs =
  App { appDraw         = drawUI dimAttrs
      , appHandleEvent  = eventHandler
      , appStartEvent   = return ()
      , appAttrMap      = const attrs
      , appChooseCursor = Brick.showFirstCursor
      }


drawUI :: AttrMap -> BrickState -> [Widget Name]
drawUI dimAttrs st = 
  case st ^. mode of
    Navigation -> [ui dimAttrs st]
    Tutorial   -> 
      let tutorial = centerLayer
                   $ Brick.hLimitPercent 75
                   $ Brick.vLimitPercent 50
                   $ Brick.withBorderStyle unicode
                   $ borderWithLabel (Brick.txt "Tutorial")
                   $ Brick.vBox 
                     [ Brick.txt "GHCup is a tool for managing your Haskell tooling."
                     , center (Brick.txt "--- o ---")
                     , Brick.txt "This symbol "
                        <+> Brick.withAttr (Brick.attrName "installed") (Brick.str "✓ ")
                        <+> Brick.txt " means that the tool is installed but not in used"
                     , Brick.txt "This symbol "
                        <+> Brick.withAttr (Brick.attrName "set") (Brick.str "✔✔") 
                        <+> Brick.txt " means that the tool is installed and in used"
                     , Brick.txt "This symbol "
                        <+> Brick.withAttr (Brick.attrName "not-installed") (Brick.str "✗ ")
                        <+> Brick.txt " means that the tool isn't installed"
                     , center (Brick.txt "--- o ---")
                     , Brick.txt "Press Enter to exit the tutorial"
                     ]
       in [tutorial, ui dimAttrs st]


defaultAttributes :: Bool -> AttrMap
defaultAttributes no_color = Brick.attrMap
  Vty.defAttr
  [ (L.listSelectedFocusedAttr          , Vty.defAttr `withBackColor` Vty.blue)  --, (attrName "active"            , Vty.defAttr `withBackColor` Vty.blue)
  , (L.listSelectedAttr                 , Vty.defAttr)                           -- we use list attributes for active.
  , (Brick.attrName "not-installed"     , Vty.defAttr `withForeColor` Vty.red)
  , (Brick.attrName "set"               , Vty.defAttr `withForeColor` Vty.green)
  , (Brick.attrName "installed"         , Vty.defAttr `withForeColor` Vty.green)
  , (Brick.attrName "recommended"       , Vty.defAttr `withForeColor` Vty.green)
  , (Brick.attrName "hls-powered"       , Vty.defAttr `withForeColor` Vty.green)
  , (Brick.attrName "latest"            , Vty.defAttr `withForeColor` Vty.yellow)
  , (Brick.attrName "latest-prerelease" , Vty.defAttr `withForeColor` Vty.red)
  , (Brick.attrName "latest-nightly"    , Vty.defAttr `withForeColor` Vty.red)
  , (Brick.attrName "prerelease"        , Vty.defAttr `withForeColor` Vty.red)
  , (Brick.attrName "nightly"           , Vty.defAttr `withForeColor` Vty.red)
  , (Brick.attrName "compiled"          , Vty.defAttr `withForeColor` Vty.blue)
  , (Brick.attrName "stray"             , Vty.defAttr `withForeColor` Vty.blue)
  , (Brick.attrName "day"               , Vty.defAttr `withForeColor` Vty.blue)
  , (Brick.attrName "help"              , Vty.defAttr `withStyle`     Vty.italic)
  , (Brick.attrName "hooray"            , Vty.defAttr `withForeColor` Vty.brightWhite)
  ]
  where
    withForeColor | no_color  = const
                  | otherwise = Vty.withForeColor

    withBackColor | no_color  = \attr _ -> attr `Vty.withStyle` Vty.reverseVideo
                  | otherwise = Vty.withBackColor

    withStyle                 = Vty.withStyle

dimAttributes :: Bool -> AttrMap
dimAttributes no_color = Brick.attrMap
  (Vty.defAttr `Vty.withStyle` Vty.dim)
  [ (Brick.attrName "active"    , Vty.defAttr `withBackColor` Vty.blue) -- has no effect ??
  , (Brick.attrName "no-bindist", Vty.defAttr `Vty.withStyle` Vty.dim)
  ]
  where
    withBackColor | no_color  = \attr _ -> attr `Vty.withStyle` Vty.reverseVideo
                  | otherwise = Vty.withBackColor

tutorialHandler :: BrickEvent Name e -> EventM Name BrickState ()
tutorialHandler ev = 
  case ev of
    VtyEvent (Vty.EvKey Vty.KEnter _) -> mode .= Navigation
    _ -> pure ()

navigationHandler :: BrickEvent Name e -> EventM Name BrickState ()
navigationHandler ev = do
  AppState { keyBindings = kb } <- liftIO $ readIORef settings'
  case ev of
    inner_event@(VtyEvent (Vty.EvKey key _)) ->
      case find (\(key', _, _) -> key' == key) (keyHandlers kb) of
        Nothing -> void $ Brick.zoom (toLensVL appState) $ handleGenericListEvent inner_event
        Just (_, _, handler) -> handler
    inner_event -> Brick.zoom (toLensVL appState) $ handleGenericListEvent inner_event

eventHandler :: BrickEvent Name e -> EventM Name BrickState ()
eventHandler ev = do
  m <- use mode
  case m of
    Navigation -> navigationHandler ev
    Tutorial   -> tutorialHandler ev

-- | Suspend the current UI and run an IO action in terminal. If the
-- IO action returns a Left value, then it's thrown as userError.
withIOAction :: (Ord n, Eq n)
             => ( (Int, ListResult) -> ReaderT AppState IO (Either String a))
             -> EventM n BrickState ()
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
  replaceLR (filterVisible (_showAllVersions settings)
                           (_showAllTools settings))
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
      newSectionList = sectionList AllTools newVec 0
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


install' :: (MonadReader AppState m, MonadIO m, MonadThrow m, MonadFail m, MonadMask m, MonadUnliftIO m)
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
                      (GHCupInfo mempty mempty Nothing)
                      (PlatformRequest A_64 Darwin Nothing)
                      loggerConfig


brickMain :: AppState
          -> IO ()
brickMain s = do
  writeIORef settings' s

  eAppData <- getAppData (Just $ ghcupInfo s)
  case eAppData of
    Right ad ->
      Brick.defaultMain
          (app (defaultAttributes (noColor $ settings s)) (dimAttributes (noColor $ settings s)))
          (BrickState ad
                    defaultAppSettings
                    (constructList ad defaultAppSettings Nothing)
                    (keyBindings (s :: AppState))
                    Navigation

          )
        $> ()
    Left e -> do
      flip runReaderT s $ logError $ "Error building app state: " <> T.pack (show e)
      exitWith $ ExitFailure 2


defaultAppSettings :: BrickSettings
defaultAppSettings = BrickSettings { _showAllVersions = False, _showAllTools = False }


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
