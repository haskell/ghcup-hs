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
    ( defaultMain,
      suspendAndResume,
      attrMap,
      showFirstCursor,
      hLimit,
      vBox,
      viewport,
      visible,
      fill,
      vLimit,
      forceAttr,
      putCursor,
      updateAttrMap,
      withDefAttr,
      padLeft,
      (<+>),
      emptyWidget,
      txtWrap,
      attrName,
      withAttr,
      (<=>),
      str,
      withBorderStyle,
      padBottom,
      halt,
      BrickEvent(VtyEvent, MouseDown),
      App(..),
      ViewportType(Vertical),
      Size(Greedy),
      Location(Location),
      Padding(Max, Pad),
      Widget(Widget, render),
      AttrMap,
      Direction(..),
      get,
      zoom, 
      EventM,
      suffixLenses,
      Named(..), modify )
import           Brick.Widgets.Border ( hBorder, borderWithLabel )
import           Brick.Widgets.Border.Style ( unicode )
import           Brick.Widgets.Center ( center )
import           Brick.Widgets.Dialog (buttonSelectedAttr)
import           Brick.Widgets.List             ( listSelectedFocusedAttr
                                                , listSelectedAttr
                                                , listAttr
                                                )
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
import           Data.Function ( (&))
import           Data.List
import           Data.Maybe
import           Data.IORef (IORef, readIORef, newIORef, writeIORef, modifyIORef)
import           Data.Vector                    ( Vector
                                                , (!?)
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
import           Optics.Optic ((%))
import           Optics.Operators ((.~), (^.))
import           Optics.Getter (view)
import           Optics.Lens (Lens', lens, toLensVL, lensVL)

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

-- | Handle events for list cursor movement.  Events handled are:
--
-- * Up (up arrow key). If first element of section, then jump prev section
-- * Down (down arrow key). If last element of section, then jump next section
-- * Page Up (PgUp)
-- * Page Down (PgDown)
-- * Go to next section (Tab)
-- * Go to prev section (BackTab)
handleGenericListEvent :: (Foldable t, L.Splittable t, Ord n)
                       => BrickEvent n ()
                       -> EventM n (GenericSectionList n t e) ()
handleGenericListEvent (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = sectionListFocusRingL %= F.focusNext
handleGenericListEvent (VtyEvent (Vty.EvKey Vty.KBackTab []))     = sectionListFocusRingL %= F.focusPrev
handleGenericListEvent (VtyEvent ev@(Vty.EvKey Vty.KDown [])) = do
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
                        Just new_l -> zoom (toLensVL $ sectionL new_l) (modify L.listMoveToBeginning)
                else zoom (toLensVL $ sectionL l) $ L.handleListEvent ev
handleGenericListEvent (VtyEvent ev@(Vty.EvKey Vty.KUp [])) = do
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
                        Just new_l -> zoom (toLensVL $ sectionL new_l) (modify L.listMoveToEnd)
                else zoom (toLensVL $ sectionL l) $ L.handleListEvent ev
handleGenericListEvent (VtyEvent ev) = do
    ring <- use sectionListFocusRingL
    case F.focusGetCurrent ring of
        Nothing -> pure ()
        Just l  -> zoom (toLensVL $ sectionL l) $ L.handleListEvent ev
handleGenericListEvent _ = pure ()

-- This re-uses Brick.Widget.List.renderList
renderSectionList :: (Traversable t, Ord n, Show n, Eq n, L.Splittable t)
                  => (Bool -> n -> Widget n)             -- ^ Rendering function for separator between sections. True for selected before section
                  -> (Bool -> n -> Widget n -> Widget n) -- ^ Rendering function for the borders. True for selected section
                  -> (Bool -> e -> Widget n)             -- ^ Rendering function of the list element, True for the selected element
                  -> Bool                                -- ^ Whether the section list has focus
                  -> GenericSectionList n t e            -- ^ The section list to render
                  -> Widget n
renderSectionList render_separator render_border render_elem section_focus (GenericSectionList focus elms _) =
    V.foldl' (\wacc list ->
                let has_focus = is_focused_section list
                    list_name = L.listName list
                 in   wacc
                  <=> render_separator has_focus list_name
                  <=> inner_widget has_focus list_name list
             )
             emptyWidget elms
  where
    is_focused_section l = section_focus && Just (L.listName l) == F.focusGetCurrent focus
    inner_widget has_focus k l = render_border has_focus k (L.renderList render_elem has_focus l)


-- | Equivalent to listSelectedElement
sectionListSelectedElement :: (Eq n, L.Splittable t, Traversable t, Semigroup (t e)) => GenericSectionList n t e -> Maybe (Int, e)
sectionListSelectedElement generic_section_list = do
  current_focus <- generic_section_list ^. sectionListFocusRingL & F.focusGetCurrent 
  let current_section = generic_section_list ^. sectionL current_focus
  L.listSelectedElement current_section 

{- GHCUp State

-}


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

data BrickInternalState = BrickInternalState
  { _clr   :: Vector ListResult
  , _ix    :: Int
  }
  --deriving Show

makeLenses ''BrickInternalState

data BrickState = BrickState
  { _appData     :: BrickData
  , _appSettings :: BrickSettings
  , _appState    :: BrickInternalState
  , _appKeys     :: KeyBindings
  }
  --deriving Show

makeLenses ''BrickState

keyHandlers :: KeyBindings
            -> [ ( KeyCombination
                 , BrickSettings -> String
                 , EventM String BrickState ()
                 )
               ]
keyHandlers KeyBindings {..} =
  [ (bQuit, const "Quit"     , halt)
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
    appState    .= constructList ad app_settings (Just current_app_state)



showKey :: Vty.Key -> String
showKey (Vty.KChar c) = [c]
showKey Vty.KUp = "↑"
showKey Vty.KDown = "↓"
showKey key = tail (show key)

showMod :: Vty.Modifier -> String
showMod = tail . show


ui :: AttrMap -> BrickState -> Widget String
ui dimAttrs BrickState{ _appSettings = as@BrickSettings{}, ..}
  = padBottom Max
      ( withBorderStyle unicode
        $ borderWithLabel (str "GHCup")
          (center (header <=> hBorder <=> renderList' _appState))
      )
    <=> footer

 where
  footer =
    withAttr (attrName "help")
      . txtWrap
      . T.pack
      . foldr1 (\x y -> x <> "  " <> y)
      . fmap (\(KeyCombination key mods, s, _) -> intercalate "+" (showKey key : (showMod <$> mods)) <> ":" <> s as)
      $ keyHandlers _appKeys
  header =
    minHSize 2 emptyWidget
      <+> padLeft (Pad 2) (minHSize 6 $ str "Tool")
      <+> minHSize 15 (str "Version")
      <+> padLeft (Pad 1) (minHSize 25 $ str "Tags")
      <+> padLeft (Pad 5) (str "Notes")
  renderList' bis@BrickInternalState{..} =
    let minTagSize = V.maximum $ V.map (length . intercalate "," . fmap tagToString . lTag) _clr
        minVerSize = V.maximum $ V.map (\ListResult{..} -> T.length $ tVerToText (GHCTargetVersion lCross lVer)) _clr
    in withDefAttr listAttr . drawListElements (renderItem minTagSize minVerSize) True $ bis
  renderItem minTagSize minVerSize _ b listResult@ListResult{lTag = lTag', ..} =
    let marks = if
          | lSet       -> (withAttr (attrName "set") $ str setSign)
          | lInstalled -> (withAttr (attrName "installed") $ str installedSign)
          | otherwise  -> (withAttr (attrName "not-installed") $ str notInstalledSign)
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

app :: AttrMap -> AttrMap -> App BrickState () String
app attrs dimAttrs =
  App { appDraw     = \st -> [ui dimAttrs st]
  , appHandleEvent  = eventHandler
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
  , (buttonSelectedAttr           , Vty.defAttr `withBackColor` Vty.brightWhite)
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

eventHandler :: BrickEvent String e -> EventM String BrickState ()
eventHandler ev = do
  AppState { keyBindings = kb } <- liftIO $ readIORef settings'
  case ev of
    (MouseDown _ Vty.BScrollUp _ _) -> appState %= moveCursor 1 Up
    (MouseDown _ Vty.BScrollDown _ _) -> appState %= moveCursor 1 Down
    (VtyEvent (Vty.EvResize _ _)) -> pure ()
    (VtyEvent (Vty.EvKey Vty.KUp _)) -> appState %= moveCursor 1 Up
    (VtyEvent (Vty.EvKey Vty.KDown _)) -> appState %= moveCursor 1 Down
    (VtyEvent (Vty.EvKey key _)) ->
      case find (\(key', _, _) -> key' == key) (keyHandlers kb) of
        Nothing -> pure ()
        Just (_, _, handler) -> handler
    _ -> pure ()


moveCursor :: Int -> Direction -> BrickInternalState -> BrickInternalState
moveCursor steps direction ais@BrickInternalState{..} =
  let newIx = if direction == Down then _ix + steps else _ix - steps
  in  case _clr !? newIx of
        Just _  -> ais & ix .~ newIx
        Nothing -> ais


-- | Suspend the current UI and run an IO action in terminal. If the
-- IO action returns a Left value, then it's thrown as userError.
withIOAction :: Ord n
             => ( (Int, ListResult) -> ReaderT AppState IO (Either String a))
             -> EventM n BrickState ()
withIOAction action = do
  as <- get
  case listSelectedElement' (view appState as) of
    Nothing      -> pure ()
    Just (curr_ix, e) -> do
      suspendAndResume $ do
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
                 }


constructList :: BrickData
              -> BrickSettings
              -> Maybe BrickInternalState
              -> BrickInternalState
constructList appD appSettings =
  replaceLR (filterVisible (_showAllVersions appSettings)
                           (_showAllTools appSettings))
            (_lr appD)

listSelectedElement' :: BrickInternalState -> Maybe (Int, ListResult)
listSelectedElement' BrickInternalState{..} = fmap (_ix, ) $ _clr !? _ix


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

