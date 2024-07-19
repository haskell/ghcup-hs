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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}


{- **************

A general system inspired by Brick.Form. It uses optics instead of microlenses and it is less generic than
Brick.Form, but generic enough to serve our purpose.

A Menu consists in
  a) A state value
  b) A list of fields. Each field is capable of modifying a part of the state
  c) some metadata

A field (type MenuField) consists in
  a) a Lens to a part of the Menu state, so the Menu can call that lens to modify its own state
  b) an input widget

An input (type FieldInput) consist in
  a) some state
  b) a validator function
  c) a handler and a renderer

We have to use existential types to achive a composable API since every FieldInput has a different
internal type, and every MenuField has a different Lens. For example:
  - The menu state is a record (MyRecord {uri: URI, flag : Bool})
  - Then, there are two MenuField:
    - One MenuField has (Lens' MyRecord URI) and the other has (Lens' MyRecord Bool)
    - The MenuFields has FieldInputs with internal state Text and Bool, respectively
  - Obviously, the MenuField has to be polimorphic in the Lens' and in the Input internal state,
    But we must hide that polimorphisim (existential), in order to store all MenuField in a List

************** -}

module GHCup.Brick.Widgets.Menu where

import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.Common as Common

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..),
      (<+>))
import qualified Brick
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Border.Style as Border
import qualified Brick.Widgets.Center as Brick
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as Edit
import           Brick.Focus (FocusRing)
import qualified Brick.Focus as F
import           Data.Function ( (&))
import           Prelude             hiding ( appendFile )

import           Data.Maybe
import qualified Data.Text                     as T


import           Optics.TH (makeLensesFor)
import qualified Graphics.Vty as Vty
import Optics.State.Operators ((%=), (.=))
import Optics.Optic ((%))
import Optics.State (use)
import GHCup.Types (KeyCombination(..))
import Optics (Lens', to, lens)
import Optics.Operators ( (^.), (.~) )
import Data.Foldable (find, foldl')
import           Data.List.NonEmpty             ( NonEmpty (..) )
import qualified Data.List.NonEmpty            as NE


-- | Just some type synonym to make things explicit
type Formatter n = Bool -> Widget n -> Widget n
-- | A label
type Label = T.Text
-- | A help message of an entry
type HelpMessage = T.Text
-- | A button name
type ButtonName n = n

idFormatter :: Formatter n
idFormatter = const id


-- | An error message
type ErrorMessage = T.Text
data ErrorStatus = Valid | Invalid ErrorMessage deriving (Eq)

-- | A lens which does nothing. Usefull to defined no-op fields
emptyLens :: Lens' s ()
emptyLens = lens (const ()) (\s _ -> s)

-- | A FieldInput is a pair label-content
--   a - is the type of the field it manipulates
--   b - is its internal state (modified in the gui)
--   n - your application's resource name type
data FieldInput a b n =
  FieldInput
    { inputState :: b                                     -- ^ The state of the input field (what's rendered in the screen)
    , inputValidator :: b -> Either ErrorMessage a        -- ^ A validator function
    , inputHelp   :: HelpMessage                          -- ^ The input helpMessage
    , inputRender :: Bool
                  -> ErrorStatus
                  -> HelpMessage
                  -> Label
                  -> b
                  -> (Widget n -> Widget n)
                  -> (Widget n, Maybe (Widget n))         -- ^ How to draw the input and optionally an overlay, with focus a help message and input.
                                                          --   A extension function can be applied too
    , inputHandler :: BrickEvent n () -> EventM n b ()    -- ^ The handler
    }

makeLensesFor
  [ ("inputState", "inputStateL")
  , ("inputValidator", "inputValidatorL")
  , ("inputName", "inputNameL")
  , ("inputHelp", "inputHelpL")
  ]
  ''FieldInput

-- | The MenuField is an existential type which stores a Lens' to a part of the Menu state.
--   In also contains a Field input which internal state is hidden
data MenuField s n where
  MenuField ::
      { fieldAccesor   :: Lens' s a                   -- ^ A Lens pointing to some part of the state
      , fieldInput     :: FieldInput a b n            -- ^ The input which modifies the state
      , fieldLabel     :: Label                       -- ^ The label
      , fieldStatus    :: ErrorStatus                 -- ^ Whether the current is valid or not.
      , fieldName      :: n
      } -> MenuField s n

isValidField :: MenuField s n -> Bool
isValidField = (== Valid) . fieldStatus

makeLensesFor
  [ ("fieldLabel", "fieldLabelL")
  , ("fieldStatus", "fieldStatusL")
  ]
  ''MenuField

data SelectState i = SelectState
  { selectStateItems :: NonEmpty (Int, (i, Bool))         -- ^ All items along with their selected state
  , selectStateFocusRing :: FocusRing Int                 -- ^ Focus ring using integeral values assigned to each item
  , selectStateOverlayOpen :: Bool                        -- ^ Whether the select menu is open
  }

makeLensesFor
  [ ("selectStateItems", "selectStateItemsL")
  , ("selectStateFocusRing", "selectStateFocusRingL")
  , ("selectStateOverlayOpen", "selectStateOverlayOpenL")
  ]
  ''SelectState

data EditState n = EditState
  { editState :: Edit.Editor T.Text n
  , editStateOverlayOpen :: Bool                        -- ^ Whether the edit menu is open
  }

makeLensesFor
  [ ("editState", "editStateL")
  , ("editStateOverlayOpen", "editStateOverlayOpenL")
  ]
  ''EditState

-- | A fancy lens to the help message
fieldHelpMsgL :: Lens' (MenuField s n) HelpMessage
fieldHelpMsgL = lens g s
  where g (MenuField {..})= fieldInput ^. inputHelpL
        s (MenuField{..}) msg = MenuField {fieldInput = fieldInput & inputHelpL .~ msg , ..}

-- | How to draw a field given a formater
drawField :: Formatter n -> Bool -> MenuField s n -> Widget n
drawField amp focus (MenuField { fieldInput = FieldInput {..}, ..}) =
  let (input, overlay) = inputRender focus fieldStatus inputHelp fieldLabel inputState (amp focus)
    in case (focus, overlay) of
         (True, Nothing) -> Common.enableScreenReader fieldName $ Brick.visible input
         _ -> input

drawFieldOverlay :: MenuField s n -> Maybe (Widget n)
drawFieldOverlay (MenuField { fieldInput = FieldInput {..}, ..}) =
  snd $ inputRender True fieldStatus inputHelp fieldLabel inputState id

instance Brick.Named (MenuField s n) n where
  getName :: MenuField s n -> n
  getName entry = entry & fieldName


{- *****************
  CheckBox widget
***************** -}

type CheckBoxField = MenuField

createCheckBoxInput :: FieldInput Bool Bool n
createCheckBoxInput = FieldInput False Right "" checkBoxRender checkBoxHandler
  where
    border w = Brick.txt "[" <+> (Brick.padRight (Brick.Pad 1) $ Brick.padLeft (Brick.Pad 2) w) <+> Brick.txt "]"
    drawBool b =
        if b
          then border . Brick.withAttr Attributes.installedAttr    $ Brick.str Common.installedSign
          else border . Brick.withAttr Attributes.notInstalledAttr $ Brick.str Common.notInstalledSign
    checkBoxRender focus _ help _ check f = (, Nothing) $
      let core = f $ drawBool check
      in if focus
        then core
        else core <+> (Brick.padLeft (Brick.Pad 1) . renderAsHelpMsg $ help)
    checkBoxHandler = \case
        VtyEvent (Vty.EvKey Vty.KEnter []) -> Brick.modify not
        _ -> pure ()

createCheckBoxField :: n -> Lens' s Bool -> CheckBoxField s n
createCheckBoxField name access = MenuField access createCheckBoxInput "" Valid name

{- *****************
  Editable widget
***************** -}

type EditableField = MenuField

createEditableInput :: (Ord n, Show n) => n -> (T.Text -> Either ErrorMessage a) -> KeyCombination -> FieldInput a (EditState n) n
createEditableInput name validator exitKey@(KeyCombination {..}) = FieldInput initEdit validateEditContent "" drawEdit handler
  where
    drawEdit focus errMsg help label (EditState edi overlayOpen) amp = (field, mOverlay)
      where
        field =
          let
            borderBox w = amp (Brick.vLimit 1 $ Border.vBorder <+> Brick.padRight Brick.Max w <+> Border.vBorder)
            editorContents = Brick.txt $ T.unlines $ Edit.getEditContents edi
            isEditorEmpty = Edit.getEditContents edi == [mempty]
          in case errMsg of
               Valid | isEditorEmpty -> borderBox $ renderAsHelpMsg help
                     | otherwise -> borderBox editorContents
               Invalid msg
                 | focus && isEditorEmpty -> borderBox $ renderAsHelpMsg help
                 | focus     -> borderBox editorContents
                 | otherwise -> borderBox $ renderAsErrMsg msg
        mOverlay = if overlayOpen
          then Just (overlayLayer ("Edit " <> label) $ overlay)
          else Nothing
        overlay = Brick.vBox $
          [ Brick.txtWrap help
          , Border.border $ Edit.renderEditor (Brick.txt . T.unlines) focus edi
          , case errMsg of
              Invalid msg -> renderAsErrMsg msg
              _ -> Brick.txt " "
          , Brick.padRight Brick.Max $
              Brick.txt "Press "
              <+> Common.keyToWidget exitKey
              <+> Brick.txt " or Enter to go back"
          ]
    handler ev = do
      (EditState edi overlayOpen) <- Brick.get
      if overlayOpen
        then case ev of
          VtyEvent (Vty.EvKey k m) | k == key && m == mods -> editStateOverlayOpenL .= False
          VtyEvent (Vty.EvKey Vty.KEnter []) -> editStateOverlayOpenL .= False
          _ -> Common.zoom editStateL $ Edit.handleEditorEvent ev
        else case ev of
          VtyEvent (Vty.EvKey Vty.KEnter []) -> editStateOverlayOpenL .= True
          _ -> pure ()
    validateEditContent = validator . T.init . T.unlines . Edit.getEditContents . editState
    initEdit = EditState (Edit.editorText name (Just 1) "") False

createEditableField :: (Eq n, Ord n, Show n) => n -> (T.Text -> Either ErrorMessage a) -> Lens' s a -> KeyCombination -> EditableField s n
createEditableField name validator access exitKey = MenuField access input "" Valid name
  where
    input = createEditableInput name validator exitKey

{- *****************
  Button widget
***************** -}

type Button = MenuField

createButtonInput :: FieldInput () () n
createButtonInput = FieldInput () Right "" drawButton (const $ pure ())
  where
    drawButton True (Invalid err) _    _ _ amp = (amp . centerV . renderAsErrMsg $ err, Nothing)
    drawButton _    _             help _ _ amp = (amp . centerV . renderAsHelpMsg $ help, Nothing)

createButtonField :: n -> Button s n
createButtonField = MenuField emptyLens createButtonInput "" Valid

{- *****************
  Select widget
***************** -}

type SelectField = MenuField

createSelectInput :: (Ord n, Show n)
  => NonEmpty i
  -> (i -> T.Text)
  -> (Int -> NonEmpty (Int, (i, Bool)) -> NonEmpty (Int, (i, Bool)))
  -> ([i] -> k)
  -> n
  -> KeyCombination
  -> FieldInput k (SelectState i) n
createSelectInput items showItem updateSelection getSelection fieldName exitKey@(KeyCombination {..})
  = FieldInput initState (Right . getSelection . getSelectedItems) "" selectRender selectHandler
  where
    initState = SelectState
      (NE.zip (1 NE.:| [2..]) $ fmap (,False) items)
      (F.focusRing [1..(length items)])
      False
    getSelectedItems = fmap (fst . snd) . (filter (snd . snd)) . NE.toList . selectStateItems

    border w = Brick.txt "[" <+> (Brick.padRight (Brick.Pad 1) $ Brick.padLeft (Brick.Pad 1) w) <+> Brick.txt "]"
    selectRender focus errMsg help label s amp = (field, mOverlay)
      where
        field = amp $ case getSelectedItems s of
          [] -> (Brick.padLeft (Brick.Pad 1) . renderAsHelpMsg $ help)
          xs ->
            let list = border $ Brick.hBox $ fmap (Brick.padRight (Brick.Pad 1) . Brick.txt . showItem) xs
            in if focus
              then list
              else list <+> (Brick.padLeft (Brick.Pad 1) . renderAsHelpMsg $ help)
        mOverlay = if selectStateOverlayOpen s
          then Just (overlayLayer ("Select " <> label)  $ overlay s)
          else Nothing
    overlay (SelectState {..}) = Brick.vBox $
      [ Brick.padRight Brick.Max $
            Brick.txt "Press "
            <+> Common.keyToWidget exitKey
            <+> Brick.txt " to go back, Press Enter to select"
      , Brick.vLimit (length items) $ Brick.withVScrollBars Brick.OnRight
          $ Brick.viewport fieldName Brick.Vertical
          $ Brick.vBox $ (NE.toList $ fmap (mkSelectRow focused) selectStateItems)
      ]
      where focused = fromMaybe 1 $ F.focusGetCurrent selectStateFocusRing
    mkSelectRow focused (ix, (item, selected)) = (if focused == ix then Brick.visible else id) $
      Brick.txt "[" <+> (Brick.padRight (Brick.Pad 1) $ Brick.padLeft (Brick.Pad 1) m) <+> Brick.txt "] "
        <+> (renderAslabel (showItem item) (focused == ix))
      where m = if selected then Brick.txt "*" else Brick.txt " "

    selectHandler ev = do
      s <- Brick.get
      if selectStateOverlayOpen s
        then case ev of
          VtyEvent (Vty.EvKey k m) | k == key && m == mods -> selectStateOverlayOpenL .= False
          VtyEvent (Vty.EvKey (Vty.KChar '\t') [])  -> selectStateFocusRingL %= F.focusNext
          VtyEvent (Vty.EvKey Vty.KBackTab [])      -> selectStateFocusRingL %= F.focusPrev
          VtyEvent (Vty.EvKey Vty.KDown [])         -> selectStateFocusRingL %= F.focusNext
          VtyEvent (Vty.EvKey Vty.KUp [])           -> selectStateFocusRingL %= F.focusPrev
          VtyEvent (Vty.EvKey Vty.KEnter [])        -> do
            focused <- use (selectStateFocusRingL % to F.focusGetCurrent)
            selectStateItemsL %= updateSelection (fromMaybe 1 focused)
          _ -> pure ()
        else case ev of
          VtyEvent (Vty.EvKey Vty.KEnter []) -> selectStateOverlayOpenL .= True
          _ -> pure ()

-- | Select Field with only single selection possible, aka radio button
createSelectField :: (Ord n, Show n) => n -> Lens' s (Maybe i) -> NonEmpty i -> (i -> T.Text) -> KeyCombination -> SelectField s n
createSelectField name access items showItem exitKey = MenuField access (createSelectInput items showItem singleSelect getSelection name exitKey) "" Valid name
  where
    singleSelect :: Int -> NonEmpty (Int, (i, Bool)) -> NonEmpty (Int, (i, Bool))
    singleSelect ix = fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, True)) else (ix', (i, False)))

    getSelection = fmap NE.head . NE.nonEmpty

-- | Select Field with multiple selections possible
createMultiSelectField :: (Ord n, Show n) => n -> Lens' s [i] -> NonEmpty i -> (i -> T.Text) -> KeyCombination -> SelectField s n
createMultiSelectField name access items showItem exitKey = MenuField access (createSelectInput items showItem multiSelect id name exitKey) "" Valid name
  where
    multiSelect :: Int -> NonEmpty (Int, (i, Bool)) -> NonEmpty (Int, (i, Bool))
    multiSelect ix = fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, not b)) else (ix', (i, b)))


{- *****************
  Utilities
***************** -}

-- | highlights a widget (using List.listSelectedFocusedAttr)
highlighted :: Widget n -> Widget n
highlighted = Brick.withAttr L.listSelectedFocusedAttr

-- | Given a text, crates a highlighted label on focus. An amplifier can be passed
renderAslabel :: T.Text -> Bool -> Widget n
renderAslabel t focus =
  if focus
    then highlighted $ Brick.txt t
    else Brick.txt t

-- | Creates a left align column.
-- Example:       |- col2 is align dispite the length of col1
--   row1_col1         row1_col2
--   row2_col1_large   row2_col2
leftify :: Int -> Brick.Widget n -> Brick.Widget n
leftify i = Brick.hLimit i . Brick.padRight Brick.Max

-- | Creates a right align column.
-- Example:       |- col2 is align dispite the length of col1
--         row1_col1   row1_col2
--   row2_col1_large   row2_col2
rightify :: Int -> Brick.Widget n -> Brick.Widget n
rightify i = Brick.hLimit i . Brick.padLeft Brick.Max

-- | center a line in three rows.
centerV :: Widget n -> Widget n
centerV = Brick.padTopBottom 1

-- | render some Text using helpMsgAttr
renderAsHelpMsg :: T.Text -> Widget n
renderAsHelpMsg = Brick.withAttr Attributes.helpMsgAttr . Brick.txt

-- | render some Text using errMsgAttr
renderAsErrMsg :: T.Text -> Widget n
renderAsErrMsg = Brick.withAttr Attributes.errMsgAttr . Brick.txt

-- | Used to create a layer on top of menu
overlayLayer :: T.Text -> Brick.Widget n -> Brick.Widget n
overlayLayer layer_name =
    Brick.centerLayer
      . Brick.hLimitPercent 50
      . Brick.vLimitPercent 65
      . Brick.withBorderStyle Border.unicode
      . Border.borderWithLabel (Brick.txt layer_name)

{- *****************
  Menu widget
***************** -}

-- | A menu is a list of Fields and a state. Informally we can think about s in terms of the record type returned by
-- a form.
data Menu s n
    = Menu
    { menuFields    :: [MenuField s n]   -- ^ The datatype representing the list of entries. Precisely, any array-like data type is highly unconvinient.
    , menuState     :: s
    , menuValidator :: s -> Maybe ErrorMessage  -- ^ A validator function
    , menuButtons   :: [Button s n]      -- ^ The buttons. Commonly, the handlers for buttons are defined outside the menu handler.
    , menuFocusRing :: FocusRing n       -- ^ The focus ring with the resource name for each entry and each button, in the order you want to loop them.
    , menuExitKey   :: KeyCombination    -- ^ The key to exit the Menu
    , menuName      :: n                 -- ^ The resource Name.
    , menuTitle     :: T.Text            -- ^ Menu title.
    }

makeLensesFor
  [ ("menuFields", "menuFieldsL"), ("menuState", "menuStateL"), ("menuValidator", "menuValidatorL")
  , ("menuButtons", "menuButtonsL"), ("menuFocusRing", "menuFocusRingL")
  , ("menuExitKey", "menuExitKeyL"), ("menuName", "menuNameL")
  , ("menuTitle", "menuTitleL")
  ]
  ''Menu

isValidMenu :: Menu s n -> Bool
isValidMenu m = (all isValidField $ menuFields m)
  && (case (menuValidator m) (menuState m) of { Nothing -> True; _ -> False })

createMenu :: n -> s -> T.Text -> (s -> Maybe ErrorMessage)
  -> KeyCombination -> [Button s n] -> [MenuField s n] -> Menu s n
createMenu n initial title validator exitK buttons fields = Menu fields initial validator buttons ring exitK n title
  where ring = F.focusRing $ [field & fieldName | field <- fields] ++ [button & fieldName | button <- buttons]

handlerMenu :: forall n e s. Eq n => BrickEvent n e -> EventM n (Menu s n) ()
handlerMenu ev = do
  fields  <- use menuFieldsL
  focused <- use $ menuFocusRingL % to F.focusGetCurrent
  let focusedField = (\n -> find (\x -> Brick.getName x == n) fields) =<< focused
      propagateEvent e = case focused of
        Nothing -> pure ()
        Just n  -> do
          updated_fields <- updateFields n (VtyEvent e) fields
          validator  <- use menuValidatorL
          state <- use menuStateL
          if all isValidField updated_fields
            then case validator state of
              Nothing -> menuButtonsL %= fmap (fieldStatusL .~ Valid)
              Just err -> menuButtonsL %= fmap (fieldStatusL .~ Invalid err)
            else menuButtonsL %= fmap (fieldStatusL .~ Invalid "Some fields are invalid")
          menuFieldsL .= updated_fields
  case (drawFieldOverlay =<< focusedField) of
    Just _ -> case ev of
      VtyEvent e -> propagateEvent e
      _ -> pure ()
    Nothing -> case ev of
      VtyEvent (Vty.EvKey (Vty.KChar '\t') [])  -> menuFocusRingL %= F.focusNext
      VtyEvent (Vty.EvKey Vty.KBackTab [])      -> menuFocusRingL %= F.focusPrev
      VtyEvent (Vty.EvKey Vty.KDown [])         -> menuFocusRingL %= F.focusNext
      VtyEvent (Vty.EvKey Vty.KUp [])           -> menuFocusRingL %= F.focusPrev
      VtyEvent e -> propagateEvent e
      _ -> pure ()
 where
  -- runs the Event with the inner handler of MenuField.
  updateFields :: n -> BrickEvent n () -> [MenuField s n] -> EventM n (Menu s n) [MenuField s n]
  updateFields n e = traverse $ \x@(MenuField {fieldInput = FieldInput {..}, ..}) ->
    if Brick.getName x == n
      then do
       newb <- Brick.nestEventM' inputState (inputHandler e)
       let newField = MenuField {fieldInput = (FieldInput {inputState=newb, ..}) , ..}
       case inputValidator newb of
        Left errmsg -> pure $ newField & fieldStatusL .~ Invalid errmsg
        Right a     -> menuStateL % fieldAccesor .= a >> pure (newField & fieldStatusL .~ Valid)
      else pure x


drawMenu :: (Eq n, Ord n, Show n, Brick.Named (MenuField s n) n) => Menu s n -> [Widget n]
drawMenu menu =
  overlays ++
  [Common.frontwardLayer (menu ^. menuTitleL) mainLayer]
  where
    mainLayer = Brick.vBox
      [ Brick.vBox buttonWidgets
      , Common.separator
      , Brick.vLimit (length fieldLabels) $ Brick.withVScrollBars Brick.OnRight
          $ Brick.viewport (menu ^. menuNameL) Brick.Vertical
          $ Brick.vBox fieldWidgets
      , Brick.txt " "
      , Brick.padRight Brick.Max $
          Brick.txt "Press "
          <+> Common.keyToWidget (menu ^. menuExitKeyL)
          <+> Brick.txt " to go back, Press Enter to edit the highlighted field"
      ]
    fieldLabels  = [field & fieldLabel | field <- menu ^. menuFieldsL]
    buttonLabels = [button & fieldLabel | button <- menu ^. menuButtonsL]
    allLabels    = fieldLabels ++ buttonLabels

    maxWidth = foldl' max 5 (fmap Brick.textWidth allLabels)

    -- A list of functions which draw a highlighted label with right padding at the left of a widget.
    amplifiers =
      let labelsWidgets = fmap renderAslabel fieldLabels
       in fmap (\f b -> ((rightify (maxWidth + 1) (f b <+> Brick.txt " ")) <+>) ) labelsWidgets
    drawFields = fmap drawField amplifiers
    fieldWidgets = zipWith (F.withFocusRing (menu ^. menuFocusRingL)) drawFields (menu ^. menuFieldsL)

    buttonAmplifiers =
      let buttonAsWidgets = fmap renderAslabel buttonLabels
       in fmap (\f b -> ((leftify (maxWidth + 2) . Border.border $ f b) <+>) ) buttonAsWidgets
    drawButtons = fmap drawField buttonAmplifiers
    buttonWidgets = zipWith (F.withFocusRing (menu ^. menuFocusRingL)) drawButtons (menu ^. menuButtonsL)

    overlays = catMaybes $ fmap drawFieldOverlay (menu ^. menuFieldsL)
