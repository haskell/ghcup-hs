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
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as Edit
import           Brick.Focus (FocusRing)
import qualified Brick.Focus as F
import           Data.Function ( (&))
import           Prelude             hiding ( appendFile )

import qualified Data.Text                     as T


import           Optics.TH (makeLensesFor)
import qualified Graphics.Vty as Vty
import Optics.State.Operators ((%=), (.=))
import Optics.Optic ((%))
import Optics.State (use)
import GHCup.Types (KeyCombination)
import Optics (Lens', to, lens)
import Optics.Operators ( (^.), (.~) )
import Data.Foldable (foldl')


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
data ErrorStatus = Valid | Invalid ErrorMessage

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
                  -> b
                  -> (Widget n -> Widget n)
                  -> Widget n                             -- ^ How to draw the input, with focus a help message and input. 
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


makeLensesFor
  [ ("fieldLabel", "fieldLabelL")
  , ("fieldStatus", "fieldStatusL")
  ]
  ''MenuField

-- | A fancy lens to the help message
fieldHelpMsgL :: Lens' (MenuField s n) HelpMessage
fieldHelpMsgL = lens g s
  where g (MenuField {..})= fieldInput ^. inputHelpL
        s (MenuField{..}) msg = MenuField {fieldInput = fieldInput & inputHelpL .~ msg , ..}

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
    border = Border.border . Brick.padRight (Brick.Pad 1) . Brick.padLeft (Brick.Pad 2)
    drawBool b =
        if b
          then border . Brick.withAttr Attributes.installedAttr    $ Brick.str Common.installedSign
          else border . Brick.withAttr Attributes.notInstalledAttr $ Brick.str Common.notInstalledSign
    checkBoxRender focus _ help check f =
      let core = f $ drawBool check
      in if focus 
        then core
        else core <+> (Brick.padLeft (Brick.Pad 1) . centerV . renderAsHelpMsg $ help)
    checkBoxHandler = \case
        VtyEvent (Vty.EvKey Vty.KEnter []) -> Brick.modify not
        _ -> pure ()

createCheckBoxField :: n -> Lens' s Bool -> CheckBoxField s n
createCheckBoxField name access = MenuField access createCheckBoxInput "" Valid name

{- *****************
  Editable widget
***************** -}

type EditableField = MenuField

createEditableInput :: (Ord n, Show n) => n -> (T.Text -> Either ErrorMessage a) -> FieldInput a (Edit.Editor T.Text n) n
createEditableInput name validator = FieldInput initEdit validateEditContent "" drawEdit Edit.handleEditorEvent
  where
    drawEdit focus errMsg help edi amp =
      let 
        borderBox = amp . Border.border . Brick.padRight Brick.Max
        editorRender = Edit.renderEditor (Brick.txt . T.unlines) focus edi
      in case errMsg of
           Valid ->
              if Edit.getEditContents edi == [mempty] 
                then borderBox $ renderAsHelpMsg help 
                else borderBox editorRender
           Invalid msg ->
              if focus
                then borderBox editorRender
                else borderBox $ renderAsErrMsg msg
    validateEditContent = validator . T.unlines . Edit.getEditContents
    initEdit = Edit.editorText name (Just 1) ""

createEditableField :: (Eq n, Ord n, Show n) => n -> (T.Text -> Either ErrorMessage a) -> Lens' s a  -> EditableField s n
createEditableField name validator access = MenuField access input "" Valid name
  where
    input = createEditableInput name validator

{- *****************
  Button widget
***************** -}

type Button = MenuField

createButtonInput :: FieldInput () () n
createButtonInput = FieldInput () Right "" drawButton (const $ pure ())
  where drawButton _ _ help _ amp = amp . centerV . renderAsHelpMsg $ help

createButtonField :: n -> Button s n
createButtonField = MenuField emptyLens createButtonInput "" Valid

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

-- | center a line in three rows. 
centerV :: Widget n -> Widget n
centerV = Brick.padTopBottom 1

-- | render some Text using helpMsgAttr
renderAsHelpMsg :: T.Text -> Widget n
renderAsHelpMsg = Brick.withAttr Attributes.helpMsgAttr . Brick.txt

-- | render some Text using errMsgAttr
renderAsErrMsg :: T.Text -> Widget n
renderAsErrMsg = Brick.withAttr Attributes.errMsgAttr . Brick.txt

{- *****************
  Menu widget
***************** -}

-- | A menu is a list of Fields and a state. Informally we can think about s in terms of the record type returned by 
-- a form. 
data Menu s n
    = Menu
    { menuFields    :: [MenuField s n]   -- ^ The datatype representing the list of entries. Precisely, any array-like data type is highly unconvinient.
    , menuState     :: s
    , menuButtons   :: [Button s n]      -- ^ The buttons. Commonly, the handlers for buttons are defined outside the menu handler.
    , menuFocusRing :: FocusRing n       -- ^ The focus ring with the resource name for each entry and each button, in the order you want to loop them.
    , menuExitKey   :: KeyCombination    -- ^ The key to exit the Menu
    , menuName      :: n                 -- ^ The resource Name.
    }


makeLensesFor
  [ ("menuFields", "menuFieldsL"), ("menuState", "menuStateL")
  , ("menuButtons", "menuButtonsL"), ("menuFocusRing", "menuFocusRingL")
  , ("menuExitKey", "menuExitKeyL"), ("menuName", "menuNameL")
  ]
  ''Menu

createMenu :: n -> s -> KeyCombination -> [Button s n] -> [MenuField s n] -> Menu s n
createMenu n initial exitK buttons fields = Menu fields initial buttons ring exitK n
  where ring = F.focusRing $ [field & fieldName | field <- fields] ++ [button & fieldName | button <- buttons]

handlerMenu :: forall n e s. Eq n => BrickEvent n e -> EventM n (Menu s n) ()
handlerMenu ev =
  case ev of
    VtyEvent (Vty.EvKey (Vty.KChar '\t') [])  -> menuFocusRingL %= F.focusNext
    VtyEvent (Vty.EvKey Vty.KBackTab [])      -> menuFocusRingL %= F.focusPrev
    VtyEvent (Vty.EvKey Vty.KDown [])         -> menuFocusRingL %= F.focusNext
    VtyEvent (Vty.EvKey Vty.KUp [])           -> menuFocusRingL %= F.focusPrev
    VtyEvent e -> do
      focused <- use $ menuFocusRingL % to F.focusGetCurrent
      fields  <- use menuFieldsL
      case focused of
        Nothing -> pure ()
        Just n  -> do 
          updated_fields <- updateFields n (VtyEvent e) fields
          menuFieldsL .= updated_fields
    _ -> pure ()
 where
  updateFields :: n -> BrickEvent n () -> [MenuField s n] -> EventM n (Menu s n) [MenuField s n]
  updateFields n e [] = pure []
  updateFields n e (x@(MenuField {fieldInput = i@(FieldInput {..}) , ..}):xs) =
    if Brick.getName x == n
      then do
       newb <- Brick.nestEventM' inputState (inputHandler e)
       let newField = MenuField {fieldInput = (FieldInput {inputState=newb, ..}) , ..}
       case inputValidator newb of
        Left errmsg -> pure $ (newField & fieldStatusL .~ Invalid errmsg):xs
        Right a     -> menuStateL % fieldAccesor .= a >> pure ((newField & fieldStatusL .~ Valid):xs)
      else fmap (x:) (updateFields n e xs)


drawMenu :: (Eq n, Ord n, Show n, Brick.Named (MenuField s n) n) => Menu s n -> Widget n
drawMenu menu = 
  Brick.vBox
      [ Brick.vBox buttonWidgets
      , Common.separator
      , Brick.withVScrollBars Brick.OnRight
          $ Brick.viewport (menu ^. menuNameL) Brick.Vertical
          $ Brick.vBox fieldWidgets
      , Brick.txt " "
      , Brick.padRight Brick.Max $ 
          Brick.txt "Press " 
          <+> Common.keyToWidget (menu ^. menuExitKeyL)
          <+> Brick.txt " to go back"
      ]
  where
    drawField amp focus (MenuField { fieldInput = FieldInput {..}, ..}) =
      let input = inputRender focus fieldStatus inputHelp inputState (amp focus)
       in if focus
            then Brick.visible input
            else input
    
    fieldLabels  = [field & fieldLabel | field <- menu ^. menuFieldsL]
    buttonLabels = [button & fieldLabel | button <- menu ^. menuButtonsL]
    allLabels    = fieldLabels ++ buttonLabels

    maxWidth = foldl' max 5 (fmap Brick.textWidth allLabels)

    -- A list of functions which draw a highlighted label with right padding at the left of a widget. 
    amplifiers =
      let labelsWidgets = fmap (\b -> renderAslabel b) fieldLabels
       in fmap (\f b -> ((centerV . leftify (maxWidth + 10) $ f b) <+>) ) labelsWidgets
    drawFields = fmap drawField amplifiers
    fieldWidgets = zipWith (F.withFocusRing (menu ^. menuFocusRingL)) drawFields (menu ^. menuFieldsL)

    buttonAmplifiers =
      let buttonAsWidgets = fmap (\b -> renderAslabel b) buttonLabels
       in fmap (\f b -> ((leftify (maxWidth + 10) . Border.border $ f b) <+>) ) buttonAsWidgets
    drawButtons = fmap drawField buttonAmplifiers
    buttonWidgets = zipWith (F.withFocusRing (menu ^. menuFocusRingL)) drawButtons (menu ^. menuButtonsL)


