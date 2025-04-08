{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.Widgets.InputField.SelectInput where

import GHCup.Types (KeyCombination(..))
import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.Common as Common

import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.BasicOverlay
import GHCup.Brick.Widgets.InputField.Class
import qualified GHCup.Brick.Widgets.InputField.EditInput as EditInput

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import qualified Brick.Focus as F
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Edit as Edit

import           Data.Maybe
import           Data.List.NonEmpty             ( NonEmpty (..) )
import qualified Data.List.NonEmpty            as NE
import Data.Some
import qualified Data.Text                     as T

import qualified Graphics.Vty as Vty
import Optics (Lens', lens, to, over, use, _1, (^.), (%))
import Optics.State.Operators ((%=), (.=), (?=))
import Optics.TH (makeLenses)

data SelectInput n i a = SelectInput
  { _name :: n
  , _overlay :: Maybe (Some (IsSubWidget n (SelectInput n i a)))
  , _selectInputOverlay :: BasicOverlay n (SelectInputOverlay n i a)
  , _title :: T.Text
  , _helpMessage :: HelpMessage
  }

data SelectInputOverlay n i a = SelectInputOverlay
  { _items :: (NonEmpty (Int, (i, Bool)), Bool) -- ^ All items along with their selected state
                                                          -- And Bool to indicate if editable field is selected
  , _editInput :: Maybe (EditInput.EditInput n a)  -- ^ Editable field
  , _focusRing :: F.FocusRing Int                 -- ^ Focus ring using integeral values assigned to each item
  , _showItem :: (i -> T.Text)
  , _update :: (Int -> (NonEmpty (Int, (i, Bool)), Bool) -> ((NonEmpty (Int, (i, Bool))), Bool))
  , _menuKeys :: Common.MenuKeyBindings
  , _viewportName :: n
  }

concat <$> mapM makeLenses [''SelectInputOverlay, ''SelectInput]

createSelectInput :: (Eq n, Show n)
  => n
  -> T.Text
  -> NonEmpty i
  -> (i -> T.Text)
  -> T.Text
  -> HelpMessage
  -> Common.MenuKeyBindings
  -> SelectInput n i ()
createSelectInput name label items showItem title helpMsg kb =
  SelectInput name Nothing
  (BasicOverlay overlay [kb ^. Common.mKbQuit] (Common.smallerOverlayLayer title))
  title helpMsg
  where
    overlay = SelectInputOverlay initState Nothing (F.focusRing [1.. totalRows]) showItem singleSelect kb name
    totalRows = length items
    initState = (NE.zip (1 NE.:| [2..]) $ fmap (,False) items, False)

    singleSelect :: Int -> (NonEmpty (Int, (i, Bool)), a) -> (NonEmpty (Int, (i, Bool)), a)
    singleSelect ix = over _1 $ fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, True)) else (ix', (i, False)))

createMultiSelectInput :: (Eq n, Show n)
  => n
  -> T.Text
  -> NonEmpty i
  -> (i -> T.Text)
  -> T.Text
  -> HelpMessage
  -> Common.MenuKeyBindings
  -> SelectInput n i ()
createMultiSelectInput name label items showItem title helpMsg kb =
  SelectInput name Nothing
  (BasicOverlay overlay [kb ^. Common.mKbQuit] (Common.smallerOverlayLayer title))
  title helpMsg
  where
    overlay = SelectInputOverlay initState Nothing (F.focusRing [1.. totalRows]) showItem multiSelect kb name
    totalRows = length items
    initState = (NE.zip (1 NE.:| [2..]) $ fmap (,False) items, False)

    multiSelect :: Int -> (NonEmpty (Int, (i, Bool)), a) -> (NonEmpty (Int, (i, Bool)), a)
    multiSelect ix = over _1 $ fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, not b)) else (ix', (i, b)))

createSelectInputWithEditable :: (Eq n, Show n)
  => n
  -> n
  -> T.Text
  -> NonEmpty i
  -> (i -> T.Text)
  -> (T.Text -> Either T.Text a)
  -> T.Text
  -> HelpMessage
  -> Common.MenuKeyBindings
  -> SelectInput n i a
createSelectInputWithEditable name editName label items showItem validator title helpMsg kb =
  SelectInput name Nothing
  (BasicOverlay overlay [kb ^. Common.mKbQuit] (Common.smallerOverlayLayer title))
  title helpMsg
  where
    overlay = SelectInputOverlay initState (Just editInp) (F.focusRing [1..totalRows]) showItem singleSelect kb name
    totalRows = length items + 1
    initState = (NE.zip (1 NE.:| [2..]) $ fmap (,False) items, False)

    editInp = EditInput.create editName label helpMsg validator ""

    singleSelect :: Int -> (NonEmpty (Int, (i, Bool)), Bool) -> (NonEmpty (Int, (i, Bool)), Bool)
    singleSelect ix (ne, a) = (fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, True)) else (ix', (i, False))) ne, ix == length ne + 1)

instance (Ord n, Show n) => BaseWidget n (SelectInput n i a) where
  draw (SelectInput {..}) =
    let showItem = _showItem $ _innerWidget $ _selectInputOverlay
    in case getSelection (_innerWidget $ _selectInputOverlay) of
         ([], Nothing) -> (Brick.padLeft (Brick.Pad 1) . Common.renderAsHelpMsg $ _helpMessage)
         (_, Just (Left msg)) -> Brick.padLeft (Brick.Pad 1) $ Common.renderAsErrMsg msg
         (xs, Just (Right txt)) -> Brick.hBox $
           fmap (Brick.padRight (Brick.Pad 1) . Brick.txt . showItem) xs
             ++ [Brick.txt txt]
         (xs, Nothing) -> Brick.hBox $ fmap (Brick.padRight (Brick.Pad 1) . Brick.txt . showItem) xs

  handleEvent ev = do
    case ev of
      VtyEvent (Vty.EvKey Vty.KEnter []) -> overlay ?= Some (IsSubWidget selectInputOverlay)
      _ -> pure ()
    pure Nothing

  hasOverlay = _overlay
  closeOverlay = overlay .= Nothing

instance (Ord n, Show n) => InputField n (SelectInput n i a) where
  getLabel e = (_name e, _title e)

instance (Ord n, Show n) => BaseWidget n (SelectInputOverlay n i a) where
  draw (SelectInputOverlay {..}) = Brick.vBox $
      [ Brick.txt "Press "
        <+> Common.keyToWidget (_menuKeys ^. Common.mKbQuit)
        <+> Brick.txt " to go back, Press Enter to select"
      -- , case errMsg of Invalid msg -> renderAsErrMsg msg; _ -> Brick.emptyWidget
      , Brick.vLimit (totalRows) $ Brick.withVScrollBars Brick.OnRight
          $ Brick.viewport _viewportName Brick.Vertical
          $ Brick.vBox $ mEditableField ++ (NE.toList $ fmap (mkSelectRow focused) (fst _items))
      ]
    where
      focused = fromMaybe 1 $ F.focusGetCurrent _focusRing
      txtFieldFocused = focused == totalRows
      mEditableField = case _editInput of
        Just e -> [ mkEditTextRow txtFieldFocused e (snd _items) ]
        Nothing -> []

      totalRows = (if isJust _editInput then (+) 1 else id) $ length (fst _items)

      mkSelectRow focused (ix, (item, selected)) = (if focused == ix then Brick.visible else id) $
        Brick.txt "[" <+> (Brick.padRight (Brick.Pad 1) $ Brick.padLeft (Brick.Pad 1) m) <+> Brick.txt "] "
          <+> (Common.renderAslabel (_showItem item) (focused == ix))
        where m = if selected then Brick.txt "*" else Brick.txt " "

      mkEditTextRow focused e selected = (if focused then Brick.visible else id) $
        Brick.txt "[" <+> (Brick.padRight (Brick.Pad 1) $ Brick.padLeft (Brick.Pad 1) m) <+> Brick.txt "] "
          <+> if focused
                 then Brick.txt "(Press e to edit, Enter to select)"
                 else if Edit.getEditContents edi == [mempty]
                   then Brick.txt "(Specify custom text value)"
                   else draw e
        where m = if selected then Brick.txt "*" else Brick.txt " "
              edi = EditInput._editor $ _innerWidget $ EditInput._editInputOverlay e

  handleEvent ev = do
    focused <- use (focusRing % Optics.to F.focusGetCurrent)
    (SelectInputOverlay {..}) <- Brick.get
    let totalRows = (if isJust _editInput then (+) 1 else id) $ length (fst _items)
    case ev of
      VtyEvent (Vty.EvKey k m)
        | KeyCombination k m == _menuKeys ^. Common.mKbUp   -> focusRing %= F.focusPrev
        | KeyCombination k m == _menuKeys ^. Common.mKbDown -> focusRing %= F.focusNext
      VtyEvent (Vty.EvKey Vty.KEnter []) ->
        items %= _update (fromMaybe 1 focused)
      VtyEvent (Vty.EvKey (Vty.KChar 'e') []) -> case (focused, _editInput) of
        (Just ix, Just editInput)
          | ix == totalRows -> Common.zoom selectInputOverlayEditInputJust $ EditInput.overlay ?= Some (IsSubWidget EditInput.editInputOverlay)
        _ -> pure ()
      _ -> pure ()
    pure Nothing

  hasOverlay (SelectInputOverlay {..}) = case hasOverlay =<< _editInput of
    Nothing -> Nothing
    Just (Some (IsSubWidget accessor)) -> Just (Some (IsSubWidget (selectInputOverlayEditInputJust % accessor)))

  closeOverlay = do
    (SelectInputOverlay {..}) <- Brick.get
    case _editInput of
      Nothing -> pure ()
      Just edi -> Common.zoom selectInputOverlayEditInputJust closeOverlay

selectInputOverlayEditInputJust = editInput % lens (\(Just v) -> v) (\_ v -> Just v)

getSelection :: SelectInputOverlay n i a -> ([i], Maybe (Either ErrorMessage T.Text))
getSelection (SelectInputOverlay {..}) =
  (map fst . filter snd . map snd . NE.toList $ fst _items
  , f (snd _items,  _editInput))
  where
    f (True, Just edi) = Just $ EditInput.editInputText edi
    f _ = Nothing
