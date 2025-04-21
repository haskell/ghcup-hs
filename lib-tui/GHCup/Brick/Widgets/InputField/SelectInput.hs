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
import           Data.List (find)
import           Data.List.NonEmpty             ( NonEmpty (..) )
import qualified Data.List.NonEmpty            as NE
import Data.Some
import qualified Data.Text                     as T

import qualified Graphics.Vty as Vty
import Optics (Lens', lens, to, over, use, _1, (^.), (%), (&), (%~), (.~))
import Optics.State.Operators ((%=), (.=), (?=))
import Optics.TH (makeLenses)

data SelectInput n i a = SelectInput
  { _name :: n
  , _overlay :: Maybe (Some (IsSubWidget n (SelectInput n i a)))
  , _selectInputOverlay :: BasicOverlay n (SelectInputOverlay n i a)
  , _label :: T.Text
  , _helpMessage :: HelpMessage
  }

data SelectInputOverlay n i a = SelectInputOverlay
  { -- | All items along with their selected state
    -- And Bool to indicate if editable field is selected
    _items :: ([(Int, (i, Bool))], Bool)
    -- | Editable text field
  , _editInput :: Maybe (EditInput.EditInput n a)
  -- | Focus ring using integral values assigned to each item, text field is always last
  , _focusRing :: F.FocusRing Int
  , _showItem :: (i -> T.Text)
  -- | Update the selection; Int value is the focus ring's value when user pressed 'Enter'
  , _update :: (Int -> ([(Int, (i, Bool))], Bool) -> (([(Int, (i, Bool))]), Bool))
  , _menuKeys :: Common.MenuKeyBindings
  , _viewportName :: n
  }

concat <$> mapM makeLenses [''SelectInputOverlay, ''SelectInput]

createSelectInput :: (Eq n, Show n)
  => n
  -> T.Text
  -> HelpMessage
  -> NonEmpty i
  -> (i -> T.Text)
  -> Common.MenuKeyBindings
  -> SelectInput n i ()
createSelectInput name label helpMsg items showItem kb =
  SelectInput name Nothing
  (BasicOverlay overlay [kb ^. Common.mKbQuit] (Common.smallerOverlayLayer label))
  label helpMsg
  where
    overlay = SelectInputOverlay initState Nothing (F.focusRing [1.. totalRows]) showItem singleSelect kb name
    totalRows = length items
    initState = (zip [1..] $ fmap (,False) $ NE.toList items, False)

    singleSelect :: Int -> ([(Int, (i, Bool))], a) -> ([(Int, (i, Bool))], a)
    singleSelect ix = over _1 $ fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, True)) else (ix', (i, False)))

createMultiSelectInput :: (Eq n, Show n)
  => n
  -> T.Text
  -> HelpMessage
  -> NonEmpty i
  -> (i -> T.Text)
  -> Common.MenuKeyBindings
  -> SelectInput n i ()
createMultiSelectInput name label helpMsg items showItem kb =
  SelectInput name Nothing
  (BasicOverlay overlay [kb ^. Common.mKbQuit] (Common.smallerOverlayLayer label))
  label helpMsg
  where
    overlay = SelectInputOverlay initState Nothing (F.focusRing [1.. totalRows]) showItem multiSelect kb name
    totalRows = length items
    initState = (zip [1..] $ fmap (,False) $ NE.toList items, False)

    multiSelect :: Int -> ([(Int, (i, Bool))], a) -> ([(Int, (i, Bool))], a)
    multiSelect ix = over _1 $ fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, not b)) else (ix', (i, b)))

createSelectInputWithEditable :: (Eq n, Show n)
  => n
  -> n
  -> T.Text
  -> HelpMessage
  -> [i]
  -> (i -> T.Text)
  -> (T.Text -> Either ErrorMessage a)
  -> Common.MenuKeyBindings
  -> SelectInput n i a
createSelectInputWithEditable name editName label helpMsg items showItem validator kb =
  SelectInput name Nothing
  (BasicOverlay overlay [kb ^. Common.mKbQuit] (Common.smallerOverlayLayer label))
  label helpMsg
  where
    overlay = SelectInputOverlay initState (Just editInp) (F.focusRing [1..totalRows]) showItem singleSelect kb name
    totalRows = length items + 1
    initState = (zip [1..] $ fmap (,False) $ items, False)

    editInp = EditInput.create editName label helpMsg validator ""

    singleSelect :: Int -> ([(Int, (i, Bool))], Bool) -> ([(Int, (i, Bool))], Bool)
    singleSelect ix (ne, a) = (fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, True)) else (ix', (i, False))) ne, ix == length ne + 1)

createMultiSelectInputWithEditable :: (Eq n, Show n)
  => n
  -> n
  -> T.Text
  -> HelpMessage
  -> [i]
  -> (i -> T.Text)
  -> (T.Text -> Either ErrorMessage a)
  -> Common.MenuKeyBindings
  -> SelectInput n i a
createMultiSelectInputWithEditable name editName label helpMsg items showItem validator kb =
  SelectInput name Nothing
  (BasicOverlay overlay [kb ^. Common.mKbQuit] (Common.smallerOverlayLayer label))
  label helpMsg
  where
    overlay = SelectInputOverlay initState (Just editInp) (F.focusRing [1..totalRows]) showItem multiSelect kb name
    totalRows = length items + 1
    initState = (zip [1..] $ fmap (,False) $ items, False)

    editInp = EditInput.create editName label helpMsg validator ""

    multiSelect :: Int -> ([(Int, (i, Bool))], Bool) -> ([(Int, (i, Bool))], Bool)
    multiSelect ix (ne, a) = (fmap (\(ix', (i, b)) -> if ix' == ix then (ix', (i, True)) else (ix', (i, b))) ne, ix == length ne + 1)

instance (Ord n, Show n) => BaseWidget n (SelectInput n i a) where
  -- This is not used. See drawInputField
  draw = const $ Brick.txt "SelectInput draw"

  handleEvent ev = do
    case ev of
      VtyEvent (Vty.EvKey Vty.KEnter []) -> overlay ?= Some (IsSubWidget selectInputOverlay)
      _ -> pure ()
    pure Nothing

  hasOverlay = _overlay
  closeOverlay = overlay .= Nothing

instance (Ord n, Show n) => InputField n (SelectInput n i a) where
  getLabel e = (_name e, _label e)
  drawInputField focus f (SelectInput {..}) =
    let showItem = _showItem $ _innerWidget $ _selectInputOverlay
    in f $ case getSelection' (_innerWidget $ _selectInputOverlay) of
         ([], Nothing) -> (Brick.padLeft (Brick.Pad 1) . Common.renderAsHelpMsg $ _helpMessage)
         (_, Just (Left msg)) -> Brick.padLeft (Brick.Pad 1) $ Common.renderAsErrMsg msg
         (xs, Just (Right (_, txt))) -> Brick.hBox $
           fmap (Brick.padRight (Brick.Pad 1) . Brick.txt . showItem) xs
             ++ [Brick.txt txt]
         (xs, Nothing) -> Brick.hBox $ fmap (Brick.padRight (Brick.Pad 1) . Brick.txt . showItem) xs

instance (Ord n, Show n) => BaseWidget n (SelectInputOverlay n i a) where
  draw (SelectInputOverlay {..}) = Brick.vBox $
      [ Brick.txt "Press "
        <+> Common.keyToWidget (_menuKeys ^. Common.mKbQuit)
        <+> Brick.txt " to go back, Press Enter to select"
        <+> Brick.txt (if txtFieldFocused then ", Press e to edit" else "")
      , Brick.vLimit (totalRows) $ Brick.withVScrollBars Brick.OnRight
          $ Brick.viewport _viewportName Brick.Vertical
          $ Brick.vBox $ mEditableField ++ (fmap (mkSelectRow focused) (fst _items))
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
          <+> if Edit.getEditContents edi == [mempty]
                then Common.renderAslabel "(Specify custom text value)" focused
                else case EditInput.editInputText e of
                     Left err -> Common.renderAsErrMsg err
                     Right v -> Common.renderAslabel v focused
        where
          m = if selected then Brick.txt "*" else Brick.txt " "
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
      Just edi -> do
        Common.zoom selectInputOverlayEditInputJust closeOverlay
        -- Also select the text field entry
        let txtFieldRow = length (fst _items) + 1
        items %= _update txtFieldRow

-- Useful lens when we know the editInput is a Just value
selectInputOverlayEditInputJust = editInput % lens (\(Just v) -> v) (\_ v -> Just v)

getSelection :: SelectInput n i a -> ([i], Maybe (Either ErrorMessage (a, T.Text)))
getSelection = getSelection' . _innerWidget . _selectInputOverlay

getSelection' :: SelectInputOverlay n i a -> ([i], Maybe (Either ErrorMessage (a, T.Text)))
getSelection' (SelectInputOverlay {..}) =
  (map fst . filter snd . map snd $ fst _items
  , f (snd _items,  _editInput))
  where
    f (True, Just edi) = Just $ EditInput.editInputTextAndValue edi
    f _ = Nothing

-- | Replaces the list of items with the new one, while maintaining the selection from the old
updateItems :: forall n i a . (Eq i) => [i] -> SelectInput n i a -> SelectInput n i a
updateItems new s = s
  & selectInputOverlay % innerWidget % items % _1 %~ selectFromOld
  & selectInputOverlay % innerWidget % focusRing .~ F.focusRing [1..totalRows]
  where
    selectFromOld :: [(Int, (i, Bool))] -> [(Int, (i, Bool))]
    selectFromOld old = zip [1..] $ fmap (\i -> (i, isSelected i)) new
      where isSelected i = fromMaybe False $ fmap (snd . snd) $ find (\(_,(i', b)) -> i == i') old

    totalRows = length new + (if isJust (s ^. selectInputOverlay % innerWidget % editInput) then 1 else 0)
