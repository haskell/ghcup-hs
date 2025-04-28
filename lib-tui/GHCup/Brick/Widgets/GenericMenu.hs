{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module GHCup.Brick.Widgets.GenericMenu where

import qualified GHCup.Brick.Common as Common
import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.InputField.Class
import GHCup.Types (KeyCombination (KeyCombination))

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import qualified Brick.Focus as F

import Control.Monad
import Control.Monad.Reader
import           Data.Maybe
import           Data.List (find, foldl')
import Data.Some
import qualified Data.Text                     as T

import GHC.Generics (Generic, Rep, from, to)

import qualified Graphics.Vty as Vty
import Optics (Lens', lens, (^.), (%))
import Optics.State.Operators ((%=), (.=), (?=))
import Optics.TH (makeLenses)


data GenericMenu n fs s a = GenericMenu
  { _fields :: fs n
  , _focusRing :: F.FocusRing n
  , _getOutput :: fs n -> Either ErrorMessage a
  , _menuKeys :: Common.MenuKeyBindings
  , _state :: s
  , _submitAction :: s -> a -> EventM n (GenericMenu n fs s a) ()
  , _submitButton :: Button n
  , _name :: n
  , _title :: T.Text
  , _overlay :: Maybe (Some (IsSubWidget n (GenericMenu n fs s a)))
  }

data Button n = Button
  { _buttonName :: n
  , _label :: T.Text
  , _helpMessage :: HelpMessage
  }

makeLenses ''GenericMenu

mkGenericMenu :: (Generic (fs n), GInputFields n (Rep (fs n)))
  => n
  -> fs n
  -> (fs n -> Either ErrorMessage a)
  -> s
  -> (s -> a -> EventM n (GenericMenu n fs s a) ())
  -> Common.MenuKeyBindings
  -> T.Text
  -> Button n
  -> GenericMenu n fs s a
mkGenericMenu n fs getOutput initState action kb title submitButton = GenericMenu
  { _fields = fs
  , _focusRing = F.focusRing $ (_buttonName submitButton) : (map fst $ getLabels $ GHC.Generics.from fs)
  , _getOutput = getOutput
  , _menuKeys = kb
  , _state = initState
  , _submitAction = action
  , _submitButton = submitButton
  , _name = n
  , _title = title
  , _overlay = Nothing
  }


instance (Generic (fs n), GInputFields n (Rep (fs n)), Ord n, Show n) => BaseWidget n (GenericMenu n fs s a) where
  draw (GenericMenu { .. }) = Brick.vBox
      [ Brick.vBox buttonWidgets
      , Common.separator
      , Brick.vLimit (length fieldLabels) $ Brick.withVScrollBars Brick.OnRight
          $ Brick.viewport _name Brick.Vertical
          $ Brick.vBox fieldWidgetsWithLabels
      , Brick.txt " "
      , Brick.padRight Brick.Max $
          Brick.txt "Press "
          <+> Common.keyToWidget (_menuKeys ^. Common.mKbQuit)
          <+> Brick.txt " to go back, Press Enter to edit the highlighted field"
      ]
    where
      gFields = GHC.Generics.from _fields
      fieldLabels = getLabels gFields

      maxWidth = foldl' max 5 (fmap Brick.textWidth $ map snd fieldLabels)

      fieldWidgetsWithLabels = zipWith drawOneField fieldLabels (gDrawInputFields currentFocus ampF gFields)
      drawOneField (n, l) f = Common.rightify (maxWidth + 1) (Common.renderAslabel l (n == currentFocus) <+> Brick.txt " ") <+> f

      ampF fieldName True field = Common.enableScreenReader fieldName $ Brick.visible field
      ampF _ _ field = field

      submitButtonName = _buttonName $ _submitButton
      currentFocus = fromMaybe submitButtonName $ F.focusGetCurrent _focusRing

      buttonWidgets =
        let focused = submitButtonName == currentFocus
            submitButton = ampF submitButtonName focused $ Common.renderAslabel "Submit" focused
            errMsg = case _getOutput _fields of
              Right _ -> Common.renderAsHelpMsg (_helpMessage _submitButton)
              Left msg -> Common.renderAsErrMsg msg
        in [drawOneField (submitButtonName, _label _submitButton) errMsg]

  handleEvent ev = do
    (GenericMenu { .. }) <- Brick.get
    let
      gFields = GHC.Generics.from _fields
      submitButtonName = _buttonName $ _submitButton
      currentFocus = fromMaybe submitButtonName $ F.focusGetCurrent _focusRing

    case ev of
      VtyEvent (Vty.EvKey k m)
        | KeyCombination k m == _menuKeys ^. Common.mKbUp    -> do
            focusRing %= F.focusPrev
            pure Nothing
        | KeyCombination k m == _menuKeys ^. Common.mKbDown  -> do
            focusRing %= F.focusNext
            pure Nothing
      VtyEvent (Vty.EvKey Vty.KEnter [])
        | currentFocus == submitButtonName -> case _getOutput _fields of
            Right v -> _submitAction _state v >> pure (Just CloseAllOverlays)
            Left _ -> pure Nothing
      _ -> do
        (_, (newFields, res)) <- Brick.nestEventM gFields $ gHandleEvent currentFocus gFields ev
        Brick.modify $ \s -> s { _fields = GHC.Generics.to newFields }
        pure Nothing

  hasOverlay (GenericMenu { .. }) = case gHasOverlay (GHC.Generics.from _fields) of
    Nothing -> _overlay
    Just (Some (IsSubWidget accessor)) -> Just (Some (IsSubWidget $ fields % lens GHC.Generics.from (const GHC.Generics.to) % accessor))

  closeOverlay = do
    (GenericMenu { .. }) <- Brick.get
    (_, newFields) <- Brick.nestEventM (GHC.Generics.from _fields) $ gCloseOverlay
    Brick.modify $ \s -> s { _fields = GHC.Generics.to newFields }
    overlay .= Nothing
    pure ()
