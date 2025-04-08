{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.Widgets.InputField.EditInput where

import GHCup.Types (KeyCombination(..))
import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.Common as Common

import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.BasicOverlay
import GHCup.Brick.Widgets.InputField.Class

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Edit as Edit

import Data.Some
import qualified Data.Text                     as T

import qualified Graphics.Vty as Vty
import Optics (Lens', lens, (^.), (%))
import Optics.State.Operators ((%=), (.=), (?=))
import Optics.TH (makeLenses)

data EditInput n a = EditInput
  { _name :: n
  , _label :: T.Text
  , _overlay :: Maybe (Some (IsSubWidget n (EditInput n a)))
  , _editInputOverlay :: BasicOverlay n (EditInputOverlay n a)
  }

data EditInputOverlay n a = EditInputOverlay
  { _editor :: Edit.Editor T.Text n
  , _validator :: T.Text -> Either T.Text a
  , _helpMessage :: HelpMessage
  }

concat <$> mapM makeLenses [''EditInputOverlay, ''EditInput]

createEditInput :: (Eq n, Show n)
  => n
  -> T.Text
  -> HelpMessage
  -> (T.Text -> Either T.Text a)
  -> T.Text
  -> EditInput n a
createEditInput name label helpMsg validator initVal =
  EditInput name label Nothing
    (BasicOverlay (EditInputOverlay (Edit.editor name (Just 1) initVal) validator helpMsg) [KeyCombination Vty.KEnter []] (Common.frontwardLayer label))

instance (Ord n, Show n) => BaseWidget n (EditInput n a) where
  draw = const $ Brick.txt "EditInput"
  handleEvent ev = do
    (EditInput {..}) <- Brick.get
    case ev of
      VtyEvent (Vty.EvKey Vty.KEnter []) -> overlay ?= Some (IsSubWidget editInputOverlay)
      _ -> pure ()
    pure Nothing

  hasOverlay = _overlay
  closeOverlay = overlay .= Nothing

instance (Ord n, Show n) => InputField n (EditInput n a) where
  getLabel e = (_name e, _label e)
  drawInputField focus f e = Brick.txt $ T.unlines $ Edit.getEditContents $ _editor $ _innerWidget $ _editInputOverlay e

instance (Ord n, Show n) => BaseWidget n (EditInputOverlay n a) where
  draw e = Brick.vBox $
    [ Brick.txtWrap (_helpMessage e)
    , Border.border $ Edit.renderEditor (Brick.txt . T.unlines) True edi
    , case _validator e editorContents of
        Left msg -> Common.renderAsErrMsg msg
        _ -> Brick.txt " "
    , Brick.padRight Brick.Max $
        Brick.txt "Press Enter to go back"
    ]
    where
      edi = _editor e
      editorContents = T.unlines $ Edit.getEditContents edi

  handleEvent ev = do
    Common.zoom editor $ Edit.handleEditorEvent ev
    pure Nothing
