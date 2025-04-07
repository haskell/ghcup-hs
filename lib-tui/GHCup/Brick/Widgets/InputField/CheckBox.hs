{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.Widgets.InputField.CheckBox where

import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.Common as Common

import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.InputField.Class

import Brick
    ( BrickEvent(..),
      EventM,
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import qualified Data.Text                     as T

import qualified Graphics.Vty as Vty
import Optics (Lens', lens, (^.), (%))
import Optics.State.Operators ((%=), (.=), (?=))
import Optics.TH (makeLenses)

data CheckBoxInput n a = CheckBoxInput
  { _name :: n
  , _label :: T.Text
  , _helpMessage :: HelpMessage
  , _checked :: Bool
  }

makeLenses ''CheckBoxInput

instance (Ord n, Show n) => BaseWidget n (CheckBoxInput n Bool) where
  draw = const $ Brick.txt "CheckBoxInput"

  handleEvent ev = do
    case ev of
      VtyEvent (Vty.EvKey Vty.KEnter []) -> Common.zoom checked (Brick.modify not)
      _ -> pure ()
    pure Nothing

instance (Ord n, Show n) => InputField n (CheckBoxInput n Bool) where
  drawInputField focus f (CheckBoxInput {..}) = if focus
      then core
      else core <+> (Brick.padLeft (Brick.Pad 1) . Common.renderAsHelpMsg $ _helpMessage)
    where
      core = f $ drawBool _checked
      border w = Brick.txt "[" <+> (Brick.padRight (Brick.Pad 1) $ Brick.padLeft (Brick.Pad 2) w) <+> Brick.txt "]"
      drawBool b =
        if b
          then border . Brick.withAttr Attributes.installedAttr    $ Brick.str Common.checkBoxSelectedSign
          else border . Brick.withAttr Attributes.notInstalledAttr $ Brick.str Common.notInstalledSign

  getLabel e = (_name e, _label e)
