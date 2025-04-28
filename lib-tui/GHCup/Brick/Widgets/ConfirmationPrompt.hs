{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHCup.Brick.Widgets.ConfirmationPrompt where

import GHCup.Types
import qualified GHCup.Brick.Common as Common

import GHCup.Brick.Widgets.BaseWidget
import GHCup.Brick.Widgets.BasicOverlay

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

type ConfirmationPromptOverlay n = BasicOverlay n (ConfirmationPrompt n)

-- | This creates an overlay which shows user the provided message and runs the
-- action if the user presses 'Enter'
create :: T.Text
  -> T.Text
  -> Brick.EventM n () (Maybe HandleEventResult)
  -> KeyCombination
  -> ConfirmationPromptOverlay n
create overlayTitle message action quitKey =
  BasicOverlay prompt [quitKey] (Common.smallerOverlayLayer overlayTitle)
  where
    prompt = ConfirmationPrompt message action quitKey


data ConfirmationPrompt n = ConfirmationPrompt
  { _message :: T.Text
  , _action :: Brick.EventM n () (Maybe HandleEventResult)
  , _quitKey :: KeyCombination
  }

instance (Ord n, Show n) => BaseWidget n (ConfirmationPrompt n) where
  draw ConfirmationPrompt {..} = Brick.vBox
      [ Brick.txtWrap _message
      , Common.separator
      , Brick.padRight Brick.Max $
          Brick.txt "Press "
          <+> Common.keyToWidget _quitKey
          <+> Brick.txt " to go back, Press Enter to continue"
      ]

  handleEvent ev = do
    (ConfirmationPrompt {..}) <- Brick.get
    case ev of
      VtyEvent (Vty.EvKey Vty.KEnter []) -> snd <$> Brick.nestEventM () _action
      _ -> pure Nothing
