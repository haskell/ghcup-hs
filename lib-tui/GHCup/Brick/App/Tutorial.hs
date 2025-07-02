{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHCup.Brick.App.Tutorial (Tutorial(..)) where

import GHCup.Brick.Widgets.BaseWidget
import qualified GHCup.Brick.App.Common as Common
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.Attributes as Attributes
import GHCup.Types (KeyCombination(..))

import Brick
    ( BrickEvent(..),
      Padding(Max),
      Widget(..),
      (<=>), (<+>))
import qualified Brick
import           Brick.Widgets.Center ( center )
import qualified Graphics.Vty as Vty
import           Prelude                 hiding ( appendFile )

data Tutorial = Tutorial
  { _quitKey :: KeyCombination
  }

instance BaseWidget Common.Name Tutorial where
  draw (Tutorial {..}) =
    let
      mkTextBox = Brick.hLimitPercent 70 . Brick.vBox . fmap (Brick.padRight Brick.Max)

    in Brick.vBox
          (fmap center
            [ mkTextBox [Brick.txtWrap "GHCup is a distribution channel for Haskell's tools."]
            , Common.separator
            , mkTextBox [
                Brick.hBox [
                  Brick.txt "This symbol "
                , Brick.withAttr Attributes.installedAttr (Brick.str Common.installedSign)
                , Brick.txtWrap " means that the tool is installed but not in used"
                ]
              , Brick.hBox [
                  Brick.txt "This symbol "
                , Brick.withAttr Attributes.setAttr (Brick.str Common.setSign)
                , Brick.txtWrap " means that the tool is installed and in used"
                ]
              , Brick.hBox [
                  Brick.txt "This symbol "
                , Brick.withAttr Attributes.notInstalledAttr (Brick.str Common.notInstalledSign)
                , Brick.txt " means that the tool isn't installed"
                ]
              ]
            , Common.separator
            , mkTextBox [
                Brick.hBox [
                  Brick.withAttr Attributes.recommendedAttr $ Brick.str "recommended"
                , Brick.txtWrap " tag is based on community adoption, known bugs, etc... So It makes this version the least experimental"
                ]
              , Brick.hBox [
                  Brick.withAttr Attributes.latestAttr $ Brick.str "latest"
                , Brick.txtWrap " tag is for the latest distributed version of the tool"
                ]
              , Brick.hBox [
                  Brick.withAttr Attributes.latestAttr $ Brick.str "hls-powered"
                , Brick.txt " denotes the compiler version supported by the currently set ("
                , Brick.withAttr Attributes.setAttr (Brick.str Common.setSign)
                , Brick.txt ") hls"
                ]
              , Brick.txtWrap "base-X.Y.Z.W tag is the minimun version of the base package admited in such ghc version"
              ]
            , Brick.txt " "
            ])
        <=> (Brick.padRight Brick.Max $
          Brick.txt "Press " <+> Common.keyToWidget _quitKey <+> Brick.txt " to exit the tutorial")

  handleEvent ev = do
    (Tutorial {..}) <- Brick.get
    case ev of
      VtyEvent (Vty.EvKey key mods)
        | _quitKey == KeyCombination key mods -> pure (Just CloseAllOverlays)
      _ -> pure Nothing
