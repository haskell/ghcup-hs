{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
A very simple information-only widget with no handler. 
-}

module GHCup.Brick.Widgets.Tutorial (draw) where

import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.Attributes as Attributes

import Brick
    ( Padding(Max),
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import           Brick.Widgets.Center ( center )
import           Prelude                 hiding ( appendFile )



draw :: Widget Common.Name
draw =
  let
    mkTextBox = Brick.hLimitPercent 70 . Brick.vBox . fmap (Brick.padRight Brick.Max)

  in Common.frontwardLayer "Tutorial"
      $ Brick.vBox
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
        <=> Brick.padRight Brick.Max (Brick.txt "Press q to exit the tutorial")
