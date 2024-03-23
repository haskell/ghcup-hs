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
This module defined the attributes. Despite of brick's capability to have a hierarchy of attributes, here
we go for the most-simple-approach: a plain hierarchy
-}

module GHCup.Brick.Attributes where

import           Brick    ( AttrMap)
import qualified Brick
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty                  as Vty

defaultAttributes :: Bool -> AttrMap
defaultAttributes no_color = Brick.attrMap
  Vty.defAttr
  [ (L.listSelectedFocusedAttr , Vty.defAttr `withBackColor` Vty.blue)
  , (L.listSelectedAttr        , Vty.defAttr)
  , (notInstalledAttr          , Vty.defAttr `withForeColor` Vty.red)
  , (setAttr                   , Vty.defAttr `withForeColor` Vty.green)
  , (installedAttr             , Vty.defAttr `withForeColor` Vty.green)
  , (recommendedAttr           , Vty.defAttr `withForeColor` Vty.green)
  , (hlsPoweredAttr            , Vty.defAttr `withForeColor` Vty.green)
  , (latestAttr                , Vty.defAttr `withForeColor` Vty.yellow)
  , (latestPrereleaseAttr      , Vty.defAttr `withForeColor` Vty.red)
  , (latestNightlyAttr         , Vty.defAttr `withForeColor` Vty.red)
  , (prereleaseAttr            , Vty.defAttr `withForeColor` Vty.red)
  , (nightlyAttr               , Vty.defAttr `withForeColor` Vty.red)
  , (compiledAttr              , Vty.defAttr `withForeColor` Vty.blue)
  , (strayAttr                 , Vty.defAttr `withForeColor` Vty.blue)
  , (dayAttr                   , Vty.defAttr `withForeColor` Vty.blue)
  , (helpAttr                  , Vty.defAttr `withStyle`     Vty.italic)
  , (hoorayAttr                , Vty.defAttr `withForeColor` Vty.brightWhite)
  , (helpMsgAttr               , Vty.defAttr `withForeColor` Vty.brightBlack)
  , (errMsgAttr                , Vty.defAttr `withForeColor` Vty.red)
  ]
  where
    withForeColor | no_color  = const
                  | otherwise = Vty.withForeColor
    withBackColor | no_color  = \attr _ -> attr `Vty.withStyle` Vty.reverseVideo
                  | otherwise = Vty.withBackColor
    withStyle                 = Vty.withStyle


notInstalledAttr, setAttr, installedAttr, recommendedAttr, hlsPoweredAttr :: Brick.AttrName
latestAttr, latestPrereleaseAttr, latestNightlyAttr, prereleaseAttr, nightlyAttr :: Brick.AttrName
compiledAttr, strayAttr, dayAttr, helpAttr, hoorayAttr, helpMsgAttr, errMsgAttr :: Brick.AttrName

notInstalledAttr = Brick.attrName "not-installed"
setAttr = Brick.attrName "set"
installedAttr = Brick.attrName "installed"
recommendedAttr = Brick.attrName "recommended"
hlsPoweredAttr = Brick.attrName "hls-powered"
latestAttr = Brick.attrName "latest"
latestPrereleaseAttr = Brick.attrName "latest-prerelease"
latestNightlyAttr = Brick.attrName "latest-nightly"
prereleaseAttr = Brick.attrName "prerelease"
nightlyAttr = Brick.attrName "nightly"
compiledAttr = Brick.attrName "compiled"
strayAttr = Brick.attrName "stray"
dayAttr = Brick.attrName "day"
helpAttr = Brick.attrName "help"
hoorayAttr = Brick.attrName "hooray"
helpMsgAttr = Brick.attrName "helpMsg"
errMsgAttr = Brick.attrName "errMsg"

dimAttributes :: Bool -> AttrMap
dimAttributes no_color = Brick.attrMap
  (Vty.defAttr `Vty.withStyle` Vty.dim)
  [ (Brick.attrName "active"    , Vty.defAttr `withBackColor` Vty.blue) -- has no effect ??
  , (Brick.attrName "no-bindist", Vty.defAttr `Vty.withStyle` Vty.dim)
  ]
  where
    withBackColor | no_color  = \attr _ -> attr `Vty.withStyle` Vty.reverseVideo
                  | otherwise = Vty.withBackColor
