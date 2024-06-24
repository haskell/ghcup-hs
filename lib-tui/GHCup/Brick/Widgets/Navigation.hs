{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- Brick's navigation widget:
It is a FocusRing over many list's. Each list contains the information for each tool. Each list has an internal name (for Brick's runtime)
and a label which we can use in rendering. This data-structure helps to reuse Brick.Widget.List and to navegate easily across

-}


module GHCup.Brick.Widgets.Navigation (BrickInternalState, create, handler, draw) where

import GHCup.List ( ListResult(..) )
import GHCup.Types
    ( GHCTargetVersion(GHCTargetVersion),
      Tool(..),
      Tag(..),
      tVerToText,
      tagToString )
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.Widgets.SectionList as SectionList
import Brick
    ( BrickEvent(..),
      Padding(Max, Pad),
      AttrMap,
      EventM,
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import           Brick.Widgets.Border ( hBorder, borderWithLabel)
import           Brick.Widgets.Border.Style ( unicode )
import           Brick.Widgets.Center ( center )
import qualified Brick.Widgets.List as L
import           Data.List ( intercalate, sort )
import           Data.Maybe ( mapMaybe )
import           Data.Vector ( Vector)
import           Data.Versions ( prettyPVP, prettyVer )
import           Prelude                 hiding ( appendFile )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V


type BrickInternalState = SectionList.SectionList Common.Name ListResult

-- | How to create a navigation widget
create :: Common.Name                         -- The name of the section list
       -> [(Common.Name, Vector ListResult)]  -- a list of tuples (section name, collection of elements)
       -> Int                                 -- The height of each item in a list. Commonly 1
       -> BrickInternalState
create = SectionList.sectionList

-- | How the navigation handler handle events
handler :: BrickEvent Common.Name e -> EventM Common.Name BrickInternalState ()
handler = SectionList.handleGenericListEvent

-- | How to draw the navigation widget
draw :: AttrMap -> BrickInternalState -> Widget Common.Name
draw dimAttrs section_list
  = Brick.padBottom Max
      ( Brick.withBorderStyle unicode
        $ borderWithLabel (Brick.str "GHCup")
          (center (header <=> hBorder <=> renderList' section_list))
      )
 where
  header =
    minHSize 2 Brick.emptyWidget
      <+> Brick.padLeft (Pad 2) (minHSize 6 $ Brick.str "Tool")
      <+> minHSize 15 (Brick.str "Version")
      <+> Brick.padLeft (Pad 1) (minHSize 25 $ Brick.str "Tags")
      <+> Brick.padLeft (Pad 5) (Brick.str "Notes")
  renderList' bis =
    let allElements = V.concatMap L.listElements $ SectionList.sectionListElements bis
        minTagSize = V.maximum $ V.map (length . intercalate "," . fmap tagToString . lTag) allElements
        minVerSize = V.maximum $ V.map (\ListResult{..} -> T.length $ tVerToText (GHCTargetVersion lCross lVer)) allElements
    in Brick.withDefAttr L.listAttr $ SectionList.renderSectionList (renderItem minTagSize minVerSize) True bis
  renderItem minTagSize minVerSize b listResult@ListResult{lTag = lTag', ..} =
    let marks = if
          | lSet       -> (Brick.withAttr Attributes.setAttr $ Brick.str Common.setSign)
          | lInstalled -> (Brick.withAttr Attributes.installedAttr $ Brick.str Common.installedSign)
          | otherwise  -> (Brick.withAttr Attributes.notInstalledAttr $ Brick.str Common.notInstalledSign)
        ver = case lCross of
          Nothing -> T.unpack . prettyVer $ lVer
          Just c  -> T.unpack (c <> "-" <> prettyVer lVer)
        dim
          | lNoBindist && not lInstalled
            && not b -- TODO: overloading dim and active ignores active
                       --       so we hack around it here
          = Brick.updateAttrMap (const dimAttrs) . Brick.withAttr (Brick.attrName "no-bindist")
          | otherwise  = id
        hooray
          | elem Latest lTag' && not lInstalled =
              Brick.withAttr Attributes.hoorayAttr
          | otherwise = id
        active = if b then Common.enableScreenReader Common.AllTools else id
    in  hooray $ active $ dim
          (   marks
          <+> Brick.padLeft (Pad 2)
               ( minHSize 6
                 (printTool lTool)
               )
          <+> minHSize minVerSize (Brick.str ver)
          <+> (let l = mapMaybe printTag $ sort lTag'
               in  Brick.padLeft (Pad 1) $ minHSize minTagSize $ if null l
                     then Brick.emptyWidget
                     else foldr1 (\x y -> x <+> Brick.str "," <+> y) l
              )
          <+> Brick.padLeft (Pad 5)
              ( let notes = printNotes listResult
                in  if null notes
                      then Brick.emptyWidget
                      else foldr1 (\x y -> x <+> Brick.str "," <+> y) notes
              )
          <+> Brick.vLimit 1 (Brick.fill ' ')
          )

  printTag Recommended    = Just $ Brick.withAttr Attributes.recommendedAttr $ Brick.str "recommended"
  printTag Latest         = Just $ Brick.withAttr Attributes.latestAttr $ Brick.str "latest"
  printTag Prerelease     = Just $ Brick.withAttr Attributes.prereleaseAttr $ Brick.str "prerelease"
  printTag Nightly        = Just $ Brick.withAttr Attributes.nightlyAttr $ Brick.str "nightly"
  printTag (Base pvp'')   = Just $ Brick.str ("base-" ++ T.unpack (prettyPVP pvp''))
  printTag Old            = Nothing
  printTag LatestPrerelease = Just $ Brick.withAttr Attributes.latestPrereleaseAttr $ Brick.str "latest-prerelease"
  printTag LatestNightly    = Just $ Brick.withAttr Attributes.latestNightlyAttr $ Brick.str "latest-nightly"
  printTag (UnknownTag t) = Just $ Brick.str t

  printTool Cabal = Brick.str "cabal"
  printTool GHC = Brick.str "GHC"
  printTool GHCup = Brick.str "GHCup"
  printTool HLS = Brick.str "HLS"
  printTool Stack = Brick.str "Stack"

  printNotes ListResult {..} =
    (if hlsPowered then [Brick.withAttr Attributes.hlsPoweredAttr $ Brick.str "hls-powered"] else mempty
      )
      ++ (if lStray then [Brick.withAttr Attributes.strayAttr $ Brick.str "stray"] else mempty)
      ++ (case lReleaseDay of
            Nothing -> mempty
            Just d  -> [Brick.withAttr Attributes.dayAttr $ Brick.str (show d)])

  minHSize s' = Brick.hLimit s' . Brick.vLimit 1 . (<+> Brick.fill ' ')