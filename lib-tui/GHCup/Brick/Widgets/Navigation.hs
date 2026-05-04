{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

{- Brick's navigation widget:
It is a FocusRing over many list's. Each list contains the information for each tool. Each list has an internal name (for Brick's runtime)
and a label which we can use in rendering. This data-structure helps to reuse Brick.Widget.List and to navegate easily across

-}


module GHCup.Brick.Widgets.Navigation (BrickInternalState, handler, draw) where

import GHCup.Command.List ( ListResult(..) )
import GHCup.Types
    ( TargetVersion(TargetVersion),
      Tool(..),
      Tag(..),
      tVerToText,
      tagToString, ToolDescription )
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.Attributes as Attributes
import Brick
    ( BrickEvent(..),
      Padding(Max, Pad),
      AttrMap,
      EventM,
      Widget(..),
      (<+>),
      (<=>))
import qualified Brick
import           Brick.Widgets.Border ( hBorder, borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style ( unicode )
import           Brick.Widgets.Center ( center )
import qualified Brick.Widgets.List as L
import           Data.List ( intercalate, sort )
import           Data.Maybe ( mapMaybe )
import           Data.Versions ( prettyPVP, prettyVer )
import           Prelude                 hiding ( appendFile )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Control.Monad.State.Class (get, modify)
import qualified Graphics.Vty           as Vty

type BrickList = L.GenericList Common.Name V.Vector

type BrickInternalState = BrickList (Tool, (Maybe ToolDescription, BrickList ListResult))

-- | How the navigation handler handle events
handler :: Bool -> BrickEvent Common.Name e -> EventM Common.Name BrickInternalState ()
handler False (Brick.VtyEvent e) = L.handleListEvent e
handler True (Brick.VtyEvent e) = do
  bis :: BrickInternalState <- get
  case L.listSelectedElement bis of
    Nothing -> pure ()
    Just (_, (t, (td, vlr))) -> do
      updatedVlr <- Brick.nestEventM' vlr (handleVersionEvent e)
      modify (L.listModify $ (fmap . fmap) (const updatedVlr))
 where
  -- need to reverse because we reversed the list
  handleVersionEvent (Vty.EvKey Vty.KUp [])   = L.handleListEvent (Vty.EvKey Vty.KDown [])
  handleVersionEvent (Vty.EvKey Vty.KDown []) = L.handleListEvent (Vty.EvKey Vty.KUp [])
  handleVersionEvent (Vty.EvKey Vty.KPageDown [])   = L.handleListEvent (Vty.EvKey Vty.KPageUp [])
  handleVersionEvent (Vty.EvKey Vty.KPageUp [])   = L.handleListEvent (Vty.EvKey Vty.KPageDown [])
  handleVersionEvent (Vty.EvKey Vty.KHome [])   = L.handleListEvent (Vty.EvKey Vty.KEnd [])
  handleVersionEvent (Vty.EvKey Vty.KEnd [])   = L.handleListEvent (Vty.EvKey Vty.KHome [])
  handleVersionEvent e' = L.handleListEvent e'
handler _ _ = pure ()


-- | How to draw the navigation widget
draw :: Bool -> AttrMap -> BrickInternalState -> Widget Common.Name
draw versionFocus dimAttrs bis
  = Brick.padBottom Max
      ( Brick.joinBorders $ Brick.withBorderStyle unicode
        $ borderWithLabel (Brick.str "GHCup")
          (center (Brick.vLimit 1 header <=> hBorder <=> renderList'))
      )
 where
  minHSize s' = Brick.hLimit s' . Brick.vLimit 1 . (<+> Brick.fill ' ')
  allElements = L.listElements bis
  minToolSize = V.maximum $ V.map (length . prettyShow . fst) allElements
  selectedTool = fmap snd $ L.listSelectedElement bis
  minTagSize = maybe 0 (\(t, (_, vlr)) -> V.maximum $ V.map (length . intercalate "," . fmap tagToString . lTag) $ L.listElements vlr) selectedTool
  minVerSizeList = maybe 0 (\(t, (_, vlr)) ->  V.maximum $ V.map (\ListResult{..} -> T.length $ tVerToText (TargetVersion lCross lVer)) $ L.listElements vlr) selectedTool
  minVerHeaderSize = length $ maybe "Versions" (\(fst -> t) -> prettyShow t <> " versions") selectedTool
  minVerSize = max minVerSizeList minVerHeaderSize
  header =
      Brick.padLeft (Pad 1) (minHSize (minToolSize + 2) (Brick.str "Tool"))
      <+> vBorder
      <+> Brick.padLeft (Pad 1) (minHSize (minVerSize + 2) (maybe (Brick.str "Versions") (\(fst -> t) -> printTool t <+> Brick.str " versions") selectedTool))
      <+> Brick.padLeft (Pad 2) (minHSize minTagSize (Brick.str "Tags"))
      <+> Brick.padLeft (Pad 5) (Brick.str "Notes")

  renderList' =
    let toolColumn = Brick.hLimit (minToolSize + 2)
          (Brick.withDefAttr L.listAttr (L.renderList renderTool (not versionFocus) bis))
        versionColumn = maybe Brick.emptyWidget
          (\(t, (_, vlr)) ->
            Brick.withDefAttr L.listAttr
                $ L.renderListWithIndex (renderItem t) versionFocus
                $ L.listReverse vlr
          )
          selectedTool
    in Brick.padLeft (Pad 1) toolColumn
       <+> vBorder
       <+> Brick.padLeft (Pad 1) versionColumn

  renderTool :: Bool -> (Tool, (Maybe ToolDescription, BrickList ListResult)) -> Widget Common.Name
  renderTool b (t, (tDesc, _)) = minHSize minToolSize $ printTool t

  renderItem t listIx b listResult@ListResult{lTag = lTag', ..} =
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
        active = if b then Common.enableScreenReader (Common.ListItem t listIx) else id
    in Brick.clickable (Common.ListItem t listIx) $ hooray $ active $ dim
          (   marks
          <+> Brick.padLeft (Pad 1) (minHSize minVerSize (Brick.str ver))
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
  printTag Experimental     = Just $ Brick.withAttr Attributes.latestNightlyAttr $ Brick.str "experimental"
  printTag (UnknownTag t) = Just $ Brick.str t

  printTool (Tool "cabal") = Brick.str "cabal"
  printTool (Tool "ghc") = Brick.str "GHC"
  printTool (Tool "ghcup") = Brick.str "GHCup"
  printTool (Tool "hls") = Brick.str "HLS"
  printTool (Tool "stack") = Brick.str "Stack"
  printTool (Tool t) = Brick.str t

  printNotes ListResult {..} =
    (if hlsPowered then [Brick.withAttr Attributes.hlsPoweredAttr $ Brick.str "hls-powered"] else mempty
      )
      ++ (if lStray then [Brick.withAttr Attributes.strayAttr $ Brick.str "stray"] else mempty)
      ++ (case lReleaseDay of
            Nothing -> mempty
            Just d  -> [Brick.withAttr Attributes.dayAttr $ Brick.str (show d)])

