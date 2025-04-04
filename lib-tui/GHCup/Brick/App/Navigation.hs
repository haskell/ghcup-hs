{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module GHCup.Brick.App.Navigation where

import GHCup.Brick.Actions
import qualified GHCup.Brick.Common as Common
import qualified GHCup.Brick.App.Common as Common
import GHCup.Brick.Widgets.BaseWidget
import qualified GHCup.Brick.Attributes as Attributes
import qualified GHCup.Brick.Widgets.SectionList as SectionList

import GHCup.List ( ListResult(..) )
import GHCup.Types
    ( GHCTargetVersion(GHCTargetVersion),
      Tool(..),
      Tag(..),
      KeyCombination (KeyCombination),
      tVerToText,
      tagToString )

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

import Control.Monad
import Control.Monad.Reader
import Data.Some
import Data.Vector ( Vector )
import qualified Graphics.Vty as Vty
import Optics (Lens', (^.), (%))
import Optics.TH (makeLenses)

import           Data.List ( intercalate, sort )
import           Data.Maybe ( mapMaybe )
import           Data.Vector ( Vector)
import           Data.Versions ( prettyPVP, prettyVer )
import           Prelude                 hiding ( appendFile )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V

data Navigation = Navigation
  { _sectionList :: NavigationList
  , _listResult    :: [ListResult]
  , _showAllVersions :: Bool
  , _attrMap :: AttrMap
  }

makeLenses ''Navigation

-- | How to create a navigation widget
create :: Common.Name                         -- The name of the section list
       -> [(Common.Name, Vector ListResult)]  -- a list of tuples (section name, collection of elements)
       -> Int                                 -- The height of each item in a list. Commonly 1
       -> AttrMap
       -> Navigation
create name elements height dimAttrs =
  let
  in Navigation
    { _sectionList = SectionList.sectionList name elements height
    , _listResult = []
    , _showAllVersions = False
    , _attrMap = dimAttrs
    }

instance BaseWidget Common.Name Navigation where
  draw (Navigation {..}) = drawNav _attrMap _sectionList

  handleEvent ev = do
    pure Nothing


-- | How to draw the navigation widget
drawNav :: AttrMap -> NavigationList -> Widget Common.Name
drawNav dimAttrs section_list
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
  renderItem minTagSize minVerSize listIx b listResult@ListResult{lTag = lTag', ..} =
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
        active = if b then Common.enableScreenReader (Common.ListItem lTool listIx) else id
    in Brick.clickable (Common.ListItem lTool listIx) $ hooray $ active $ dim
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
  printTag Experimental     = Just $ Brick.withAttr Attributes.latestNightlyAttr $ Brick.str "experimental"
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
