{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Requirements
Description : Requirements utilities
Copyright   : (c) Julian Ospald, 2020
License     : LGPL-3.0
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : portable
-}
module GHCup.Requirements where

import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics
import           GHCup.Version

import           Control.Applicative
import           Data.List                      ( find )
import           Data.Maybe
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T


-- | Get the requirements. Right now this combines GHC and cabal
-- and doesn't do fine-grained distinction. However, the 'ToolRequirements'
-- type allows it.
getCommonRequirements :: PlatformResult
                      -> ToolRequirements
                      -> Maybe Requirements
getCommonRequirements pr tr =
  with_distro <|> without_distro_ver <|> without_distro
 where
  with_distro        = distro_preview _platform _distroVersion
  without_distro_ver = distro_preview _platform (const Nothing)
  without_distro     = distro_preview (set _Linux UnknownLinux . _platform) (const Nothing)

  distro_preview f g =
    let platformVersionSpec =
          preview (ix GHC % ix Nothing % ix (f pr)) tr
        mv' = g pr
    in  fmap snd
          .   find
                (\(mverRange, _) -> maybe
                  (isNothing mv')
                  (\range -> maybe False (`versionRange` range) mv')
                  mverRange
                )
          .   M.toList
          =<< platformVersionSpec


prettyRequirements :: Requirements -> T.Text
prettyRequirements Requirements {..} =
  let d = if not . null $ _distroPKGs
        then
          "\n  Please install the following distro packages: "
            <> T.intercalate " " _distroPKGs
        else ""
      n = if not . T.null $ _notes then "\n  Note: " <> _notes else ""
  in  "System requirements " <> d <> n
