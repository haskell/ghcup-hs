{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : GHCup.Requirements
Description : Requirements utilities
Copyright   : (c) Julian Ospald, 2020
License     : GPL-3
Maintainer  : hasufell@hasufell.de
Stability   : experimental
Portability : POSIX
-}
module GHCup.Requirements where

import           GHCup.Types
import           GHCup.Types.JSON               ( )
import           GHCup.Types.Optics

import           Control.Applicative
import           Data.Maybe
import           Optics
import           Prelude                 hiding ( abs
                                                , readFile
                                                , writeFile
                                                )

import qualified Data.Text                     as T


-- | Get the requirements. Right now this combines GHC and cabal
-- and doesn't do fine-grained distinction. However, the 'ToolRequirements'
-- type allows it.
getCommonRequirements :: PlatformResult
                      -> ToolRequirements
                      -> Maybe Requirements
getCommonRequirements pr tr =
  preview (ix GHC % ix Nothing % ix (_platform pr) % ix (_distroVersion pr)) tr
    <|> preview (ix GHC % ix Nothing % ix (_platform pr) % ix Nothing) tr
    <|> preview
          ( ix GHC
          % ix Nothing
          % ix (set _Linux UnknownLinux $ _platform pr)
          % ix Nothing
          )
          tr


prettyRequirements :: Requirements -> T.Text
prettyRequirements Requirements {..} =
  let d = if not . null $ _distroPKGs
        then
          "\n  Install the following distro packages: "
            <> T.intercalate " " _distroPKGs
        else ""
      n = if not . T.null $ _notes then "\n  Note: " <> _notes else ""
  in  "System requirements " <> d <> n
