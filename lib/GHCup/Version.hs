{-# LANGUAGE QuasiQuotes       #-}


module GHCup.Version where

import           GHCup.Utils.Version.QQ

import           Data.Versions

ghcUpVer :: PVP
ghcUpVer = [pver|0.0.0|]
