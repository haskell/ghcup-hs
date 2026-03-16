{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module InstallTest where

import Test.Tasty
import GHCup.OptParse hiding (HLSCompileOptions(isolateDir))
import Utils
import GHCup.Types
import Data.Versions
import Data.List.NonEmpty (NonEmpty ((:|)))

-- Some interests:
--   install ghc *won't* select `set as activate version` as default
--   install cabal *will* select `set as activate version` as default
--   install hls *will* select `set as activate version` as default
--   install stack *will* select `set as activate version` as default

installTests :: TestTree
installTests = testGroup "install"
  $ map
      (buildTestTree installParseWith)
      [ ("ghc", installGhcCheckList)
      , ("cabal", installCabalCheckList)
      , ("hls", installHlsCheckList)
      , ("stack", installStackCheckList)
      ]

defaultOptions :: InstallOptions
defaultOptions = InstallOptions Nothing Nothing False Nothing False Nothing []

-- | Don't set as active version
mkInstallOptions :: ToolVersion -> InstallOptions
mkInstallOptions ver = InstallOptions (Just ver) Nothing False Nothing False Nothing []

-- | Set as active version
mkInstallOptions' :: ToolVersion -> InstallOptions
mkInstallOptions' ver = InstallOptions (Just ver) Nothing True Nothing False Nothing []

installGhcCheckList :: [(String, InstallCommand)]
installGhcCheckList =
  ("install ghc", InstallGHC defaultOptions)
  : mapSecond (InstallGHC . mkInstallOptions)
    [ ("install ghc 9.2", GHCVersion
          $ GHCTargetVersion
            Nothing
            $(versionQ "9.2")
      )
    , ("install ghc next", GHCVersion
          $ GHCTargetVersion
            Nothing
            $(versionQ "next")
      )
    , ("install ghc latest", ToolTag Latest)
    , ("install ghc nightly", GHCVersion
          $ GHCTargetVersion
            Nothing
            $(versionQ "nightly")
      )
    , ("install ghc recommended", ToolTag Recommended)
    , ("install ghc prerelease", GHCVersion
          $ GHCTargetVersion
            Nothing
            $(versionQ "prerelease")
      )
    , ("install ghc latest-prerelease", ToolTag LatestPrerelease)
    , ("install ghc latest-nightly", ToolTag LatestNightly)
    , ("install ghc javascript-unknown-ghcjs-9.6", GHCVersion
          $ GHCTargetVersion
            (Just "javascript-unknown-ghcjs")
            $(versionQ "9.6")
      )
    , ("install ghc base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    ]

installCabalCheckList :: [(String, InstallCommand)]
installCabalCheckList =
  ("install cabal", InstallCabal (defaultOptions{ instSet = True } :: InstallOptions))
  : mapSecond (InstallCabal . mkInstallOptions')
    [ ("install cabal 3.10", ToolVersion $(versionQ "3.10"))
    , ("install cabal next", ToolVersion $(versionQ "next"))
    , ("install cabal latest", ToolTag Latest)
    , ("install cabal nightly", ToolVersion $(versionQ "nightly"))
    , ("install cabal recommended", ToolTag Recommended)
    , ("install cabal prerelease", ToolVersion $(versionQ "prerelease"))
    , ("install cabal latest-prerelease", ToolTag LatestPrerelease)
    , ("install cabal latest-nightly", ToolTag LatestNightly)
    , ("install cabal base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    ]

installHlsCheckList :: [(String, InstallCommand)]
installHlsCheckList =
  ("install hls", InstallHLS defaultOptions{instSet = True})
  : mapSecond (InstallHLS . mkInstallOptions')
    [ ("install hls 3.10", ToolVersion $(versionQ "3.10"))
    , ("install hls next", ToolVersion $(versionQ "next"))
    , ("install hls latest", ToolTag Latest)
    , ("install hls nightly", ToolVersion $(versionQ "nightly"))
    , ("install hls recommended", ToolTag Recommended)
    , ("install hls prerelease", ToolVersion $(versionQ "prerelease"))
    , ("install hls latest-prerelease", ToolTag LatestPrerelease)
    , ("install hls latest-nightly", ToolTag LatestNightly)
    , ("install hls base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    ]

installStackCheckList :: [(String, InstallCommand)]
installStackCheckList =
  ("install stack", InstallStack defaultOptions{instSet = True})
  : mapSecond (InstallStack . mkInstallOptions')
    [ ("install stack 3.10", ToolVersion $(versionQ "3.10"))
    , ("install stack next", ToolVersion $(versionQ "next"))
    , ("install stack latest", ToolTag Latest)
    , ("install stack nightly", ToolVersion $(versionQ "nightly"))
    , ("install stack recommended", ToolTag Recommended)
    , ("install stack prerelease", ToolVersion $(versionQ "prerelease"))
    , ("install stack latest-prerelease", ToolTag LatestPrerelease)
    , ("install stack latest-nightly", ToolTag LatestNightly)
    , ("install stack base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    ]

installParseWith :: [String] -> IO InstallCommand
installParseWith args = do
  Install a <- parseWith args
  pure a
