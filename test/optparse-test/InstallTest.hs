{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module InstallTest where

import Test.Tasty
import GHCup.OptParse hiding (HLSCompileOptions(isolateDir))
import Utils
import GHCup.Types
import Data.Versions
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHCup.OptParse.Install as Install
import URI.ByteString.QQ

-- Some interests:
--   install ghc *won't* select `set as activate version` as default
--   install cabal *will* select `set as activate version` as default
--   install hls *will* select `set as activate version` as default
--   install stack *will* select `set as activate version` as default

installTests :: TestTree
installTests = testGroup "install"
  $ map
      (buildTestTree installParseWith)
      [ ("old-style", oldStyleCheckList)
      , ("ghc", installGhcCheckList)
      , ("cabal", installCabalCheckList)
      , ("hls", installHlsCheckList)
      , ("stack", installStackCheckList)
      ]

defaultOptions :: InstallOptions
defaultOptions = InstallOptions Nothing Nothing False Nothing False []

-- | Don't set as active version
mkInstallOptions :: ToolVersion -> InstallOptions
mkInstallOptions ver = InstallOptions (Just ver) Nothing False Nothing False []

-- | Set as active version
mkInstallOptions' :: ToolVersion -> InstallOptions
mkInstallOptions' ver = InstallOptions (Just ver) Nothing True Nothing False []

oldStyleCheckList :: [(String, Either InstallCommand InstallOptions)]
oldStyleCheckList =
      ("install", Right defaultOptions)
    : ("install --set", Right defaultOptions{instSet = True})
    : ("install --force", Right defaultOptions{forceInstall = True})
#ifdef IS_WINDOWS
    : ("install -i C:\\\\", Right defaultOptions{Install.isolateDir = Just "C:\\\\"})
#else
    : ("install -i /", Right defaultOptions{Install.isolateDir = Just "/"})
#endif
    : ("install -u https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz head"
    , Right defaultOptions
        { instBindist = Just [uri|https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz|]
        , instVer = Just $ GHCVersion $ GHCTargetVersion Nothing  $(verQ "head")
        }
    )
    : mapSecond
        (Right . mkInstallOptions)
        [ ("install ghc-9.2", GHCVersion
              $ GHCTargetVersion
                (Just "ghc")
                $(verQ "9.2")
          )
          -- invalid
        , ("install next", GHCVersion
              $ GHCTargetVersion
                Nothing
                $(verQ "next")
          )
        , ("install latest", ToolTag Latest)
        , ("install nightly", GHCVersion
              $ GHCTargetVersion
                Nothing
                $(verQ "nightly")
          )
        , ("install recommended", ToolTag Recommended)
        , ("install prerelease", GHCVersion
              $ GHCTargetVersion
                Nothing
                $(verQ "prerelease")
          )
        , ("install latest-prerelease", ToolTag LatestPrerelease)
        , ("install latest-nightly", ToolTag LatestNightly)
        , ("install ghc-javascript-unknown-ghcjs-9.6", GHCVersion
              $ GHCTargetVersion
                (Just "ghc-javascript-unknown-ghcjs")
                $(verQ "9.6")
          )
        , ("install base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
        , ("install cabal-3.10", GHCVersion
              $ GHCTargetVersion
                (Just "cabal")
                $(verQ "3.10")
          )
        , ("install hls-2.0.0.0", GHCVersion
              $ GHCTargetVersion
                (Just "hls")
                $(verQ "2.0.0.0")
          )
        , ("install stack-2.9.3", GHCVersion
              $ GHCTargetVersion
                (Just "stack")
                $(verQ "2.9.3")
          )
        ]

installGhcCheckList :: [(String, Either InstallCommand InstallOptions)]
installGhcCheckList =
  ("install ghc", Left $ InstallGHC defaultOptions)
  : mapSecond (Left . InstallGHC . mkInstallOptions)
    [ ("install ghc 9.2", GHCVersion
          $ GHCTargetVersion
            Nothing
            $(verQ "9.2")
      )
    , ("install ghc next", GHCVersion
          $ GHCTargetVersion
            Nothing
            $(verQ "next")
      )
    , ("install ghc latest", ToolTag Latest)
    , ("install ghc nightly", GHCVersion
          $ GHCTargetVersion
            Nothing
            $(verQ "nightly")
      )
    , ("install ghc recommended", ToolTag Recommended)
    , ("install ghc prerelease", GHCVersion
          $ GHCTargetVersion
            Nothing
            $(verQ "prerelease")
      )
    , ("install ghc latest-prerelease", ToolTag LatestPrerelease)
    , ("install ghc latest-nightly", ToolTag LatestNightly)
    , ("install ghc javascript-unknown-ghcjs-9.6", GHCVersion
          $ GHCTargetVersion
            (Just "javascript-unknown-ghcjs")
            $(verQ "9.6")
      )
    , ("install ghc base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install ghc ghc-9.2", GHCVersion
          $ GHCTargetVersion
            (Just "ghc")
            $(verQ "9.2")
      )
    ]

installCabalCheckList :: [(String, Either InstallCommand InstallOptions)]
installCabalCheckList =
  ("install cabal", Left $ InstallCabal defaultOptions{instSet = True})
  : mapSecond (Left . InstallCabal . mkInstallOptions')
    [ ("install cabal 3.10", ToolVersion $(verQ "3.10"))
    , ("install cabal next", ToolVersion $(verQ "next"))
    , ("install cabal latest", ToolTag Latest)
    , ("install cabal nightly", ToolVersion $(verQ "nightly"))
    , ("install cabal recommended", ToolTag Recommended)
    , ("install cabal prerelease", ToolVersion $(verQ "prerelease"))
    , ("install cabal latest-prerelease", ToolTag LatestPrerelease)
    , ("install cabal latest-nightly", ToolTag LatestNightly)
    , ("install cabal base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install cabal cabal-3.10", ToolVersion $(verQ "cabal-3.10"))
    ]

installHlsCheckList :: [(String, Either InstallCommand InstallOptions)]
installHlsCheckList =
  ("install hls", Left $ InstallHLS defaultOptions{instSet = True})
  : mapSecond (Left . InstallHLS . mkInstallOptions')
    [ ("install hls 3.10", ToolVersion $(verQ "3.10"))
    , ("install hls next", ToolVersion $(verQ "next"))
    , ("install hls latest", ToolTag Latest)
    , ("install hls nightly", ToolVersion $(verQ "nightly"))
    , ("install hls recommended", ToolTag Recommended)
    , ("install hls prerelease", ToolVersion $(verQ "prerelease"))
    , ("install hls latest-prerelease", ToolTag LatestPrerelease)
    , ("install hls latest-nightly", ToolTag LatestNightly)
    , ("install hls base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install hls hls-2.0", ToolVersion $(verQ "hls-2.0"))
    ]

installStackCheckList :: [(String, Either InstallCommand InstallOptions)]
installStackCheckList =
  ("install stack", Left $ InstallStack defaultOptions{instSet = True})
  : mapSecond (Left . InstallStack . mkInstallOptions')
    [ ("install stack 3.10", ToolVersion $(verQ "3.10"))
    , ("install stack next", ToolVersion $(verQ "next"))
    , ("install stack latest", ToolTag Latest)
    , ("install stack nightly", ToolVersion $(verQ "nightly"))
    , ("install stack recommended", ToolTag Recommended)
    , ("install stack prerelease", ToolVersion $(verQ "prerelease"))
    , ("install stack latest-prerelease", ToolTag LatestPrerelease)
    , ("install stack latest-nightly", ToolTag LatestNightly)
    , ("install stack base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install stack stack-2.9", ToolVersion $(verQ "stack-2.9"))
    ]

installParseWith :: [String] -> IO (Either InstallCommand InstallOptions)
installParseWith args = do
  Install a <- parseWith args
  pure a
