{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
    : ("install -i /", Right defaultOptions{Install.isolateDir = Just "/"})
    : ("install -u https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz head"
    , Right defaultOptions
        { instBindist = Just [uri|https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz|]
        , instVer = Just $ GHCVersion $ GHCTargetVersion Nothing (mkVersion $ (Str "head" :| []) :| [])
        }
    )
    : mapSecond
        (Right . mkInstallOptions)
        [ ("install ghc-9.2", GHCVersion
              $ GHCTargetVersion
                (Just "ghc")
                (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| []])
          )
          -- invalid
        , ("install next", GHCVersion
              $ GHCTargetVersion
                Nothing
                (mkVersion $ (Str "next" :| []) :| [])
          )
        , ("install latest", ToolTag Latest)
        , ("install nightly", GHCVersion
              $ GHCTargetVersion
                Nothing
                (mkVersion $ (Str "nightly" :| []) :| [])
          )
        , ("install recommended", ToolTag Recommended)
        , ("install prerelease", GHCVersion
              $ GHCTargetVersion
                Nothing
                (mkVersion $ (Str "prerelease" :| []) :| [])
          )
        , ("install latest-prerelease", ToolTag LatestPrerelease)
        , ("install latest-nightly", ToolTag LatestNightly)
        , ("install ghc-javascript-unknown-ghcjs-9.6", GHCVersion
              $ GHCTargetVersion
                (Just "ghc-javascript-unknown-ghcjs")
                (mkVersion $ (Digits 9 :| []) :| [Digits 6 :| []])
          )
        , ("install base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
        , ("install cabal-3.10", GHCVersion
              $ GHCTargetVersion
                (Just "cabal")
                (mkVersion $ (Digits 3 :| []) :| [Digits 10 :| []])
          )
        , ("install hls-2.0.0.0", GHCVersion
              $ GHCTargetVersion
                (Just "hls")
                (mkVersion $ (Digits 2 :| []) :| [Digits 0 :| [], Digits 0 :| [], Digits 0 :| []])
          )
        , ("install stack-2.9.3", GHCVersion
              $ GHCTargetVersion
                (Just "stack")
                (mkVersion $ (Digits 2 :| []) :| [Digits 9 :| [], Digits 3 :| []])
          )
        ]

installGhcCheckList :: [(String, Either InstallCommand InstallOptions)]
installGhcCheckList =
  ("install ghc", Left $ InstallGHC defaultOptions)
  : mapSecond (Left . InstallGHC . mkInstallOptions)
    [ ("install ghc 9.2", GHCVersion
          $ GHCTargetVersion
            Nothing
            (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| []])
      )
    , ("install ghc next", GHCVersion
          $ GHCTargetVersion
            Nothing
            (mkVersion $ (Str "next" :| []) :| [])
      )
    , ("install ghc latest", ToolTag Latest)
    , ("install ghc nightly", GHCVersion
          $ GHCTargetVersion
            Nothing
            (mkVersion $ (Str "nightly" :| []) :| [])
      )
    , ("install ghc recommended", ToolTag Recommended)
    , ("install ghc prerelease", GHCVersion
          $ GHCTargetVersion
            Nothing
            (mkVersion $ (Str "prerelease" :| []) :| [])
      )
    , ("install ghc latest-prerelease", ToolTag LatestPrerelease)
    , ("install ghc latest-nightly", ToolTag LatestNightly)
    , ("install ghc javascript-unknown-ghcjs-9.6", GHCVersion
          $ GHCTargetVersion
            (Just "javascript-unknown-ghcjs")
            (mkVersion $ (Digits 9 :| []) :| [Digits 6 :| []])
      )
    , ("install ghc base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install ghc ghc-9.2", GHCVersion
          $ GHCTargetVersion
            (Just "ghc")
            (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| []])
      )
    ]

installCabalCheckList :: [(String, Either InstallCommand InstallOptions)]
installCabalCheckList =
  ("install cabal", Left $ InstallCabal defaultOptions{instSet = True})
  : mapSecond (Left . InstallCabal . mkInstallOptions')
    [ ("install cabal 3.10", ToolVersion $ mkVersion $ (Digits 3 :| []) :| [Digits 10 :| []])
    , ("install cabal next", ToolVersion $ mkVersion $ (Str "next" :| []) :| [])
    , ("install cabal latest", ToolTag Latest)
    , ("install cabal nightly", ToolVersion $ mkVersion $ (Str "nightly" :| []) :| [])
    , ("install cabal recommended", ToolTag Recommended)
    , ("install cabal prerelease", ToolVersion $ mkVersion $ (Str "prerelease" :| []) :| [])
    , ("install cabal latest-prerelease", ToolTag LatestPrerelease)
    , ("install cabal latest-nightly", ToolTag LatestNightly)
    , ("install cabal base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install cabal cabal-3.10", ToolVersion
          $ Version
            { _vEpoch = Nothing
            , _vChunks = (Str "cabal" :| []) :| []
            , _vRel = [Digits 3 :| [], Digits 10 :| []]
            , _vMeta = Nothing
            }
      )
    ]

installHlsCheckList :: [(String, Either InstallCommand InstallOptions)]
installHlsCheckList =
  ("install hls", Left $ InstallHLS defaultOptions{instSet = True})
  : mapSecond (Left . InstallHLS . mkInstallOptions')
    [ ("install hls 3.10", ToolVersion $ mkVersion $ (Digits 3 :| []) :| [Digits 10 :| []])
    , ("install hls next", ToolVersion $ mkVersion $ (Str "next" :| []) :| [])
    , ("install hls latest", ToolTag Latest)
    , ("install hls nightly", ToolVersion $ mkVersion $ (Str "nightly" :| []) :| [])
    , ("install hls recommended", ToolTag Recommended)
    , ("install hls prerelease", ToolVersion $ mkVersion $ (Str "prerelease" :| []) :| [])
    , ("install hls latest-prerelease", ToolTag LatestPrerelease)
    , ("install hls latest-nightly", ToolTag LatestNightly)
    , ("install hls base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install hls hls-2.0", ToolVersion
          $ Version
            { _vEpoch = Nothing
            , _vChunks = (Str "hls" :| []) :| []
            , _vRel = [Digits 2 :| [], Digits 0 :| []]
            , _vMeta = Nothing
            }
      )
    ]

installStackCheckList :: [(String, Either InstallCommand InstallOptions)]
installStackCheckList =
  ("install stack", Left $ InstallStack defaultOptions{instSet = True})
  : mapSecond (Left . InstallStack . mkInstallOptions')
    [ ("install stack 3.10", ToolVersion $ mkVersion $ (Digits 3 :| []) :| [Digits 10 :| []])
    , ("install stack next", ToolVersion $ mkVersion $ (Str "next" :| []) :| [])
    , ("install stack latest", ToolTag Latest)
    , ("install stack nightly", ToolVersion $ mkVersion $ (Str "nightly" :| []) :| [])
    , ("install stack recommended", ToolTag Recommended)
    , ("install stack prerelease", ToolVersion $ mkVersion $ (Str "prerelease" :| []) :| [])
    , ("install stack latest-prerelease", ToolTag LatestPrerelease)
    , ("install stack latest-nightly", ToolTag LatestNightly)
    , ("install stack base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install stack stack-2.9", ToolVersion
          $ Version
            { _vEpoch = Nothing
            , _vChunks = (Str "stack" :| []) :| []
            , _vRel = [Digits 2 :| [], Digits 9 :| []]
            , _vMeta = Nothing
            }
      )
    ]

installParseWith :: [String] -> IO (Either InstallCommand InstallOptions)
installParseWith args = do
  Install a <- parseWith args
  pure a
