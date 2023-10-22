{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module InstallTest where

import Test.Tasty
import GHCup.OptParse hiding (HLSCompileOptions(isolateDir))
import Utils
import GHCup.Types
import Data.Versions
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHCup.OptParse.Install as Install
import URI.ByteString.QQ
import URI.ByteString
import Data.Text (Text)

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
      , ("cabal", (fmap . fmap . fmap) toGHCOptions installCabalCheckList)
      , ("hls", (fmap . fmap . fmap) toGHCOptions installHlsCheckList)
      , ("stack", (fmap . fmap . fmap) toGHCOptions installStackCheckList)
      ]

toGHCOptions :: InstallOptions -> InstallGHCOptions
toGHCOptions InstallOptions{..}
  = InstallGHCOptions instVer
                      instBindist
                      instSet
                      isolateDir
                      forceInstall
                      addConfArgs
                      Nothing



defaultOptions :: InstallOptions
defaultOptions = InstallOptions Nothing Nothing False Nothing False []

defaultGHCOptions :: InstallGHCOptions
defaultGHCOptions = InstallGHCOptions Nothing Nothing False Nothing False [] Nothing

-- | Don't set as active version
mkInstallOptions :: ToolVersion -> InstallGHCOptions
mkInstallOptions ver = InstallGHCOptions (Just ver) Nothing False Nothing False [] Nothing

-- | Set as active version
mkInstallOptions' :: ToolVersion -> InstallOptions
mkInstallOptions' ver = InstallOptions (Just ver) Nothing True Nothing False []

oldStyleCheckList :: [(String, Either InstallCommand InstallGHCOptions)]
oldStyleCheckList =
      ("install", Right defaultGHCOptions)
    : ("install --set", Right (defaultGHCOptions{instSet = True} :: InstallGHCOptions))
    : ("install --force", Right (defaultGHCOptions{forceInstall = True} :: InstallGHCOptions))
#ifdef IS_WINDOWS
    : ("install -i C:\\\\", Right (defaultGHCOptions{Install.isolateDir = Just "C:\\\\"} :: InstallGHCOptions))
#else
    : ("install -i /", Right (defaultGHCOptions{Install.isolateDir = Just "/"} :: InstallGHCOptions))
#endif
    : ("install -u https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz head"
    , Right (defaultGHCOptions
        { instBindist = Just [uri|https://gitlab.haskell.org/ghc/ghc/-/jobs/artifacts/master/raw/ghc-x86_64-linux-fedora33-release.tar.xz|]
        , instVer = Just $ GHCVersion $ GHCTargetVersion Nothing  $(versionQ "head")
        } :: InstallGHCOptions)
    )
    : mapSecond
        (Right . mkInstallOptions)
        [ ("install ghc-9.2", GHCVersion
              $ GHCTargetVersion
                (Just "ghc")
                $(versionQ "9.2")
          )
          -- invalid
        , ("install next", GHCVersion
              $ GHCTargetVersion
                Nothing
                $(versionQ "next")
          )
        , ("install latest", ToolTag Latest)
        , ("install nightly", GHCVersion
              $ GHCTargetVersion
                Nothing
                $(versionQ "nightly")
          )
        , ("install recommended", ToolTag Recommended)
        , ("install prerelease", GHCVersion
              $ GHCTargetVersion
                Nothing
                $(versionQ "prerelease")
          )
        , ("install latest-prerelease", ToolTag LatestPrerelease)
        , ("install latest-nightly", ToolTag LatestNightly)
        , ("install ghc-javascript-unknown-ghcjs-9.6", GHCVersion
              $ GHCTargetVersion
                (Just "ghc-javascript-unknown-ghcjs")
                $(versionQ "9.6")
          )
        , ("install base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
        , ("install cabal-3.10", GHCVersion
              $ GHCTargetVersion
                (Just "cabal")
                $(versionQ "3.10")
          )
        , ("install hls-2.0.0.0", GHCVersion
              $ GHCTargetVersion
                (Just "hls")
                $(versionQ "2.0.0.0")
          )
        , ("install stack-2.9.3", GHCVersion
              $ GHCTargetVersion
                (Just "stack")
                $(versionQ "2.9.3")
          )
        ]

installGhcCheckList :: [(String, Either InstallCommand InstallGHCOptions)]
installGhcCheckList =
  ("install ghc", Left $ InstallGHC defaultGHCOptions)
  : mapSecond (Left . InstallGHC . mkInstallOptions)
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
    , ("install ghc ghc-9.2", GHCVersion
          $ GHCTargetVersion
            (Just "ghc")
            $(versionQ "9.2")
      )
    ]

installCabalCheckList :: [(String, Either InstallCommand InstallOptions)]
installCabalCheckList =
  ("install cabal", Left $ InstallCabal (defaultOptions{instSet = True} :: InstallOptions))
  : mapSecond (Left . InstallCabal . mkInstallOptions')
    [ ("install cabal 3.10", ToolVersion $(versionQ "3.10"))
    , ("install cabal next", ToolVersion $(versionQ "next"))
    , ("install cabal latest", ToolTag Latest)
    , ("install cabal nightly", ToolVersion $(versionQ "nightly"))
    , ("install cabal recommended", ToolTag Recommended)
    , ("install cabal prerelease", ToolVersion $(versionQ "prerelease"))
    , ("install cabal latest-prerelease", ToolTag LatestPrerelease)
    , ("install cabal latest-nightly", ToolTag LatestNightly)
    , ("install cabal base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install cabal cabal-3.10", ToolVersion $(versionQ "cabal-3.10"))
    ]

installHlsCheckList :: [(String, Either InstallCommand InstallOptions)]
installHlsCheckList =
  ("install hls", Left $ InstallHLS defaultOptions{instSet = True})
  : mapSecond (Left . InstallHLS . mkInstallOptions')
    [ ("install hls 3.10", ToolVersion $(versionQ "3.10"))
    , ("install hls next", ToolVersion $(versionQ "next"))
    , ("install hls latest", ToolTag Latest)
    , ("install hls nightly", ToolVersion $(versionQ "nightly"))
    , ("install hls recommended", ToolTag Recommended)
    , ("install hls prerelease", ToolVersion $(versionQ "prerelease"))
    , ("install hls latest-prerelease", ToolTag LatestPrerelease)
    , ("install hls latest-nightly", ToolTag LatestNightly)
    , ("install hls base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install hls hls-2.0", ToolVersion $(versionQ "hls-2.0"))
    ]

installStackCheckList :: [(String, Either InstallCommand InstallOptions)]
installStackCheckList =
  ("install stack", Left $ InstallStack defaultOptions{instSet = True})
  : mapSecond (Left . InstallStack . mkInstallOptions')
    [ ("install stack 3.10", ToolVersion $(versionQ "3.10"))
    , ("install stack next", ToolVersion $(versionQ "next"))
    , ("install stack latest", ToolTag Latest)
    , ("install stack nightly", ToolVersion $(versionQ "nightly"))
    , ("install stack recommended", ToolTag Recommended)
    , ("install stack prerelease", ToolVersion $(versionQ "prerelease"))
    , ("install stack latest-prerelease", ToolTag LatestPrerelease)
    , ("install stack latest-nightly", ToolTag LatestNightly)
    , ("install stack base-4.18", ToolTag (Base (PVP {_pComponents = 4 :| [18]})))
    , ("install stack stack-2.9", ToolVersion $(versionQ "stack-2.9"))
    ]

installParseWith :: [String] -> IO (Either InstallCommand InstallGHCOptions)
installParseWith args = do
  Install a <- parseWith args
  pure a
