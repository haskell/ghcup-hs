{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SetTest where

import GHCup.OptParse
import GHCup.Utils.Parsers (SetToolVersion(..))
import Test.Tasty
import GHCup.Types
import Data.Versions
import Data.List.NonEmpty (NonEmpty ((:|)))
import Utils

setTests :: TestTree
setTests =
  testGroup "set"
    $ map
        (buildTestTree setParseWith)
        [ ("old-style", oldStyleCheckList)
        , ("ghc", setGhcCheckList)
        , ("cabal", setCabalCheckList)
        , ("hls", setHlsCheckList)
        , ("stack", setStackCheckList)
        ]

oldStyleCheckList :: [(String, Either SetCommand SetOptions)]
oldStyleCheckList = mapSecond (Right . SetOptions)
  [ ("set", SetRecommended)
  , ("set ghc-9.2", SetGHCVersion
          $ GHCTargetVersion
            (Just "ghc")
            $(versionQ "9.2")
    )
  , ("set next", SetNext)
  , ("set latest", SetToolTag Latest)
  , ("set nightly", SetGHCVersion
          $ GHCTargetVersion
            Nothing
            $(versionQ "nightly")
    )
    -- different from `set`
  , ("set recommended", SetToolTag Recommended)
  , ("set prerelease", SetGHCVersion
          $ GHCTargetVersion
            Nothing
            $(versionQ "prerelease")
    )
  , ("set latest-prerelease", SetToolTag LatestPrerelease)
  , ("set latest-nightly", SetToolTag LatestNightly)
  , ("set ghc-javascript-unknown-ghcjs-9.6", SetGHCVersion
          $ GHCTargetVersion
            (Just "ghc-javascript-unknown-ghcjs")
            $(versionQ "9.6")
    )
  , ("set base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set cabal-3.10", SetGHCVersion
          $ GHCTargetVersion
            (Just "cabal")
            $(versionQ "3.10")
    )
  , ("set hls-2.0.0.0", SetGHCVersion
        $ GHCTargetVersion
            (Just "hls")
            $(versionQ "2.0.0.0")
    )
  , ("set stack-2.9.3", SetGHCVersion
        $ GHCTargetVersion
            (Just "stack")
            $(versionQ "2.9.3")
    )
  ]

setGhcCheckList :: [(String, Either SetCommand SetOptions)]
setGhcCheckList = mapSecond (Left . SetGHC . SetOptions)
  [ ("set ghc", SetRecommended)
  , ("set ghc 9.2", SetGHCVersion
        $ GHCTargetVersion
          Nothing
          $(versionQ "9.2")
    )
  , ("set ghc next", SetNext)
  , ("set ghc latest", SetToolTag Latest)
  , ("set ghc nightly", SetGHCVersion
        $ GHCTargetVersion
          Nothing
          $(versionQ "nightly")
    )
  , ("set ghc recommended", SetToolTag Recommended)
  , ("set ghc prerelease", SetGHCVersion
        $ GHCTargetVersion
          Nothing
          $(versionQ "prerelease")
    )
  , ("set ghc latest-prerelease", SetToolTag LatestPrerelease)
  , ("set ghc latest-nightly", SetToolTag LatestNightly)
  , ("set ghc javascript-unknown-ghcjs-9.6", SetGHCVersion
        $ GHCTargetVersion
          (Just "javascript-unknown-ghcjs")
          $(versionQ "9.6")
    )
  , ("set ghc base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set ghc ghc-9.2", SetGHCVersion
        $ GHCTargetVersion
          (Just "ghc")
          $(versionQ "9.2")
    )
  ]

setCabalCheckList :: [(String, Either SetCommand SetOptions)]
setCabalCheckList = mapSecond (Left . SetCabal . SetOptions)
  [ ("set cabal", SetRecommended)
  , ("set cabal 3.10", SetToolVersion $(versionQ "3.10"))
  , ("set cabal next", SetNext)
  , ("set cabal latest", SetToolTag Latest)
  , ("set cabal nightly", SetToolVersion $(versionQ "nightly"))
  , ("set cabal recommended", SetToolTag Recommended)
  , ("set cabal prerelease", SetToolVersion $(versionQ "prerelease"))
  , ("set cabal latest-prerelease", SetToolTag LatestPrerelease)
  , ("set cabal latest-nightly", SetToolTag LatestNightly)
  , ("set cabal base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set cabal cabal-3.10", SetToolVersion $(versionQ "cabal-3.10"))
  ]

setHlsCheckList :: [(String, Either SetCommand SetOptions)]
setHlsCheckList = mapSecond (Left . SetHLS . SetOptions)
  [ ("set hls", SetRecommended)
  , ("set hls 2.0", SetToolVersion $(versionQ "2.0"))
  , ("set hls next", SetNext)
  , ("set hls latest", SetToolTag Latest)
  , ("set hls nightly", SetToolVersion $(versionQ "nightly"))
  , ("set hls recommended", SetToolTag Recommended)
  , ("set hls prerelease", SetToolVersion $(versionQ "prerelease"))
  , ("set hls latest-prerelease", SetToolTag LatestPrerelease)
  , ("set hls latest-nightly", SetToolTag LatestNightly)
  , ("set hls base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set hls hls-2.0", SetToolVersion $(versionQ "hls-2.0"))
  ]

setStackCheckList :: [(String, Either SetCommand SetOptions)]
setStackCheckList = mapSecond (Left . SetStack . SetOptions)
  [ ("set stack", SetRecommended)
  , ("set stack 2.9", SetToolVersion $(versionQ "2.9"))
  , ("set stack next", SetNext)
  , ("set stack latest", SetToolTag Latest)
  , ("set stack nightly", SetToolVersion $(versionQ "nightly"))
  , ("set stack recommended", SetToolTag Recommended)
  , ("set stack prerelease", SetToolVersion $(versionQ "prerelease"))
  , ("set stack latest-prerelease", SetToolTag LatestPrerelease)
  , ("set stack latest-nightly", SetToolTag LatestNightly)
  , ("set stack base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set stack stack-2.9", SetToolVersion $(versionQ "stack-2.9"))
  ]

setParseWith :: [String] -> IO (Either SetCommand SetOptions)
setParseWith args = do
  Set a <- parseWith args
  pure a
