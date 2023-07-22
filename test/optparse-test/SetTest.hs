{-# LANGUAGE OverloadedStrings #-}

module SetTest where

import GHCup.OptParse
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
            (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| []])
    )
  , ("set next", SetNext)
  , ("set latest", SetToolTag Latest)
  , ("set nightly", SetGHCVersion
          $ GHCTargetVersion
            Nothing
            (mkVersion $ (Str "nightly" :| []) :| [])
    )
    -- different from `set`
  , ("set recommended", SetToolTag Recommended)
  , ("set prerelease", SetGHCVersion
          $ GHCTargetVersion
            Nothing
            (mkVersion $ (Str "prerelease" :| []) :| [])
    )
  , ("set latest-prerelease", SetToolTag LatestPrerelease)
  , ("set latest-nightly", SetToolTag LatestNightly)
  , ("set ghc-javascript-unknown-ghcjs-9.6", SetGHCVersion
          $ GHCTargetVersion
            (Just "ghc-javascript-unknown-ghcjs")
            (mkVersion $ (Digits 9 :| []) :| [Digits 6 :| []])
    )
  , ("set base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set cabal-3.10", SetGHCVersion
          $ GHCTargetVersion
            (Just "cabal")
            (mkVersion $ (Digits 3 :| []) :| [Digits 10 :| []])
    )
  , ("set hls-2.0.0.0", SetGHCVersion
        $ GHCTargetVersion
            (Just "hls")
            (mkVersion $ (Digits 2 :| []) :| [Digits 0 :| [], Digits 0 :| [], Digits 0 :| []])
    )
  , ("set stack-2.9.3", SetGHCVersion
        $ GHCTargetVersion
            (Just "stack")
            (mkVersion $ (Digits 2 :| []) :| [Digits 9 :| [], Digits 3 :| []])
    )
  ]

setGhcCheckList :: [(String, Either SetCommand SetOptions)]
setGhcCheckList = mapSecond (Left . SetGHC . SetOptions)
  [ ("set ghc", SetRecommended)
  , ("set ghc 9.2", SetGHCVersion
        $ GHCTargetVersion
          Nothing
          (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| []])
    )
  , ("set ghc next", SetNext)
  , ("set ghc latest", SetToolTag Latest)
  , ("set ghc nightly", SetGHCVersion
        $ GHCTargetVersion
          Nothing
          (mkVersion $ (Str "nightly" :| []) :| [])
    )
  , ("set ghc recommended", SetToolTag Recommended)
  , ("set ghc prerelease", SetGHCVersion
        $ GHCTargetVersion
          Nothing
          (mkVersion $ (Str "prerelease" :| []) :| [])
    )
  , ("set ghc latest-prerelease", SetToolTag LatestPrerelease)
  , ("set ghc latest-nightly", SetToolTag LatestNightly)
  , ("set ghc javascript-unknown-ghcjs-9.6", SetGHCVersion
        $ GHCTargetVersion
          (Just "javascript-unknown-ghcjs")
          (mkVersion $ (Digits 9 :| []) :| [Digits 6 :| []])
    )
  , ("set ghc base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set ghc ghc-9.2", SetGHCVersion
        $ GHCTargetVersion
          (Just "ghc")
          (mkVersion $ (Digits 9 :| []) :| [Digits 2 :| []])
    )
  ]

setCabalCheckList :: [(String, Either SetCommand SetOptions)]
setCabalCheckList = mapSecond (Left . SetCabal . SetOptions)
  [ ("set cabal", SetRecommended)
  , ("set cabal 3.10", SetToolVersion $ mkVersion $ (Digits 3 :| []) :| [Digits 10 :| []])
  , ("set cabal next", SetNext)
  , ("set cabal latest", SetToolTag Latest)
  , ("set cabal nightly", SetToolVersion $ mkVersion $ (Str "nightly" :| []) :| [])
  , ("set cabal recommended", SetToolTag Recommended)
  , ("set cabal prerelease", SetToolVersion $ mkVersion $ (Str "prerelease" :| []) :| [])
  , ("set cabal latest-prerelease", SetToolTag LatestPrerelease)
  , ("set cabal latest-nightly", SetToolTag LatestNightly)
  , ("set cabal base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set cabal cabal-3.10", SetToolVersion
      $ Version
        { _vEpoch = Nothing
        , _vChunks = (Str "cabal" :| []) :| []
        , _vRel = [Digits 3 :| [], Digits 10 :| []]
        , _vMeta = Nothing
        }
    )
  ]

setHlsCheckList :: [(String, Either SetCommand SetOptions)]
setHlsCheckList = mapSecond (Left . SetHLS . SetOptions)
  [ ("set hls", SetRecommended)
  , ("set hls 2.0", SetToolVersion $ mkVersion $ (Digits 2 :| []) :| [Digits 0 :| []])
  , ("set hls next", SetNext)
  , ("set hls latest", SetToolTag Latest)
  , ("set hls nightly", SetToolVersion $ mkVersion $ (Str "nightly" :| []) :| [])
  , ("set hls recommended", SetToolTag Recommended)
  , ("set hls prerelease", SetToolVersion $ mkVersion $ (Str "prerelease" :| []) :| [])
  , ("set hls latest-prerelease", SetToolTag LatestPrerelease)
  , ("set hls latest-nightly", SetToolTag LatestNightly)
  , ("set hls base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set hls hls-2.0", SetToolVersion
      $ Version
        { _vEpoch = Nothing
        , _vChunks = (Str "hls" :| []) :| []
        , _vRel = [Digits 2 :| [], Digits 0 :| []]
        , _vMeta = Nothing
        }
    )
  ]

setStackCheckList :: [(String, Either SetCommand SetOptions)]
setStackCheckList = mapSecond (Left . SetStack . SetOptions)
  [ ("set stack", SetRecommended)
  , ("set stack 2.9", SetToolVersion $ mkVersion $ (Digits 2 :| []) :| [Digits 9 :| []])
  , ("set stack next", SetNext)
  , ("set stack latest", SetToolTag Latest)
  , ("set stack nightly", SetToolVersion $ mkVersion $ (Str "nightly" :| []) :| [])
  , ("set stack recommended", SetToolTag Recommended)
  , ("set stack prerelease", SetToolVersion $ mkVersion $ (Str "prerelease" :| []) :| [])
  , ("set stack latest-prerelease", SetToolTag LatestPrerelease)
  , ("set stack latest-nightly", SetToolTag LatestNightly)
  , ("set stack base-4.18", SetToolTag (Base (PVP {_pComponents = 4 :| [18]})))
  , ("set stack stack-2.9", SetToolVersion
      $ Version
        { _vEpoch = Nothing
        , _vChunks = (Str "stack" :| []) :| []
        , _vRel = [Digits 2 :| [], Digits 9 :| []]
        , _vMeta = Nothing
        }
    )
  ]

setParseWith :: [String] -> IO (Either SetCommand SetOptions)
setParseWith args = do
  Set a <- parseWith args
  pure a
