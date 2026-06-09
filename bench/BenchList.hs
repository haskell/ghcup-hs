{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module BenchList (benchMark) where

import BenchSetup

import GHCup.Command.List
import GHCup.Errors
import GHCup.Types

import Control.Monad.Reader (runReaderT)
import Data.Maybe
import Data.Variant.Excepts
import Prelude hiding ( appendFile )
import Test.Tasty.Bench
import Data.Text (Text)
import Data.Versions (Version)

import qualified Data.Map.Strict as Map


benchMark :: Benchmark
benchMark =
  bgroup "GHCup.Commands.List"
    [ bgroup "ST"
       [ env (getListInfoArgs ["data/bench/ghcup-cross-0.1.0.yaml"])           (bench "cross"   . nf listVersionsB)
       , env (getListInfoArgs ["data/bench/ghcup-0.1.0.yaml"])                 (bench "def"     . nf listVersionsB)
       , env (getListInfoArgs ["data/bench/ghcup-nightlies-2025-0.0.7.yaml"])  (bench "nightly" . nf listVersionsB)
       , env (getListInfoArgs ["data/bench/ghcup-nightlies-2025-0.0.7.yaml"
                              ,"data/bench/ghcup-0.1.0.yaml"
                              ,"data/bench/ghcup-cross-0.1.0.yaml"
                              ,"data/bench/ghcup-prereleases-0.1.0.yaml"
                              ,"data/bench/ghcup-3rdparty-0.1.0.yaml"
                              ])                                               (bench "all" . nf listVersionsB)
       ]
    , bgroup "IO"
        [ env (getAppState ["data/bench/ghcup-cross-0.1.0.yaml"])           (bench "cross"   . nfAppIO listVersionsA)
        , env (getAppState ["data/bench/ghcup-0.1.0.yaml"])                 (bench "def"     . nfAppIO listVersionsA)
        , env (getAppState ["data/bench/ghcup-nightlies-2025-0.0.7.yaml"])  (bench "nightly" . nfAppIO listVersionsA)
        , env (getAppState ["data/bench/ghcup-nightlies-2025-0.0.7.yaml"
                           ,"data/bench/ghcup-0.1.0.yaml"
                           ,"data/bench/ghcup-cross-0.1.0.yaml"
                           ,"data/bench/ghcup-prereleases-0.1.0.yaml"
                           ,"data/bench/ghcup-3rdparty-0.1.0.yaml"
                           ])                                               (bench "all" . nfAppIO listVersionsA)
        ]
    ]

listVersionsA :: AppState -> IO ToolListResult
listVersionsA s' = do
  VRight r <- flip runReaderT s' . runE @'[ParseError] $ listVersions
    Nothing     -- tools
    []          -- list criteria
    ShowUpdates -- showRevisions
    False       -- hideOld
    NShowAll    -- showNightly
    (Nothing, Nothing)
  pure r

listVersionsB :: (GHCupDownloads, PlatformRequest, Map.Map Tool (Map.Map (Maybe Text) ([VersionRev], Maybe VersionRev)), [Version]) -> ToolListResult
listVersionsB (dls, pfreq, instTools, hlsGHCs) =
  listVersions' dls pfreq instTools hlsGHCs
    Nothing     -- tools (need to sync with getListInfoArgs)
    []          -- list criteria
    ShowUpdates -- showRevisions
    False       -- hideOld
    NShowAll    -- showNightly
    (Nothing, Nothing)

