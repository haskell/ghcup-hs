{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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


benchMark :: Benchmark
benchMark =
  bgroup "GHCup.Commands.List"
     [ env (getAppState ["data/bench/ghcup-cross-0.1.0.yaml"])           (bench "cross"   . nfAppIO listVersionsB)
     , env (getAppState ["data/bench/ghcup-0.1.0.yaml"])                 (bench "def"     . nfAppIO listVersionsB)
     , env (getAppState ["data/bench/ghcup-nightlies-2025-0.0.7.yaml"])  (bench "nightly" . nfAppIO listVersionsB)
     , env (getAppState ["data/bench/ghcup-nightlies-2025-0.0.7.yaml"
                        ,"data/bench/ghcup-0.1.0.yaml"
                        ,"data/bench/ghcup-cross-0.1.0.yaml"
                        ,"data/bench/ghcup-prereleases-0.1.0.yaml"
                        ,"data/bench/ghcup-3rdparty-0.1.0.yaml"
                        ])                                               (bench "all" . nfAppIO listVersionsB)
     ]

listVersionsB :: AppState -> IO ToolListResult
listVersionsB s' = do
  VRight r <- flip runReaderT s' . runE @'[ParseError] $ listVersions Nothing []
    ShowUpdates -- showRevisions
    False       -- hideOld
    True        -- showNightly
    (Nothing, Nothing)
  pure r

