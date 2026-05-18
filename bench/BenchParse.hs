{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module BenchParse (benchMark) where

import BenchSetup

import Prelude hiding ( appendFile )
import Test.Tasty.Bench

import qualified Data.ByteString as BS


benchMark :: Benchmark
benchMark =
  bgroup "Parse metadata"
     [ env (BS.readFile "data/bench/ghcup-cross-0.1.0.yaml")           (bench "cross"   . nfAppIO parseGHCupInfo')
     , env (BS.readFile "data/bench/ghcup-0.1.0.yaml")                 (bench "def"     . nfAppIO parseGHCupInfo')
     , env (BS.readFile "data/bench/ghcup-nightlies-2025-0.0.7.yaml")  (bench "nightly" . nfAppIO parseGHCupInfo')
     ]

