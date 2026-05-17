{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module BenchList (benchMark) where

import GHCup.Command.List
import GHCup.Download
import GHCup.Errors
import GHCup.Prelude
import GHCup.Query.GHCupDirs
import GHCup.Types

import Control.DeepSeq (force)
import Control.Monad.Reader (runReaderT)
import Control.Exception (displayException, evaluate)
import Data.Maybe
import Data.Yaml (decodeFileEither)
import Data.Variant.Excepts
import Prelude hiding ( appendFile )
import Test.Tasty.Bench


benchMark :: Benchmark
benchMark =
  bgroup "GHCup.Commands.List"
     [ env (getAppState "data/bench/ghcup-cross-0.1.0.yaml")           (bench "cross"   . nfAppIO listVersionsB)
     , env (getAppState "data/bench/ghcup-0.1.0.yaml")                 (bench "def"     . nfAppIO listVersionsB)
     , env (getAppState "data/bench/ghcup-nightlies-2025-0.0.7.yaml")  (bench "nightly" . nfAppIO listVersionsB)
     ]

readGHCupInfo :: FilePath -> IO [NewURLSource]
readGHCupInfo fp = do
  ghcupInfo <- either (fail . displayException) pure =<< decodeFileEither fp
  evaluate $ force [NewGHCupInfo ghcupInfo]

getAppState :: FilePath -> IO AppState
getAppState fp = do
  infos <- readGHCupInfo fp
  spawnAppState infos

spawnAppState :: [NewURLSource] -> IO AppState
spawnAppState infos = do
  dirs <- getAllDirs
  let loggerConfig = LoggerConfig
        { lcPrintDebugLvl = Nothing
        , consoleOutter  = mempty
        , fileOutter    = mempty
        , fancyColors = False
        }
  let pfreq = PlatformRequest A_64 Darwin Nothing
  let settings = defaultSettings { urlSource = infos }
  let keybindings = defaultKeyBindings
  let leanAppstate = LeanAppState settings dirs keybindings pfreq loggerConfig

  VRight ghcupInfo <-
    ( flip runReaderT leanAppstate . runE @DownloadErrors $ do
       liftE $ getDownloadsF pfreq
    )

  let s' = AppState settings dirs keybindings ghcupInfo pfreq loggerConfig
  evaluate (force s')


type DownloadErrors = '[ContentLengthError, DigestError, DistroNotFound, DownloadFailed, FileDoesNotExistError, GPGError, JSONError, NoCompatibleArch, NoCompatiblePlatform, NoDownload, GHCup.Errors.ParseError, ProcessError, UnsupportedSetupCombo, StackPlatformDetectError, UnsupportedMetadataFormat]

listVersionsB :: AppState -> IO ToolListResult
listVersionsB s' = do
  VRight r <- flip runReaderT s' . runE @'[ParseError] $ listVersions Nothing []
    ShowUpdates -- showRevisions
    False       -- hideOld
    True        -- showNightly
    (Nothing, Nothing)
  pure r

