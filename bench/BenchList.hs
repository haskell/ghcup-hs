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

import Control.Monad.Reader (runReaderT)
import Control.Exception (displayException)
import Data.Maybe
import Data.Yaml (decodeFileEither)
import Data.Variant.Excepts
import Prelude hiding ( appendFile )
import Test.Tasty.Bench


benchMark :: Benchmark
benchMark =
  bgroup "GHCup.Commands.List"
     [ env (readGHCupInfo "data/bench/ghcup-cross-0.1.0.yaml")           (bench "cross"   . nfAppIO listVersionsB)
     , env (readGHCupInfo "data/bench/ghcup-0.1.0.yaml")                 (bench "def"     . nfAppIO listVersionsB)
     , env (readGHCupInfo "data/bench/ghcup-nightlies-2025-0.0.7.yaml")  (bench "nightly" . nfAppIO listVersionsB)
     ]


readGHCupInfo :: FilePath -> IO [NewURLSource]
readGHCupInfo fp = either (fail . displayException) (pure . (:[]) . NewGHCupInfo) =<< decodeFileEither fp

type DownloadErrors = '[ContentLengthError, DigestError, DistroNotFound, DownloadFailed, FileDoesNotExistError, GPGError, JSONError, NoCompatibleArch, NoCompatiblePlatform, NoDownload, GHCup.Errors.ParseError, ProcessError, UnsupportedSetupCombo, StackPlatformDetectError, UnsupportedMetadataFormat]

listVersionsB :: [NewURLSource] -> IO ToolListResult
listVersionsB infos = do
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

  ghcupInfo <-
    ( flip runReaderT leanAppstate . runE @DownloadErrors $ do
       liftE $ getDownloadsF pfreq
    )
      >>= \case
            VRight r -> pure r
            VLeft  e -> fail (show e)
  let s' = AppState settings dirs keybindings ghcupInfo pfreq loggerConfig

  VRight r <- flip runReaderT s' . runE @'[ParseError] $ listVersions Nothing []
    ShowUpdates -- showRevisions
    False       -- hideOld
    True        -- showNightly
    (Nothing, Nothing)
  pure r

