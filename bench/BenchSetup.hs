{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module BenchSetup where

import GHCup.Download
import GHCup.Errors
import GHCup.Prelude
import GHCup.Query.GHCupDirs
import GHCup.Types

import Control.DeepSeq (force)
import Control.Monad (forM)
import Control.Monad.Reader (runReaderT)
import Control.Exception (displayException, evaluate)
import Data.Maybe
import Data.Yaml (decodeFileEither, decodeEither')
import Data.Variant.Excepts
import Data.Versions (Version)
import Prelude hiding ( appendFile )
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHCup.Types.Optics (getPlatformReq, getGHCupInfo)

import qualified Data.Map.Strict as Map
import GHCup.Query.DB
import GHCup.Query.DB.HLS
import Control.Monad.Trans (lift)

type DownloadErrors = '[ContentLengthError, DigestError, DistroNotFound, DownloadFailed, FileDoesNotExistError, GPGError, JSONError, NoCompatibleArch, NoCompatiblePlatform, NoDownload, GHCup.Errors.ParseError, ProcessError, UnsupportedSetupCombo, StackPlatformDetectError, UnsupportedMetadataFormat]

readGHCupInfo :: [FilePath] -> IO [NewURLSource]
readGHCupInfo fps =
  forM fps $ \fp -> do
    ghcupInfo <- either (fail . displayException) pure =<< decodeFileEither fp
    evaluate $ force $ NewGHCupInfo ghcupInfo

parseGHCupInfo' :: ByteString -> IO GHCupInfo
parseGHCupInfo' bs = do
  ghcupInfo <- either (fail . displayException) pure $ decodeEither' bs
  evaluate $ force ghcupInfo

getAppState :: [FilePath] -> IO AppState
getAppState fps = do
  infos <- readGHCupInfo fps
  spawnAppState infos

getListInfoArgs ::
     [FilePath]
  -> IO (GHCupDownloads, PlatformRequest, Map.Map Tool (Map.Map (Maybe Text) ([VersionRev], Maybe VersionRev)), [Version])
getListInfoArgs fps = do
  s' <- getAppState fps
  VRight r <- flip runReaderT s' . runE @'[ParseError] $ do
    GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
    pfreq <- lift getPlatformReq
    instTools <- getAllInstalledTools Nothing
    hlsGHCs <- let hlsSet = do
                     hlsMap <- instTools Map.!? hls
                     (_, mSet) <- hlsMap Map.!? Nothing
                     mSet
               in case hlsSet of
                 Just VersionRev{..} -> do
                   getHLSGHCs _vrVersion
                 Nothing -> pure []
    pure (dls, pfreq, instTools, hlsGHCs)
  pure r

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

  VRight ghcupInfo <- flip runReaderT leanAppstate . runE @DownloadErrors $ do liftE $ getDownloadsF pfreq

  let s' = AppState settings dirs keybindings ghcupInfo pfreq loggerConfig
  evaluate (force s')

