{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate where

import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils


import           Codec.Archive
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Resource   ( runResourceT
                                                , MonadUnliftIO
                                                )
import qualified Data.Aeson.Encode.Pretty     as Aeson
import           Data.ByteString                ( ByteString )
import           Data.Either
import           Data.Maybe
import           Data.List
import           Data.Map.Strict                ( Map )
import           Data.Versions
import           Haskus.Utils.Variant.Excepts
import           System.Exit
import           Text.Regex.Posix
import           GHCup.Utils.String.QQ

import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Yaml.Pretty              as YAML
import qualified Text.Megaparsec               as MP

data Format = FormatJSON
            | FormatYAML

data Output
  = FileOutput FilePath -- optsparse-applicative doesn't handle ByteString correctly anyway
  | StdOut

type HlsGhcVersions = Map Version (Map Architecture (Map Platform Version))

generate :: ( MonadFail m
            , MonadMask m
            , Monad m
            , MonadReader env m
            , HasSettings env
            , HasDirs env
            , HasLog env
            , MonadThrow m
            , MonadIO m
            , MonadUnliftIO m
            )
         => GHCupDownloads
         -> M.Map GlobalTool DownloadInfo
         -> Format
         -> Output
         -> m ExitCode
generate dls _ format output = do
  let hlses = dls M.! HLS
  r <- forM hlses $ \(_viArch -> archs) ->
         forM archs $ \plats ->
           forM plats $ \(head . M.toList -> (_, dli)) -> do
             VRight r <- runResourceT . runE
                    @'[DigestError
                      , GPGError
                      , DownloadFailed
                      , UnknownArchive
                      , ArchiveResult
                      ] $ do
               fp <- liftE $ downloadCached dli Nothing
               files <- liftE $ getArchiveFiles fp
               let regex = makeRegexOpts compExtended execBlank ([s|^haskell-language-server-([0-9]+\.)*([0-9]+)$|] :: ByteString)
               let ghcs = rights $ MP.parse version' ""
                                 . T.pack
                                 . fromJust
                                 . stripPrefix "haskell-language-server-"
                                <$> filter (match regex) files
               pure ghcs
             pure r
  let w = case format of
            FormatYAML -> BSL.fromStrict $ YAML.encodePretty YAML.defConfig r
            FormatJSON -> Aeson.encodePretty r
  case output of
    StdOut -> liftIO $ BSL.putStr w
    FileOutput f -> liftIO $ BSL.writeFile f w
  pure ExitSuccess
