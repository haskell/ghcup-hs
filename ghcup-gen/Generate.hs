{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils
import           GHCup.Utils.Logger
import           GHCup.Utils.Version.QQ

import           Codec.Archive
import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Control.Monad.Trans.Resource   ( runResourceT
                                                , MonadUnliftIO
                                                )
import           Data.Containers.ListUtils      ( nubOrd )
import           Data.ByteString                ( ByteString )
import           Data.IORef
import           Data.Either
import           Data.Maybe
import           Data.List
import           Data.Map.Strict                ( Map )
import           Data.Versions
import           Haskus.Utils.Variant.Excepts
import           Optics
import           System.FilePath
import           System.Exit
import           Text.ParserCombinators.ReadP
import           Text.PrettyPrint.HughesPJClass ( prettyShow )
import           Text.Regex.Posix
import           GHCup.Utils.String.QQ

import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Version                  as V
import qualified Data.Yaml.Pretty              as YAML
import qualified Text.Megaparsec               as MP



data GhcHlsVersions = GhcHlsVersions {

}

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
         -> m ExitCode
generate dls _ = do
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
  liftIO $ BS.putStr $ YAML.encodePretty YAML.defConfig r
  pure ExitSuccess
