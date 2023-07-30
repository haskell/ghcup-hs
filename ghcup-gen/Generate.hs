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
import           GHCup.Requirements
import           GHCup.Errors
import           GHCup.Types
import           GHCup.Types.Optics
import           GHCup.Utils


import           Codec.Archive
import           Control.DeepSeq
import           Control.Exception              ( evaluate )
import           Control.Exception.Safe      hiding ( handle )
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
import           System.FilePath
import           System.IO
import           Text.Regex.Posix
import           GHCup.Prelude.String.QQ

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

generateHLSGhc :: ( MonadFail m
                  , MonadMask m
                  , Monad m
                  , MonadReader env m
                  , HasSettings env
                  , HasDirs env
                  , HasLog env
                  , MonadThrow m
                  , MonadIO m
                  , MonadUnliftIO m
                  , HasGHCupInfo env
                  )
               => Format
               -> Output
               -> m ExitCode
generateHLSGhc format output = do
  GHCupInfo { _ghcupDownloads = dls } <- getGHCupInfo
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
                      , ContentLengthError
                      ] $ do
               fp <- liftE $ downloadCached dli Nothing
               let subd = _dlSubdir dli
               filesL <- liftE $ getArchiveFiles fp
               files <- liftIO $ evaluate $ force filesL
               case subd of
                         Just (RealDir d)
                           | d </> "GNUmakefile" `elem` files
                           -> do let regex = makeRegexOpts compExtended execBlank ([s|^haskell-language-server-([0-9]+\.)*([0-9]+)(\.in)$|] :: ByteString)
                                 pure (rights $ MP.parse version' ""
                                      . T.pack
                                      . fromJust
                                      . stripPrefix "haskell-language-server-"
                                      . stripIn
                                     <$> filter (match regex) (fromJust . stripPrefix (d <> "/") <$> files)
                                      )
                         _ -> do let regex = makeRegexOpts compExtended execBlank ([s|^haskell-language-server-([0-9]+\.)*([0-9]+)(\.exe)?$|] :: ByteString)
                                 pure (rights $ MP.parse version' ""
                                      . T.pack
                                      . fromJust
                                      . stripPrefix "haskell-language-server-"
                                      . stripExe
                                     <$> filter (match regex) files
                                      )
             pure (sort r)
  let w = case format of
            FormatYAML -> BSL.fromStrict $ YAML.encodePretty YAML.defConfig r
            FormatJSON -> Aeson.encodePretty r
  case output of
    StdOut -> liftIO $ BSL.putStr w
    FileOutput f -> liftIO $ BSL.writeFile f w
  pure ExitSuccess
 where
  stripExe :: String -> String
  stripExe f = case reverse f of
                 ('e':'x':'e':'.':r) -> reverse r
                 _ -> f
  stripIn :: String -> String
  stripIn f = case reverse f of
                 ('n':'i':'.':r) -> reverse r
                 _ -> f

generateTable :: ( MonadFail m
                 , MonadMask m
                 , Monad m
                 , MonadReader env m
                 , HasSettings env
                 , HasDirs env
                 , HasLog env
                 , MonadThrow m
                 , MonadIO m
                 , HasPlatformReq env
                 , HasGHCupInfo env
                 , MonadUnliftIO m
                 )
              => Output
              -> m ExitCode
generateTable output = do
  handle <- case output of
              StdOut -> pure stdout
              FileOutput fp -> liftIO $ openFile fp WriteMode

  forM_ [GHC,Cabal,HLS,Stack] $ \tool -> do
    case tool of
      GHC -> liftIO $ hPutStrLn handle $ "<details> <summary>Show all supported <a href='https://www.haskell.org/ghc/'>GHC</a> versions</summary>"
      Cabal -> liftIO $ hPutStrLn handle $ "<details> <summary>Show all supported <a href='https://cabal.readthedocs.io/en/stable/'>cabal-install</a> versions</summary>"
      HLS -> liftIO $ hPutStrLn handle $ "<details> <summary>Show all supported <a href='https://haskell-language-server.readthedocs.io/en/stable/'>HLS</a> versions</summary>"
      Stack -> liftIO $ hPutStrLn handle $ "<details> <summary>Show all supported <a href='https://docs.haskellstack.org/en/stable/README/'>Stack</a> versions</summary>"
      _ -> fail "no"
    liftIO $ hPutStrLn handle $ "<table>"
    liftIO $ hPutStrLn handle $ "<thead><tr><th>" <> show tool <> " Version</th><th>Tags</th></tr></thead>"
    liftIO $ hPutStrLn handle $ "<tbody>"
    vers <- reverse <$> listVersions (Just tool) [] False False (Nothing, Nothing)
    forM_ (filter (\ListResult{..} -> not lStray) vers) $ \ListResult{..} -> do
      liftIO $ hPutStrLn handle $
          "<tr><td>"
        <> T.unpack (prettyVer lVer)
        <> "</td><td>"
        <> intercalate ", " (filter (/= "") . fmap printTag $ sort lTag)
        <> "</td></tr>"
      pure ()
    liftIO $ hPutStrLn handle $ "</tbody>"
    liftIO $ hPutStrLn handle $ "</table>"
    liftIO $ hPutStrLn handle $ "</details>"
    liftIO $ hPutStrLn handle $ ""
  pure ExitSuccess
 where
  printTag Recommended        = "<span style=\"color:green\">recommended</span>"
  printTag Latest             = "<span style=\"color:blue\">latest</span>"
  printTag Prerelease         = "<span style=\"color:red\">prerelease</span>"
  printTag (Base       pvp'') = "base-" ++ T.unpack (prettyPVP pvp'')
  printTag (UnknownTag t    ) = t
  printTag Old                = ""


generateSystemInfo :: ( MonadFail m
                      , MonadMask m
                      , Monad m
                      , MonadReader env m
                      , HasSettings env
                      , HasDirs env
                      , HasLog env
                      , MonadThrow m
                      , MonadIO m
                      , HasPlatformReq env
                      , HasGHCupInfo env
                      , MonadUnliftIO m
                      )
                   => Output
                   -> m ExitCode
generateSystemInfo output = do
  handle <- case output of
              StdOut -> pure stdout
              FileOutput fp -> liftIO $ openFile fp WriteMode

  forM_ [ Linux Debian
        , Linux Ubuntu
        , Linux Fedora
        , Linux CentOS
        , Linux Alpine
        , Linux UnknownLinux
        , Darwin
        , FreeBSD
        , Windows
        ] $ \plat -> do
            GHCupInfo { .. } <- getGHCupInfo
            (Just req) <- pure $ getCommonRequirements (PlatformResult plat Nothing) _toolRequirements
            liftIO $ hPutStrLn handle $ "### " <> (prettyPlat plat) <> "\n"
            liftIO $ hPutStrLn handle $ (T.unpack $ pretty' req) <> "\n"
  pure ExitSuccess
 where
  pretty' Requirements {..} =
    let d = if not . null $ _distroPKGs
          then "The following distro packages are required: " <> "`" <> T.intercalate " " _distroPKGs <> "`"
          else ""
        n = if not . T.null $ _notes then _notes else ""
    in  if | T.null d -> n
           | T.null n -> d
           | otherwise -> d <> "\n" <> n

  prettyPlat (Linux UnknownLinux) = "Linux (generic)"
  prettyPlat p = show p

