{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Main where

import           GHCup
import           GHCup.Download
import           GHCup.Errors
import           GHCup.Platform
import           GHCup.Requirements
import           GHCup.Types
import           GHCup.Utils
import           GHCup.Utils.Logger
import           GHCup.Utils.Prelude
import           GHCup.Version

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail             ( MonadFail )
#endif
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.List                      ( intercalate )
import           Data.String.Interpolate
import           Data.Text                      ( Text )
import           Data.Versions
import           Data.Void
import           GHC.IO.Encoding
import           Haskus.Utils.Variant.Excepts
import           HPath
import           HPath.IO
import           Options.Applicative     hiding ( style )
import           Prelude                 hiding ( appendFile )
import           System.Console.Pretty
import           System.Environment
import           System.Exit
import           System.IO               hiding ( appendFile )
import           Text.Read
import           Text.Layout.Table
import           URI.ByteString

import qualified Data.ByteString               as B
import qualified Data.ByteString.UTF8          as UTF8
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as E
import qualified Text.Megaparsec               as MP





data Options = Options
  {
  -- global options
    optVerbose   :: Bool
  , optCache     :: Bool
  , optUrlSource :: Maybe URI
  , optNoVerify  :: Bool
  -- commands
  , optCommand   :: Command
  }

data Command
  = Install InstallOptions
  | InstallCabal InstallOptions
  | SetGHC SetGHCOptions
  | List ListOptions
  | Rm RmOptions
  | DInfo
  | Compile CompileCommand
  | Upgrade UpgradeOpts Bool
  | NumericVersion
  | ToolRequirements

data ToolVersion = ToolVersion Version
                 | ToolTag Tag


data InstallOptions = InstallOptions
  { instVer      :: Maybe ToolVersion
  , instPlatform :: Maybe PlatformRequest
  }

data SetGHCOptions = SetGHCOptions
  { ghcVer :: Maybe ToolVersion
  }

data ListOptions = ListOptions
  { lTool     :: Maybe Tool
  , lCriteria :: Maybe ListCriteria
  }

data RmOptions = RmOptions
  { ghcVer :: Version
  }


data CompileCommand = CompileGHC CompileOptions
                    | CompileCabal CompileOptions


data CompileOptions = CompileOptions
  { targetVer    :: Version
  , bootstrapGhc :: Either Version (Path Abs)
  , jobs         :: Maybe Int
  , buildConfig  :: Maybe (Path Abs)
  , patchDir     :: Maybe (Path Abs)
  }

data UpgradeOpts = UpgradeInplace
                 | UpgradeAt (Path Abs)
                 | UpgradeGHCupDir
                 deriving Show


opts :: Parser Options
opts =
  Options
    <$> switch
          (short 'v' <> long "verbose" <> help
            "Enable verbosity"
          )
    <*> switch
          (short 'c' <> long "cache" <> help
            "Cache downloads in ~/.ghcup/cache"
          )
    <*> (optional
          (option
            (eitherReader parseUri)
            (  short 's'
            <> long "url-source"
            <> metavar "URL"
            <> help "Alternative ghcup download info url"
            <> internal
            )
          )
        )
    <*> switch
          (short 'n' <> long "no-verify" <> help
            "Skip tarball checksum verification"
          )
    <*> com
 where
  parseUri s' =
    bimap show id $ parseURI strictURIParserOptions (UTF8.fromString s')


com :: Parser Command
com =
  subparser
      (  command
          "install"
          ((info ((Install <$> installOpts) <**> helper)
                 (progDesc "Install or update GHC")
           )
          )
      <> command
           "set"
           (   SetGHC
           <$> (info (setGHCOpts <**> helper)
                     (progDesc "Set currently active GHC version")
               )
           )
      <> command
           "rm"
           (   Rm
           <$> (info (rmOpts <**> helper) (progDesc "Remove a GHC version"))
           )

      <> command
           "install-cabal"
           ((info ((InstallCabal <$> installOpts) <**> helper)
                  (progDesc "Install or update cabal")
            )
           )
      <> command
           "list"
           (   List
           <$> (info (listOpts <**> helper)
                     (progDesc "Show available GHCs and other tools")
               )
           )
      <> command
           "upgrade"
           (info ((Upgrade <$> upgradeOptsP <*>
            switch
            (short 'f' <> long "force" <> help
              "Force update"
            )
           ) <**> helper) (progDesc "Upgrade ghcup"))
      <> command
           "compile"
           (   Compile
           <$> (info (compileP <**> helper)
                     (progDesc "Compile a tool from source")
               )
           )
      <> commandGroup "Main commands:"
      )
    <|> subparser
          (  command
              "debug-info"
              ((\_ -> DInfo) <$> (info (helper) (progDesc "Show debug info")))
          <> command
               "numeric-version"
               (   (\_ -> NumericVersion)
               <$> (info (helper) (progDesc "Show the numeric version"))
               )
          <> command
               "tool-requirements"
               (   (\_ -> ToolRequirements)
               <$> (info (helper)
                         (progDesc "Show the requirements for ghc/cabal")
                   )
               )
          <> commandGroup "Other commands:"
          <> hidden
          )


installOpts :: Parser InstallOptions
installOpts =
  (flip InstallOptions)
    <$> (optional
          (option
            (eitherReader platformParser)
            (  short 'p'
            <> long "platform"
            <> metavar "PLATFORM"
            <> help
                 "Override for platform (triple matching ghc tarball names), e.g. x86_64-fedora27-linux"
            )
          )
        )
    <*> optional toolVersionArgument


setGHCOpts :: Parser SetGHCOptions
setGHCOpts = SetGHCOptions <$> optional toolVersionArgument

listOpts :: Parser ListOptions
listOpts =
  ListOptions
    <$> optional
          (option
            (eitherReader toolParser)
            (short 't' <> long "tool" <> metavar "<ghc|cabal>" <> help
              "Tool to list versions for. Default is all"
            )
          )
    <*> (optional
          (option
            (eitherReader criteriaParser)
            (  short 'c'
            <> long "show-criteria"
            <> metavar "<installed|set>"
            <> help "Show only installed or set tool versions"
            )
          )
        )

rmOpts :: Parser RmOptions
rmOpts = RmOptions <$> versionArgument


compileP :: Parser CompileCommand
compileP = subparser
  (  command
      "ghc"
      (   CompileGHC
      <$> (info (compileOpts <**> helper) (progDesc "Compile GHC from source")
          )
      )
  <> command
       "cabal"
       (   CompileCabal
       <$> (info (compileOpts <**> helper)
                 (progDesc "Compile Cabal from source")
           )
       )
  )


compileOpts :: Parser CompileOptions
compileOpts =
  CompileOptions
    <$> (option
          (eitherReader
            (bimap (const "Not a valid version") id . version . T.pack)
          )
          (short 'v' <> long "version" <> metavar "VERSION" <> help
            "The tool version to compile"
          )
        )
    <*> (option
          (eitherReader
            (\x ->
              (bimap (const "Not a valid version") Left . version . T.pack $ x)
                <|> (bimap show Right . parseAbs . E.encodeUtf8 . T.pack $ x)
            )
          )
          (  short 'b'
          <> long "bootstrap-ghc"
          <> metavar "BOOTSTRAP_GHC"
          <> help
               "The GHC version (or full path) to bootstrap with (must be installed)"
          )
        )
    <*> optional
          (option
            (eitherReader (readEither @Int))
            (short 'j' <> long "jobs" <> metavar "JOBS" <> help
              "How many jobs to use for make"
            )
          )
    <*> optional
          (option
            (eitherReader
              (\x ->
                bimap show id . parseAbs . E.encodeUtf8 . T.pack $ x :: Either
                    String
                    (Path Abs)
              )
            )
            (short 'c' <> long "config" <> metavar "CONFIG" <> help
              "Absolute path to build config file"
            )
          )
    <*> optional
          (option
            (eitherReader
              (\x ->
                bimap show id . parseAbs . E.encodeUtf8 . T.pack $ x :: Either
                    String
                    (Path Abs)
              )
            )
            (short 'p' <> long "patchdir" <> metavar "PATCH_DIR" <> help
              "Absolute path to patch directory (applied in order, uses -p1)"
            )
          )


toolVersionParser :: Parser ToolVersion
toolVersionParser = verP <|> toolP
 where
  verP = ToolVersion <$> versionParser
  toolP =
    ToolTag
      <$> (option
            (eitherReader tagEither)
            (short 't' <> long "tag" <> metavar "TAG" <> help "The target tag")
          )

-- | same as toolVersionParser, except as an argument.
toolVersionArgument :: Parser ToolVersion
toolVersionArgument =
  argument (eitherReader toolVersionEither) (metavar "VERSION|TAG")


versionArgument :: Parser Version
versionArgument = argument
  (eitherReader versionEither)
  (metavar "VERSION")

versionParser :: Parser Version
versionParser = option
  (eitherReader versionEither)
  (short 'v' <> long "version" <> metavar "VERSION" <> help "The target version"
  )

tagEither :: String -> Either String Tag
tagEither s' = case fmap toLower s' of
  "recommended" -> Right Recommended
  "latest"      -> Right Latest
  other         -> Left ([i|Unknown tag #{other}|])

versionEither :: String -> Either String Version
versionEither s' =
  -- 'version' is a bit too lax and will parse typoed tags
  case readMaybe ((:[]) . head $ s') :: Maybe Int of
    Just _  -> bimap (const "Not a valid version") id . version . T.pack $ s'
    Nothing -> Left "Not a valid version"

toolVersionEither :: String -> Either String ToolVersion
toolVersionEither s' =
  bimap id ToolTag (tagEither s') <|> bimap id ToolVersion (versionEither s')


toolParser :: String -> Either String Tool
toolParser s' | t == T.pack "ghc"   = Right GHC
              | t == T.pack "cabal" = Right Cabal
              | otherwise           = Left ("Unknown tool: " <> s')
  where t = T.toLower (T.pack s')


criteriaParser :: String -> Either String ListCriteria
criteriaParser s' | t == T.pack "installed" = Right ListInstalled
                  | t == T.pack "set"       = Right ListSet
                  | otherwise               = Left ("Unknown criteria: " <> s')
  where t = T.toLower (T.pack s')


platformParser :: String -> Either String PlatformRequest
platformParser s' = case MP.parse (platformP <* MP.eof) "" (T.pack s') of
  Right r -> pure r
  Left  e -> Left $ errorBundlePretty e
 where
  archP :: MP.Parsec Void Text Architecture
  archP = (MP.try (MP.chunk "x86_64" $> A_64)) <|> (MP.chunk "i386" $> A_32)
  platformP :: MP.Parsec Void Text PlatformRequest
  platformP = choice'
    [ (\a mv -> PlatformRequest a FreeBSD mv)
    <$> (archP <* MP.chunk "-")
    <*> (  MP.chunk "portbld"
        *> (   MP.try (Just <$> verP (MP.chunk "-freebsd" <* MP.eof))
           <|> pure Nothing
           )
        <* MP.chunk "-freebsd"
        )
    , (\a mv -> PlatformRequest a Darwin mv)
    <$> (archP <* MP.chunk "-")
    <*> (  MP.chunk "apple"
        *> (   MP.try (Just <$> verP (MP.chunk "-darwin" <* MP.eof))
           <|> pure Nothing
           )
        <* MP.chunk "-darwin"
        )
    , (\a d mv -> PlatformRequest a (Linux d) mv)
    <$> (archP <* MP.chunk "-")
    <*> distroP
    <*> ((MP.try (Just <$> verP (MP.chunk "-linux" <* MP.eof)) <|> pure Nothing
         )
        <* MP.chunk "-linux"
        )
    ]
  distroP :: MP.Parsec Void Text LinuxDistro
  distroP = choice'
    [ MP.chunk "debian" $> Debian
    , MP.chunk "deb" $> Debian
    , MP.chunk "ubuntu" $> Ubuntu
    , MP.chunk "mint" $> Mint
    , MP.chunk "fedora" $> Fedora
    , MP.chunk "centos" $> CentOS
    , MP.chunk "redhat" $> RedHat
    , MP.chunk "alpine" $> Alpine
    , MP.chunk "gentoo" $> Gentoo
    , MP.chunk "exherbo" $> Exherbo
    , MP.chunk "unknown" $> UnknownLinux
    ]
  verP :: MP.Parsec Void Text Text -> MP.Parsec Void Text Versioning
  verP suffix = do
    ver <- parseUntil suffix
    if T.null ver
      then fail "empty version"
      else do
        rest <- MP.getInput
        MP.setInput ver
        v <- versioning'
        MP.setInput rest
        pure v

  choice' []       = fail "Empty list"
  choice' [x     ] = x
  choice' (x : xs) = MP.try x <|> choice' xs

  parseUntil :: MP.Parsec Void Text Text -> MP.Parsec Void Text Text
  parseUntil p = do
    (MP.try (MP.lookAhead p) $> mempty)
      <|> (do
            c  <- T.singleton <$> MP.anySingle
            c2 <- parseUntil p
            pure (c `mappend` c2)
          )


toSettings :: Options -> Settings
toSettings Options {..} =
  let cache    = optCache
      noVerify = optNoVerify
  in  Settings { .. }


upgradeOptsP :: Parser UpgradeOpts
upgradeOptsP =
  flag'
      UpgradeInplace
      (short 'i' <> long "inplace" <> help
        "Upgrade ghcup in-place (wherever it's at)"
      )
    <|> (   UpgradeAt
        <$> (option
              (eitherReader
                (\x ->
                  bimap show id . parseAbs . E.encodeUtf8 . T.pack $ x :: Either
                      String
                      (Path Abs)
                )
              )
              (short 't' <> long "target" <> metavar "TARGET_DIR" <> help
                "Absolute filepath to write ghcup into"
              )
            )
        )
    <|> (pure UpgradeGHCupDir)




main :: IO ()
main = do

  customExecParser (prefs showHelpOnError) (info (opts <**> helper) idm)
    >>= \opt@Options {..} -> do
          let settings = toSettings opt

          -- create ~/.ghcup dir
          ghcdir <- ghcupBaseDir
          createDirIfMissing newDirPerms ghcdir

          -- logger interpreter
          logfile <- initGHCupFileLogging [rel|ghcup.log|]
          let runLogger = myLoggerT LoggerConfig
                { lcPrintDebug = optVerbose
                , colorOutter  = B.hPut stderr
                , rawOutter    = appendFile logfile
                }

          -- wrapper to run effects with settings
          let runInstTool =
                runLogger
                  . flip runReaderT settings
                  . runResourceT
                  . runE
                    @'[ AlreadyInstalled
                      , UnknownArchive
                      , DistroNotFound
                      , FileDoesNotExistError
                      , CopyError
                      , NoCompatibleArch
                      , NoDownload
                      , NotInstalled
                      , NoCompatiblePlatform
                      , BuildFailed
                      , TagNotFound
                      , DigestError
                      , DownloadFailed
                      ]

          let
            runSetGHC =
              runLogger
                . flip runReaderT settings
                . runE
                  @'[ FileDoesNotExistError
                    , NotInstalled
                    , TagNotFound
                    , TagNotFound
                    ]

          let runListGHC =
                runLogger
                  . flip runReaderT settings
                  . runE @'[FileDoesNotExistError]

          let runRmGHC =
                runLogger . flip runReaderT settings . runE @'[NotInstalled]

          let runDebugInfo =
                runLogger
                  . flip runReaderT settings
                  . runE
                    @'[NoCompatiblePlatform , NoCompatibleArch , DistroNotFound]

          let runCompileGHC =
                runLogger
                  . flip runReaderT settings
                  . runResourceT
                  . runE
                    @'[ AlreadyInstalled
                      , BuildFailed
                      , DigestError
                      , DistroNotFound
                      , DownloadFailed
                      , GHCupSetError
                      , NoCompatibleArch
                      , NoCompatiblePlatform
                      , NoDownload
                      , NotFoundInPATH
                      , PatchFailed
                      , UnknownArchive
                      ]

          let runCompileCabal =
                runLogger
                  . flip runReaderT settings
                  . runResourceT
                  . runE
                    @'[ BuildFailed
                      , DigestError
                      , DistroNotFound
                      , DownloadFailed
                      , NoCompatibleArch
                      , NoCompatiblePlatform
                      , NoDownload
                      , PatchFailed
                      , UnknownArchive
                      ]

          let runUpgrade =
                runLogger
                  . flip runReaderT settings
                  . runResourceT
                  . runE
                    @'[ DigestError
                      , DistroNotFound
                      , NoCompatiblePlatform
                      , NoCompatibleArch
                      , NoDownload
                      , NoUpdate
                      , FileDoesNotExistError
                      , CopyError
                      , DownloadFailed
                      ]

          (GHCupInfo treq dls) <-
            ( runLogger
              . flip runReaderT settings
              . runE @'[JSONError , DownloadFailed]
              $ liftE
              $ getDownloads (maybe GHCupURL OwnSource optUrlSource)
              )
              >>= \case
                    VRight r -> pure r
                    VLeft e ->
                      runLogger
                          ($(logError) [i|Error fetching download info: #{e}|])
                        >> exitFailure
          runLogger $ checkForUpdates dls

          case optCommand of
            Install (InstallOptions {..}) ->
              void
                $   (runInstTool $ do
                      v <- liftE $ fromVersion dls instVer GHC
                      liftE $ installGHCBin dls v instPlatform
                    )
                >>= \case
                      VRight _ ->
                        runLogger $ $(logInfo) ("GHC installation successful")
                      VLeft (V (AlreadyInstalled _ v)) ->
                        runLogger $ $(logWarn)
                          [i|GHC ver #{prettyVer v} already installed|]
                      VLeft (V (BuildFailed tmpdir e)) ->
                        runLogger
                            ($(logError) [i|Build failed with #{e}
Check the logs at ~/.ghcup/logs and the build directory #{tmpdir} for more clues.|]
                            )
                          >> exitFailure
                      VLeft e -> do
                        runLogger $ do
                          $(logError) [i|#{e}|]
                          $(logError) [i|Also check the logs in ~/.ghcup/logs|]
                        exitFailure
            InstallCabal (InstallOptions {..}) ->
              void
                $   (runInstTool $ do
                      v <- liftE $ fromVersion dls instVer Cabal
                      liftE $ installCabalBin dls v instPlatform
                    )
                >>= \case
                      VRight _ ->
                        runLogger $ $(logInfo) ("Cabal installation successful")
                      VLeft (V (AlreadyInstalled _ v)) ->
                        runLogger $ $(logWarn)
                          [i|Cabal ver #{prettyVer v} already installed|]
                      VLeft e -> do
                        runLogger $ do
                          $(logError) [i|#{e}|]
                          $(logError) [i|Also check the logs in ~/.ghcup/logs|]
                        exitFailure

            SetGHC (SetGHCOptions {..}) ->
              void
                $   (runSetGHC $ do
                      v <- liftE $ fromVersion dls ghcVer GHC
                      liftE $ setGHC v SetGHCOnly
                    )
                >>= \case
                      VRight v ->
                        runLogger $ $(logInfo) [i|GHC #{prettyVer v} successfully set as default version|]
                      VLeft e ->
                        runLogger ($(logError) [i|#{e}|]) >> exitFailure

            List (ListOptions {..}) ->
              void
                $   (runListGHC $ do
                      liftIO $ listVersions dls lTool lCriteria
                    )
                >>= \case
                      VRight r -> liftIO $ printListResult r
                      VLeft e ->
                        runLogger ($(logError) [i|#{e}|]) >> exitFailure

            Rm (RmOptions {..}) ->
              void
                $   (runRmGHC $ do
                      liftE $ rmGHCVer ghcVer
                    )
                >>= \case
                      VRight _ -> pure ()
                      VLeft e ->
                        runLogger ($(logError) [i|#{e}|]) >> exitFailure

            DInfo -> do
              void
                $   (runDebugInfo $ do
                      liftE $ getDebugInfo
                    )
                >>= \case
                      VRight dinfo -> putStrLn $ show dinfo
                      VLeft e ->
                        runLogger ($(logError) [i|#{e}|]) >> exitFailure

            Compile (CompileGHC CompileOptions {..}) ->
              void
                $   (runCompileGHC $ do
                      liftE
                        $ compileGHC dls targetVer bootstrapGhc jobs buildConfig patchDir
                    )
                >>= \case
                      VRight _ ->
                        runLogger $ $(logInfo)
                          ("GHC successfully compiled and installed")
                      VLeft (V (AlreadyInstalled _ v)) ->
                        runLogger $ $(logWarn)
                          [i|GHC ver #{prettyVer v} already installed|]
                      VLeft (V (BuildFailed tmpdir e)) ->
                        runLogger
                            ($(logError) [i|Build failed with #{e}
Check the logs at ~/.ghcup/logs and the build directory #{tmpdir} for more clues.
Make sure to clean up #{tmpdir} afterwards.|]
                            )
                          >> exitFailure
                      VLeft e ->
                        runLogger ($(logError) [i|#{e}|]) >> exitFailure

            Compile (CompileCabal CompileOptions {..}) ->
              void
                $   (runCompileCabal $ do
                      liftE $ compileCabal dls targetVer bootstrapGhc jobs patchDir
                    )
                >>= \case
                      VRight _ ->
                        runLogger $ $(logInfo)
                          ("Cabal successfully compiled and installed")
                      VLeft (V (BuildFailed tmpdir e)) ->
                        runLogger
                            ($(logError) [i|Build failed with #{e}
Check the logs at ~/.ghcup/logs and the build directory #{tmpdir} for more clues.|]
                            )
                          >> exitFailure
                      VLeft e ->
                        runLogger ($(logError) [i|#{e}|]) >> exitFailure

            Upgrade (uOpts) force -> do
              target <- case uOpts of
                UpgradeInplace -> do
                  efp <- liftIO $ getExecutablePath
                  p   <- parseAbs . E.encodeUtf8 . T.pack $ efp
                  pure $ Just p
                (UpgradeAt p)   -> pure $ Just p
                UpgradeGHCupDir -> do
                  bdir <- liftIO $ ghcupBinDir
                  pure (Just (bdir </> [rel|ghcup|]))

              void
                $   (runUpgrade $ do
                      liftE $ upgradeGHCup dls target force
                    )
                >>= \case
                      VRight v' -> do
                        let pretty_v = prettyVer v'
                        runLogger
                          $ $(logInfo)
                              [i|Successfully upgraded GHCup to version #{pretty_v}|]
                      VLeft (V NoUpdate) ->
                        runLogger $ $(logWarn)
                          [i|No GHCup update available|]
                      VLeft e ->
                        runLogger ($(logError) [i|#{e}|]) >> exitFailure

            NumericVersion -> T.hPutStr stdout (prettyPVP ghcUpVer)
            ToolRequirements -> (runLogger $ runE
                      @'[ NoCompatiblePlatform
                        , DistroNotFound
                        , NoToolRequirements
                        ] $ do
                platform <- liftE $ getPlatform
                req <- (getCommonRequirements platform $ treq)
                         ?? NoToolRequirements
                liftIO $ T.hPutStr stdout (prettyRequirements req))
              >>= \case
                    VRight r -> pure r
                    VLeft e ->
                      runLogger
                          ($(logError) [i|Error getting tool requirements: #{e}|])
                        >> exitFailure
  pure ()


fromVersion :: Monad m
            => GHCupDownloads
            -> Maybe ToolVersion
            -> Tool
            -> Excepts '[TagNotFound] m Version
fromVersion av Nothing tool =
  getRecommended av tool ?? TagNotFound Recommended tool
fromVersion _ (Just (ToolVersion v)) _ = pure v
fromVersion av (Just (ToolTag Latest)) tool =
  getLatest av tool ?? TagNotFound Latest tool
fromVersion av (Just (ToolTag Recommended)) tool =
  getRecommended av tool ?? TagNotFound Recommended tool


printListResult :: [ListResult] -> IO ()
printListResult lr = do
  -- https://gitlab.haskell.org/ghc/ghc/issues/8118
  setLocaleEncoding utf8

  let
    formatted =
      gridString
          [ column expand left def def
          , column expand left def def
          , column expand left def def
          , column expand left def def
          , column expand left def def
          ]
        $ fmap
            (\ListResult {..} ->
              [ if
                | lSet       -> (color Green "✔✔")
                | lInstalled -> (color Green "✓")
                | otherwise  -> (color Red "✗")
              , fmap toLower . show $ lTool
              , T.unpack . prettyVer $ lVer
              , intercalate "," $ ((fmap . fmap) toLower . fmap show $ lTag)
              , if fromSrc then (color Blue "compiled") else mempty
              ]
            )
            lr
  putStrLn $ formatted


checkForUpdates :: (MonadFail m, MonadLogger m) => GHCupDownloads -> m ()
checkForUpdates dls = do
  forM_ (getLatest dls GHCup) $ \l -> do
    (Right ghc_ver) <- pure $ version $ prettyPVP ghcUpVer
    when (l > ghc_ver)
      $ $(logWarn)
          [i|New GHCup version available: #{prettyVer l}. To upgrade, run 'ghcup upgrade'|]
